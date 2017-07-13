
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(httr)
library(rvest)
library(tidyverse)
library(magrittr)
library(nufflytics)
library(knitr)

shinyServer(function(input, output, session) {
  rebbl_data <- read_csv("data/rebbl_data.csv")
  
  #logo for top left
  output$logo <- renderImage({
    switch(input$league,
           "REL"  = list(src = "www/img/REL_s.png", width = 200, height = 200),
           "Gman" = list(src = "www/img/Gman_s.png", width = 200, height = 200),
           "BigO" = list(src = "www/img/BigO_s.png", width = 200, height = 200),
           list(src = "www/img/REBBL.png", width = 200, height = 200*0.6128)
    )
  },
  deleteFile = FALSE
  )
  
  #Update division options based on league
  observe({
    league <- input$league
    switch(league,
           "REL"  = updateSelectInput(session, "division", choices = c("Select Division" = "", 1:7)),
           "Gman" = updateSelectInput(session, "division", choices = c("Select Division" = "", 1:5)),
           "BigO" = updateSelectInput(session, "division", choices = c("Select Division" = "", 1:3)),
           NULL
    )
  })
  #Update week options based on division
  observe({
    league <- input$league
    division <- input$division
    if (league == "BigO") {
      if (division < 3 ) updateSelectInput(session, "week", choices = 1:11, selected = input$week)
    }
  })
  
  weeks_games <- reactive({
    if(input$league == "" | input$division == "" | input$week == "") return(NULL)
    rebbl_data %>% 
      filter(league == input$league, comp == paste0("Season 6 Div ",input$division), round == input$week, ID > 0) %>%
      mutate(V = "-") %>% 
      select(h_coach, h_team, h_score, V, a_score, a_team, a_coach, uuid)
  })
  
  summarise_match <- function(uuid) {
    if(is.null(uuid)) return(NULL)
    full_match_stats <- nufflytics::get_game_stats(uuid, "pc")
    
    #homeNbSupporters == 0 is an admin concede. idMatchCompletionStatus != 0 is a regular concede
    if (full_match_stats$RowMatch$homeNbSupporters == 0 | full_match_stats$RowMatch$idMatchCompletionStatus != 0) return(NULL) 
    
    #Main stats table
    stats_to_collect <- c(
      BLK = "inflictedTackles", 
      AVBr = "inflictedInjuries", 
      KO = "inflictedKO",
      CAS = "inflictedCasualties", 
      KILL = "inflictedDead", 
      SURF = "inflictedPushOuts", 
      INT = "inflictedInterceptions", 
      PASS = "inflictedPasses", 
      CATCH = "inflictedCatches"
    )
    
    scores <- data_frame(stat="TD", home = score(full_match_stats, "home"), away = score(full_match_stats, "away"))
    
    stats <- data_frame(
      stat = names(stats_to_collect),
      home = pmap_dbl(list(list(full_match_stats), stats_to_collect , "home"), stat_total),
      away = pmap_dbl(list(list(full_match_stats), stats_to_collect , "away"), stat_total)
    )
    
    stats <- bind_rows(scores, stats)
    
    #Work out injuries / level ups
    players <- map(c("home","away"), ~player_data(full_match_stats, .)) %>% set_names(c("home","away"))
    
    injuries <- players %>% map(~filter(., ! injuries %in% c(NA, "BH")))
    
    level_ups <- players %>% map(~filter(., lvlup))
    
    list(home = full_match_stats$RowMatch$teamHomeName, away = full_match_stats$RowMatch$teamAwayName, stats = stats, injuries=injuries, level_ups=level_ups)
    
  }
  
  abbr <- function(name) {
    name %>%
      str_replace_all("\\[(.*)\\]","") %>% # strip out 'clan' tags if not clan league
      str_replace_all("\\((.*)\\)", " ( \\1 )") %>% # Put spaces around brackets, so eg. USS Sulaco (REL Chapter) is abbreviated to US(RC)
      str_replace_all("([a-z_.-])([A-Z])", "\\1 \\2") %>%  # add a space before mid-word capitals and 'word separator' punctuation (_.-) followed by a capital
      str_replace_all("[&!,'\"*]",'') %>% # delete these characters
      abbreviate(1)
  }
  
  emph <- function(s) {paste0("<strong>",s,"</strong>")}
  
  format_stats <- function(stats, h_team, a_team) {
    stats %>% 
      filter(stat == "TD" | home + away > 0) %>% 
      mutate(home_bold = ifelse(home > away, emph(home), as.character(home)), away_bold = ifelse(away > home, emph(away), as.character(away))) %>%
      select(stat,home_bold,away_bold) %>% 
      knitr::kable(col.names = c("", abbr(h_team), abbr(a_team)), align = "rrl", format = "html", table.attr = "class = 'table table-condensed'", escape = F) %>% 
      HTML
  }
  
  format_injuries <- function(injuries, h_team, a_team) {
    
  }
  
  format_match <- function(summary) {
    if(is.null(summary)) return(NULL)
    
    title <- paste0(summary$home, " / ", summary$away)
    
    body <- fluidRow(
      column(2, format_stats(summary$stats, summary$home, summary$away)),
      column(5, HTML(knitr::kable(summary$injuries %>% bind_rows(.id = "team"), format="html"))),
      column(5, HTML(knitr::kable(summary$level_ups %>% bind_rows(.id = "team"), format="html")))
    )
    match <- sprintf('<div class="panel panel-default">
    <div class="panel-heading">
      <h3>%s</h3>
    </div>
    %s
    </div>', title, body)
    
    
    match
  }
  
  standings <- reactive({
    if(input$league == "" | input$division == "" | input$week == "") return(NULL)
    
    rebbl_data %>% 
      filter(league == input$league, comp == paste0("Season 6 Div ",input$division), round <= input$week) %>% 
      mutate(home_result = case_when(.$h_score>.$a_score~"Win",.$h_score==.$a_score~"Tie", .$h_score<.$a_score ~"Loss" )) %>% 
      select(h_team, a_team, home_result) %>% 
      gather(team, name, -home_result) %>% 
      group_by(name) %>% 
      summarise(
        Wins = sum(team == "h_team" & home_result == "Win", team == "a_team"& home_result == "Loss"), 
        Draws = sum(home_result == "Tie"), 
        Losses = sum(team == "h_team" & home_result == "Loss", team == "a_team"& home_result == "Win")
      ) %>% 
      mutate(Points = 3*Wins + Draws) %>% 
      arrange(desc(Points))
  })
  
  output$summary <- renderTable(weeks_games())
  output$standings <- renderTable(standings())
  output$game_summary <- renderUI(HTML(summarise_match(weeks_games()[1,"uuid"]) %>% format_match()))
  
})  
