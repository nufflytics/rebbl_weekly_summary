
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
library(purrrlyr)
library(stringr)
library(lubridate)

start_time = lubridate::dmy_hm("210218 0000", tz = "UTC")

shinyServer(function(input, output, session) {
  
  gameweek = difftime(now("UTC"), start_time, units = "weeks") %>% ceiling()
  observeEvent(
    input$division,
    updateNumericInput(session, "week", value = as.numeric(gameweek)),
    once = T
  )
  
  rebbl_data <- read_csv("data/rebbl_data.csv")
  
  #logo for top left
  output$logo <- renderImage({
    switch(input$league,
           "REL"  = list(src = "www/img/REL_s.png", width = 200, height = 200),
           "Gman" = list(src = "www/img/Gman_s.png", width = 200, height = 200),
           "BigO" = list(src = "www/img/BigO_s.png", width = 200, height = 200),
           list(src = "www/img/REBBL_s.png", width = 200, height = 200*0.6128)
    )
  },
  deleteFile = FALSE
  )
  
  #Update division options based on league
  observe({
    league <- input$league
    switch(league,
           "REL"  = updateSelectInput(session, "division", choices = c("Select Division" = "", rebbl_data %>% filter(league=="REL") %>% .$comp %>% unique)),
           "Gman" = updateSelectInput(session, "division", choices = c("Select Division" = "", rebbl_data %>% filter(league=="Gman") %>% .$comp %>% unique)),
           "BigO" = updateSelectInput(session, "division", choices = c("Select Division" = "", rebbl_data %>% filter(league=="BigO") %>% .$comp %>% unique)),
           NULL
    )
  })
  #Update week options based on division
  # observe({
  #   league <- input$league
  #   division <- input$division
  #   if (league == "BigO") {
  #     if (division < 3 ) updateSelectInput(session, "week", choices = c("Select Week"="", 1:11), selected = input$week)
  #   }
  # })
  
  weeks_games <- reactive({
    if(input$league == "" | input$division == "" | input$week == "") return(NULL)
    rebbl_data %>% 
      filter(league == input$league, comp == input$division, round == input$week, ID > 0) %>%
      use_series(uuid)
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
  
  format_player_injury <- function(player_data) {
    get_old_perms <- function(new_injuries, injury_list) {
      #vectorise the new injuries for rotters, etc
      new_injuries %<>% str_split(", ", simplify = T) %>% as.vector
      #remove new_injuries from permanent list
      for (inj in new_injuries) {
        injury_list %<>% str_replace(inj, "")
      }
      
      #Deal with errant commas from removed terms
      injury_list %>% 
        str_replace_all("^, *", "") %>% 
        str_replace_all(", , ", ", ") %>% 
        str_replace(", *$", "")
    }
    
    div(
      p(strong(player_data["name"]), em(paste0("(",player_data["type"],")")), " : " ,strong(player_data['injuries'])),
      p(player_data["skills"], em(get_old_perms(player_data['injuries'], player_data['perms'])))
    )
  }
  format_injuries <- function(inj, h_team, a_team) {
    ret = NULL
    
    if (nrow(inj[["home"]]) > 0) {
      ret = str_c(
        ret,
        h5(h_team) %>% as.character(),
        inj[["home"]] %>% 
          by_row(format_player_injury, .to = "out") %>% 
          use_series("out") %>% 
          map_chr(as.character) %>% 
          str_c(collapse="")
      )
    }
    if (nrow(inj[['away']]) > 0) {
      ret = str_c(
        ret,
        h5(a_team) %>% as.character(),
        inj[["away"]] %>% 
          by_row(format_player_injury, .to = "out") %>% 
          use_series("out") %>% 
          map_chr(as.character) %>% 
          str_c(collapse="")
      )
    }
    
    if(is.null(ret)) return(p("Nil"))
    ret %>% 
      str_c(collapse = "") %>% 
      HTML
  }
  
  format_player_levelup <- function(player_data) {
    div(
      p(strong(player_data["name"]), em(paste0("(",player_data["type"],")")), " : " , player_data["SPP"], icon("arrow-right"), player_data['SPP']+player_data["SPP_gain"]),
      p(player_data["skills"], em(player_data['perms']))
    )
  }
  format_level_ups <- function(lvl_up, h_team, a_team) {
    ret = NULL
    
    if (nrow(lvl_up[["home"]]) > 0) {
      ret = str_c(
        ret,
        h5(h_team) %>% as.character(),
        lvl_up[["home"]] %>% 
          by_row(format_player_levelup, .to = "out") %>% 
          use_series("out") %>% 
          map_chr(as.character) %>% 
          str_c(collapse="") %>% 
          str_replace_all("NA", "")
      )
    }
    if (nrow(lvl_up[['away']]) > 0) {
      ret = str_c(
        ret,
        h5(a_team) %>% as.character(),
        lvl_up[["away"]] %>% 
          by_row(format_player_levelup, .to = "out") %>% 
          use_series("out") %>% 
          map_chr(as.character) %>% 
          str_c(collapse="")%>% 
          str_replace_all("NA", "")
      )
    }
    
    if(is.null(ret)) return(p("Nil"))
    ret %>% 
      str_c(collapse = "") %>% 
      HTML
  }
  
  format_match <- function(summary) {
    if(is.null(summary)) return("")
    
    title <- paste0(summary$home, " / ", summary$away)
    
    body <- fluidRow(
      column(2, h3("Stats"),format_stats(summary$stats, summary$home, summary$away)),
      column(5, h3("Injuries"),format_injuries(summary$injuries, summary$home, summary$away)),
      column(5, h3("Development"), format_level_ups(summary$level_ups, summary$home, summary$away))
    )
    match <- sprintf('<div class="panel panel-default">
    <div class="panel-heading">
      <h1>%s</h1>
    </div>
    %s
    </div>', title, body)
    
    
    match
  }
  
  #Calculate standing up to the selected week
  standings <- reactive({
    if(input$league == "" | input$division == "" | input$week == "") return(NULL)
    
    table <- rebbl_data %>% 
      filter(
        league == input$league, 
        comp == input$division, 
        round <= as.numeric(input$week),
        ID > 0
      ) %>% 
      mutate(home_result = case_when(
        .$h_score>.$a_score~"Win",
        .$h_score==.$a_score~"Tie", 
        .$h_score<.$a_score ~"Loss" )
      ) %>% 
      select(h_team, a_team, home_result) %>% 
      gather(team, name, -home_result) %>% 
      group_by(name) %>% 
      summarise(
        Wins = sum(team == "h_team" & home_result == "Win", team == "a_team"& home_result == "Loss"), 
        Draws = sum(home_result == "Tie"), 
        Losses = sum(team == "h_team" & home_result == "Loss", team == "a_team"& home_result == "Win")
      ) %>% 
      mutate(Points = 3*Wins + Draws, Position = min_rank(desc(Points))) %>% 
      arrange(Position) %>% 
      knitr::kable(format = "html", col.names = c("Team", "Wins", "Ties", "Losses", "Points", "Position"), escape = F) %>% 
      as.character()
    
    sprintf("<div class='standings table table-condensed table-hover'>
            <h3>Standings from completed games up to the end of week %s</h3>
            %s
            </div>", input$week, table)
  })
  
  output$summary <- renderTable(weeks_games())
  output$standings <- renderUI(HTML(standings()))
  output$game_summary <- renderUI({
    if (is.null(weeks_games())) return(NULL)
    
    HTML(map_chr(weeks_games(), ~summarise_match(.) %>% format_match()))
  })
  
})  
