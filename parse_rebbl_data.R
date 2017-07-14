#Call with location of data files eg. Rscript parse_rebbl_data.R ~/bb2_box_score/data

suppressMessages(require(tidyverse))
suppressMessages(require(purrrlyr))
suppressMessages(require(magrittr))
suppressMessages(require(httr))
suppressMessages(require(rvest))
suppressMessages(require(stringr))
suppressMessages(require(nufflytics))

data_dir <- commandArgs(trailingOnly = T)[1]

#load webhook info and API calls from file
load(paste0(data_dir,"/REBBL_parameters.Rda"))
load(paste0(data_dir,"/api.Rda"))

#Get data for leagues
league_html_response <- map2(league_search_strings, "pc", api_query)

#First, build up game info table, since we already have the data from the request
get_league_data <- function(league_response) {
  response_content <- content(league_response) 
  
  #Parse basic table information
  league_games <- response_content %>% 
    html_table %>% 
    extract2(1) %>% # Get first html table in response
    set_colnames(c("comp","round","h_coach","h_team","h_img","score","a_img","a_team","a_coach")) %>% 
    separate(score,c("h_score","a_score")) %>% 
    filter(a_coach != "Coach 2")
  
  if(nrow(league_games)==0) return(NULL) # No games, don't process further
  
  #Add uuids from the [data] attribute of html nodes
  league_games$uuid <- response_content %>% 
    html_nodes("[data]") %>% 
    html_attr("data") %>% 
    magrittr::extract(seq(1,length(.),by=10)) %>% # have the uuid listed 10 times per table row, so just take one
    str_replace_all("^1[012]","") # strip initial 1<platform_code> from uuid so unrecorded games have ID = 0
  
  # add numeric ID for easy comparison and remove concedes (a_score is NA after above processing)
  league_games %>% 
    mutate(ID = strtoi(uuid, base = 16)) #%>% 
  #filter(!is.na(a_score))
}

#For each league, process the division data into a df and bind them all together
league_data <- map_df(league_html_response, ~ map_df(.,get_league_data), .id = "league") %>% as_data_frame()

write_csv(league_data, "data/rebbl_data.csv")