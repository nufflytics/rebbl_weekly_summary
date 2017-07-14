
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  tags$head(
    tags$title("ReBBL match summary"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/extra.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/font-awesome.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/google-font.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/add-on.css"),
    tags$script(src="js/jquery2.min.js"),
    tags$script(src="js/skel.min.js"),
    tags$script(src="js/util.js"),
    tags$script(src="js/backToTop.js"),
    tags$script(src="js/main.js"),
    tags$style(type="text/css", "
           #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
               }
               ")
  ),
  
  #nufflytics header
  includeHTML("www/header.html"),
  
  h2("ReBBL match summary", style = 'margin-top: 10px'),
  
  # Sidebar with a slider input for number of bins
  fluidRow(
    column(3, imageOutput("logo", height = 200)),
    column(3, selectInput("league", "Competition", choices = c("Select Competition" = "", "REL" , "Gman", "BigO"))),
    column(3, conditionalPanel("input.league != ''", selectInput("division", "Division", choices = NULL))),
    column(3, conditionalPanel("input.division != ''", selectInput("week", "Week", choices = c("Select Week" = "", 1:13))))
    ),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage")),
  tableOutput("standings"),
  tableOutput("game_summary")

))
