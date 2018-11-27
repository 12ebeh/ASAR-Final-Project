###############################################################################
# Include Libraries
###############################################################################
library(ggplot2)
library(shiny)
library(shinyTime)

###############################################################################
# External Sources
###############################################################################
# File containing data retrieval code
source("data_process.R")

# Panels to display
PANELS <- c()
ADD_PANEL <- function(new.panel) {
  if (class(new.panel) != "shiny.tag") {
    print(paste("New Panel is not a shiny.tag, class =", class(new.panel)))
    return()
  }
  
  PANELS[[(length(PANELS) + 1)]] <<- new.panel
}

# Server code for respective panels
SERVER.LOGIC <- c()
ADD_SERVER_LOGIC <- function(new.logic) {
  if (class(new.logic) != "{") {
    print(paste("New Server Logic is not a closure, class =", class(new.logic)))
    return()
  }
  
  SERVER.LOGIC[[(length(SERVER.LOGIC) + 1)]] <<- new.logic
}

#source("template_panel.R", local = T)
source("teams_panel.R", local = T)
source("player_panel.R", local = T)


###############################################################################
# Page UI
###############################################################################
ui <- shiny::fluidPage(
  fluidRow(h1("Dota 2 Analyics Tool")),
  do.call(tabsetPanel, PANELS)
)

###############################################################################
# Page UI
###############################################################################
server <- function(input, output, session) {
  for (logic in SERVER.LOGIC) {
    #print(logic)
    eval(logic)
  }
}

shiny::shinyApp(ui, server)