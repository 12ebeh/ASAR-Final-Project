###############################################################################
# Page Panel's Module Code
# Check here for tutorial: http://shiny.rstudio.com/articles/modules.html
###############################################################################

## NEED TO DEFINE AN UNIQUE PANEL NAMESPACE
PANEL.NAMESPACE <- "teams"

###############################################################################
# Panel's UI Modules
###############################################################################
teams_search_input <- function (namespace) {
  # Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  column(12, div(
    div(uiOutput(ns("teams.list"))),
    div(actionButton(ns("team.search"), label = "Team Search")))
  )
}

###############################################################################
# Panel's Server Modules
###############################################################################
team_search <- function(input, output, session, team.profile) {
  ns <- session$ns
  
  #observeEvent(input$team.search {
    
  #})
  
  teams_df <- reactive({
    teams <- get_teams()
    return(teams)
  })
  
  output$teams.list <- renderUI({
    
    div(
      div(style = "display: inline-block; vertical-align:top;", selectInput(ns("team.id"), "Team ID", as.list(teams_df())))
      #div(style = "display: inline-block; vertical-align:top;", textInput(ns("last.match.count"), "Number of Match to Retrieve", value = 30))
    )
  })
  
}

###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
# define the tab panel components
teams.panel <- tabPanel(
  title = "Teams",
  fluidRow(
    column(3, teams_search_input(PANEL.NAMESPACE)),
    column(9, {})
  )
  #fluidRow(teams_variable_select(PANEL.NAMESPACE))
)

# call this function to add the tab panel
ADD_PANEL(teams.panel)


###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
team.panel.server <- substitute({
  team.profile <- reactiveValues(
    basic.data = list(),
    team.members = list(),
    team.matches = list()
  )
  
  # TODO: Add the panel's server code here
  callModule(team_search, PANEL.NAMESPACE, team.profile)
  
  
  
})

# call this function to add your server logic
ADD_SERVER_LOGIC(team.panel.server)