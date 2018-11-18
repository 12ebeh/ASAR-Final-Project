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
  
  tagList(
    textInput(ns("team_id"), label = "Team ID"),
    actionButton(ns("team_search"), label = "Team Search")
    )
}

teams_variable_select <- function(namespace) {
  # Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  # Your component UI code here, currently using the file import module code
  # from the tutorial
  tagList(
    column(2, h2("Variables"), tags$ul(
      actionButton(ns("Variable1"), "Variable 1"),
      actionButton(ns("Variable2"), "Variable 2")
    )),
    column(3, h2("Control Panel"), uiOutput(ns("controlUI"))),
    column(7, h2("Graphical Plot"))
  )
}

###############################################################################
# Panel's Server Modules
###############################################################################
teams_variable <- function(input, output, session) {
  v <- reactiveValues(
    action.ui = NULL
  )
  
  observeEvent(input$Variable1, {
    v$action.ui <- sliderInput("new_slider", label = "Variable 1 Slider", 1, 10, 5)
  })
  
  observeEvent(input$Variable2, {
    v$action.ui <- selectInput("new_select", label = "Variable 2 Select",
                               choices = c("Choice 1" = "choice1",
                                           "Choice 2" = "choice2"))
  })
  
  output$controlUI <- renderUI({
    if (is.null(v$action.ui)) {
      return()
    }
    v$action.ui
  })
}


###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
# define the tab panel components
teams.panel <- tabPanel(
  title = "Teams",
  fluidRow(teams_search_input(PANEL.NAMESPACE)),
  fluidRow(teams_variable_select(PANEL.NAMESPACE))
)

# call this function to add the tab panel
ADD_PANEL(teams.panel)


###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
team.panel.server <- substitute({
  # TODO: Add the panel's server code here
  callModule(teams_variable, PANEL.NAMESPACE)
  
  
  
})

# call this function to add your server logic
ADD_SERVER_LOGIC(team.panel.server)