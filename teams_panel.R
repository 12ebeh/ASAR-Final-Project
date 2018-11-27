###############################################################################
# Page Panel's Module Code
# Check here for tutorial: http://shiny.rstudio.com/articles/modules.html
###############################################################################

## NEED TO DEFINE AN UNIQUE PANEL NAMESPACE
PANEL.NAMESPACE <- "teams"

###############################################################################
# Panel's UI Modules
###############################################################################
teams_search_Input <- function (namespace) {
  # Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  tagList(
    uiOutput(ns("teams.list")),
    actionButton(ns("team.search"), label = "Team Search")
  )
}

team_data_Input <- function(namespace) {
  ns <- NS(namespace)
  
  tagList(
    uiOutput(ns("team.data")),
    uiOutput(ns("team.players"))
  )
}

team_match_data_Input <- function(namespace) {
  ns <- NS(namespace)
  
  tagList(
    uiOutput(ns("team.matches.control")),
    dataTableOutput(ns("team.matches.df"))
  )
}

###############################################################################
# Panel's Server Modules
###############################################################################
teams_search <- function(input, output, session, team.profile) {
  ns <- session$ns
  
  teams_df <- reactive({
    teams <- get_teams()
    return(teams)
  })
  
  output$teams.list <- renderUI({
    df <- teams_df()
    select.list <- setNames(df$team_id, as.character(df$name))

    selectInput(ns("teams.list"), label = "Team ID", select.list)
  })
  
  observeEvent(input$team.search, {
    print("team.search pressed")
    
    team.profile$team.data <- list()
    team.profile$team.data <- get_teams(input$teams.list)
    
    team.profile$team.players <- list()
    team.profile$team.players$players <- get_team_players(input$teams.list)
    
    team.profile$team.matches <- list()
    
  })
  
}

team_data <- function (input, output, session, team.profile) {
  ns <- session$ns
  
  output$team.data <- renderUI({
    if (length(team.profile$team.data) > 0) {
      winrate = scales::percent(team.profile$team.data$wins / (team.profile$team.data$wins + team.profile$team.data$losses))
      
      div(
        h2(team.profile$team.data$name),
        img(src = team.profile$team.data$logo_url, style = "background-color:lightgrey;"),
        h3(paste("Rating:", round(team.profile$team.data$rating))),
        h3(paste("Wins:", team.profile$team.data$wins, "Loss:", team.profile$team.data$losses, "Win Rate:", winrate)),
        hr()
      )
    }
  })
  
  output$team.players <- renderUI({
    if (length(team.profile$team.players) > 0) {
      div(
        dataTableOutput(ns("team.players.df")),
        hr()
      )
      
    }
  })
  
  output$team.players.df <- renderDataTable({
    if (length(team.profile$team.players) > 0) {
      team.players.df <- team.profile$team.players$players %>%
        filter(is_current_team_member == T) %>%
        select(name, games_played, wins)
      print(team.players.df)
      
      team.players.df
    }
  })
}

team_match_data <- function (input, output, session, team.profile) {
  ns <- session$ns
  
  team_match_df <- reactive({
    team.profile$team.matches$matches.df %>%
      mutate(faction = ifelse(radiant, "Radiant", "Dire"),
             win = (radiant_win && radiant) || !(radiant_win && radiant),
             date = floor_date(ymd_hms(as.POSIXct(start_time, origin = "1970-01-01")), unit = "minute")) %>%
      select(league_name, match_id, date, faction, win, opposing_team_name) %>%
      arrange(desc(date)) %>%
      top_n(as.integer(input$team.matches.get.count), date)
  })
  
  observeEvent(input$team.matches.get, {
    print(paste("team.matches.get", "pressed"))
    
    team.profile$team.matches <- list()
    
    if (length(team.profile$team.data) > 0) {
      team.profile$team.matches$matches.df <- get_team_matches(team.profile$team.data$team_id)
      print(team.profile$team.matches$matches.df)
    }
  })
  
  output$team.matches.control <- renderUI({
    if (length(team.profile$team.data) > 0) {
      div(
        div(style = "display:inline-block;", textInput(ns("team.matches.get.count"), label = "Get Recent Matches:", value = 10)),
        div(style = "display:inline-block;", actionButton(ns("team.matches.get"), label = "Get Matches"))
      )
    }
    
  })
  
  output$team.matches.df <- renderDataTable({
    if (length(team.profile$team.matches) > 0) {
      team_match_df()
    }
  })
}


###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
# define the tab panel components
teams.panel <- tabPanel(
  title = "Teams",
  fluidRow(
    column(3, teams_search_Input(PANEL.NAMESPACE)),
    column(9, 
           team_data_Input(PANEL.NAMESPACE),
           team_match_data_Input(PANEL.NAMESPACE))
  )
)

# call this function to add the tab panel
ADD_PANEL(teams.panel)


###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
team.panel.server <- substitute({
  team.profile <- reactiveValues(
    team.data = list(),
    team.players = list(),
    team.matches = list()
  )
  
  
  # TODO: Add the panel's server code here
  callModule(teams_search, PANEL.NAMESPACE, team.profile)
  callModule(team_data, PANEL.NAMESPACE, team.profile)
  callModule(team_match_data, PANEL.NAMESPACE, team.profile)
  
  
})

# call this function to add your server logic
ADD_SERVER_LOGIC(team.panel.server)