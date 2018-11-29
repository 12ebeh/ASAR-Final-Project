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

team_match_heros_Input <- function(namespace) {
  ns <- NS(namespace)
  
  tagList(
    uiOutput(ns("team.match.search.ui")),
    uiOutput(ns("team.match.hero.appearance.nomatches")),
    plotOutput(ns("team.match.hero.appearance.plot"))
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

    selectInput(ns("teams.list"), label = "Team Name", select.list)
  })
  
  observeEvent(input$team.search, {
    print("team.search pressed")
    
    team.profile$team.data <- list()
    team.profile$team.data <- get_teams(input$teams.list)
    
    team.profile$team.players <- list()
    team.profile$team.players$players <- get_team_players(input$teams.list)
    
    team.profile$team.matches <- list()
    team.profile$team.pickbans <- list()
    
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
        h3(paste("Team ID:", team.profile$team.data$team_id)),
        h3(paste("Rating:", round(team.profile$team.data$rating))),
        h3(paste("Wins:", team.profile$team.data$wins, "Loss:", team.profile$team.data$losses, "Win Rate:", winrate)),
        hr()
      )
    }
  })
  
  output$team.players <- renderUI({
    if (length(team.profile$team.players) > 0) {
      div(
        h2("Team Members"),
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
      #print(team.players.df)
      
      team.players.df
    }
  })
}

team_match_data <- function (input, output, session, team.profile) {
  ns <- session$ns
  
  team_match_df <- reactive({
    team.profile$team.matches$recent.matches.df %>%
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
      team.profile$team.matches$recent.matches.df <- get_team_matches(team.profile$team.data$team_id)
      #print(team.profile$team.matches$recent.matches.df)
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

team_match_heros <- function (input, output, session, team.profile) {
  ns <- session$ns
  
  pick_bans_df <- reactive({
    if (length(team.profile$team.pickbans) > 0) {
      #print(team.profile$team.pickbans$draft.picks)
        team.profile$team.pickbans$draft.picks %>%
        filter(is.target.team == T)
    }
  })
  
  observeEvent(input$team.match.search.go, {
    if (length(team.profile$team.data) > 0) {
      print("team.match.search.go")
      start.date <- ymd_hms(input$team.match.search.ui.start.time)
      end.date <- ymd_hms(input$team.match.search.ui.end.time)
      team.id <- team.profile$team.data$team_id
      match.count <- as.integer(input$team.match.search.ui.count)
      
      team.profile$team.pickbans <- list()
      team.matches <- get_match_data_by_conditions(limit = match.count, start.date = start.date, end.date = end.date, team.id = team.id)
      if (is.null(team.matches)) {
        print("no matches found")
        team.profile$team.pickbans$nomatches <- T
        return(NULL)
      }
      
      #print(team.matches)
      team.profile$team.pickbans$search.matches <- team.matches
      
      team.matches.names <- names(team.matches[[1]])
      team.profile$team.pickbans$basic.match.metric <- 
      #test <- 
        data.table::rbindlist(fill = T, lapply(team.matches, function (x) {
          x[team.matches.names[1:25]]
        }))
      
      team.profile$team.pickbans$draft.picks <- 
      #test2 <-
        data.table::rbindlist(fill = T, lapply(team.matches, function (x) {
          #pick.team.id <- ifelse(x$radiant.team.id == team.profile$team.data$team.id, 2, 3)
          pick.team.id <- ifelse(x$radiant.team.id == team.id, 2, 3)
          
          draft.picks <- x$draft.picks
          draft.picks$match.id <- x$match.id
          draft.picks$is.target.team <- draft.picks$active_team == pick.team.id
          draft.picks
        }))
      print(team.profile$team.pickbans$draft.picks)
    }
    
    team.profile$team.pickbans$nomatches <- F
  })
  
  output$team.match.search.ui <- renderUI({
    if (length(team.profile$team.data) > 0) {
      time.now <- now()
      time.prev <- time.now - WEEK.IN.SEC
      div(
        hr(),
        h2("Hero Appearances:"),
        div(style="display:inline-block", textInput(ns("team.match.search.ui.start.time"), "From (yyyy-mm-dd hh:mm:ss)", value = ymd_hms(time.prev) )),
        div(style="display:inline-block", textInput(ns("team.match.search.ui.end.time"), "To (yyyy-mm-dd hh:mm:ss)", value = ymd_hms(time.now) )),
        div(style="display:inline-block", textInput(ns("team.match.search.ui.count"), "Number of Matches", value = 30 )),
        div(style="display:inline-block", actionButton(ns("team.match.search.go"), "Find Matches")),
        hr()
      )
    }
  })
  
  output$team.match.hero.appearance.plot <- renderPlot({
    if (length(team.profile$team.pickbans) > 0 && !team.profile$team.pickbans$nomatches) {
      print("team.match.hero.appearance.plot")
      total.match.count <- length(unique(team.profile$team.pickbans$draft.picks$match.id))
      pick_bans_df() %>%
        filter(pick == T) %>%
        group_by(hero_id) %>%
        summarise(picked.count = n(),
                  appearance.rate = n() / total.match.count) %>%
        arrange(desc(appearance.rate)) %>%
        top_n(10, appearance.rate) %>%
        ggplot() +
        geom_col(aes(x = as.factor(hero_id), y = appearance.rate)) +
        xlab("Hero") + ylab("Appearance Rate") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    }
  })
  
  output$team.match.hero.appearance.nomatches <- renderUI({
    if (length(team.profile$team.pickbans) > 0 && team.profile$team.pickbans$nomatches) {
      p("No Matches Found")
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
           team_match_data_Input(PANEL.NAMESPACE),
           team_match_heros_Input(PANEL.NAMESPACE))
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
    team.matches = list(),
    team.pickbans = list()
  )
  
  
  # TODO: Add the panel's server code here
  callModule(teams_search, PANEL.NAMESPACE, team.profile)
  callModule(team_data, PANEL.NAMESPACE, team.profile)
  callModule(team_match_data, PANEL.NAMESPACE, team.profile)
  callModule(team_match_heros, PANEL.NAMESPACE, team.profile)
})

# call this function to add your server logic
ADD_SERVER_LOGIC(team.panel.server)