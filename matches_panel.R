###############################################################################
# Page Panel's Module Code
# Check here for tutorial: http://shiny.rstudio.com/articles/modules.html
###############################################################################

## NEED TO DEFINE AN UNIQUE PANEL NAMESPACE
PANEL.NAMESPACE <- "matches"

###############################################################################
# Panel's UI Modules
###############################################################################
match_search_Input <- function(namespace) {
  ### Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  # Your component UI code here, currently using the file import module code
  # from the tutorial
  tagList(
    uiOutput(ns("match.serach.ui"))
  )
}

match_overall_Input <- function(namespace) {
  ns <- NS(namespace)
  
  tagList(
    uiOutput(ns("match.overall.ui"))
  )
}

###############################################################################
# Panel's Server Modules
###############################################################################
match_search <- function(input, output, session, match.data) {
  ns <- session$ns
  
  observeEvent(input$match.search.ui.action, {
    match.count <- as.integer(input$match.search.ui.match.count)
    if (is.na(match.count)) {
      match.count = 100
    } else {
      match.count = min(100, match.count)
    }
    
    start.date <- ymd_hms(input$match.search.ui.start.date)
    end.date <- ymd_hms(input$match.search.ui.end.date)
    team.id <- as.integer(input$match.search.ui.team.id)
    match.ids <- as.character(input$match.search.ui.match.id)
    print(match.ids)
    if (match.ids != "") {
      match.ids <- unlist(strsplit(match.ids, ","))
      match.ids <- lapply(match.ids, function(x) {
        as.integer(x)
      })
      match.ids <- match.ids[!is.na(match.ids)]
    } else {
      match.ids <- list()
    }
    
    print("debug")
    match.data$search.match.data <- list()
    df.all <- get_match_and_player_details_by_conditions(limit = match.count, start.date = start.date, end.date = end.date, team.id = team.id, match.id.list = match.ids)
    
    
    print("debug3")
    if (is.null(df.all)) {
      print("no matches found")
      match.data$search.match.data$nomatches <- T
      match.data$search.match.data$found.matches <- list()
      return(NULL)
    }
    found.matches <- df.all[[1]]
    match.data$search.match.data$nomatches <- F
    match.data$search.match.data$found.matches <- found.matches
    print("debug2")
    
    match.data$processed.match.data <- list()
    
    found.matches.name <- names(found.matches[[1]])
    match.data$processed.match.data$basic.match.metric <- 
    #test <- 
      data.table::rbindlist(fill = T, lapply(found.matches, function (x) {
        x[found.matches.name[1:25]]
      }))
    
    
    match.data$processed.match.data$draft.picks <- 
    #test2 <-
      data.table::rbindlist(fill = T, lapply(found.matches, function (x) {
        pick.team.id <- ifelse(x$radiant.team.id == team.id, 2, 3)
        
        if (nrow(x$draft.picks) == 0) {
          draft.picks <- data.frame(match.id = NA)
          return (draft.picks)
        }
          
        draft.picks <- x$draft.picks
        draft.picks$match.id <- x$match.id
        #draft.picks$is.target.team <- draft.picks$active_team == pick.team.id
        return(draft.picks)
      }))
    
    match.data$processed.match.data$objectives <- 
    #test3 <-
      data.table::rbindlist(fill = T, lapply(found.matches, function (x) {
        if (nrow(x$objectives) == 0) {
          objectives <- data.frame(match.id = NA)
          return (objectives)
        }
        
        objectives <- x$objectives
        objectives$match.id <- x$match.id
        return(objectives)
      }))
    
    match.data$processed.match.data$gold.xp.adv <-
    #test4 <-
      data.table::rbindlist(fill = T, lapply(found.matches, function (x) {
        if (nrow(x$gold.xp.adv) == 0) {
          gold.xp.adv <- data.frame(match.id = NA)
          return (gold.xp.adv)
        }
        
        gold.xp.adv <- x$gold.xp.adv
        gold.xp.adv$match.id <- x$match.id
        return(gold.xp.adv)
      }))
    
    #found.matches.player.data <- get_player_match_details_by_conditions(limit = match.count, start.date = start.date, end.date = end.date, team.id = team.id, match.id.list = match.ids)
    found.matches.player.data <- df.all[[2]]
    if (is.null(found.matches.player.data)) {
      return(NULL)
    }
    
    found.matches.player.data.names <- names(found.matches.player.data[[1]])
    
    match.data$processed.match.player.data <- list()
    
    #match.data$processed.match.player.data$basic.player.metric <-
    player.test <-
      data.table::rbindlist(fill = T, lapply(found.matches.player.data, function (x) {
      x[found.matches.player.data.names[1:45]]
    }))
    
    #match.data$processed.match.player.data$player.purchase.log <-
    player.test2 <- 
      data.table::rbindlist(fill = T, lapply(found.matches.player.data, function (x) {
        if (nrow(x$purchase.log) == 0) {
          purchase.log <- data.frame(match.id = NA)
          return (purchase.log)
        }
        
        purchase.log <- x$purchase.log
        purchase.log$match.id <- x$match.id
        purchase.log$account.id <- x$account.id
        return(purchase.log)
      }))
    
    match.data$processed.match.player.data$player.runes.log <-
    #player.test3 <- 
      data.table::rbindlist(fill = T, lapply(found.matches.player.data, function (x) {
        if (nrow(x$runes.log) == 0) {
          runes.log <- data.frame(match.id = NA)
          return (runes.log)
        }
        
        runes.log <- x$runes.log
        runes.log$match.id <- x$match.id
        runes.log$account.id <- x$account.id
        return(runes.log)
      }))
    
    #match.data$processed.match.player.data$player.purchase.log <-
    player.test4 <-
      data.table::rbindlist(fill = T, lapply(found.matches.player.data, function (x) {
        if (nrow(x$metrics_t) == 0) {
          metrics_t <- data.frame(match.id = NA)
          return (metrics_t)
        }
        
        metrics_t <- x$metrics_t
        metrics_t$match.id <- x$match.id
        metrics_t$account.id <- x$account.id
        return(metrics_t)
      }))
    
    #match.data$processed.match.player.data$player.abiliies.upgrade <-
    player.test5 <-
      data.table::rbindlist(fill = T, lapply(found.matches.player.data, function (x) {
        if (nrow(x$abilities.upgrade) == 0) {
          abilities.upgrade <- data.frame(match.id = NA)
          return (abilities.upgrade)
        }
        
        abilities.upgrade <- x$abilities.upgrade
        abilities.upgrade$match.id <- x$match.id
        abilities.upgrade$account.id <- x$account.id
        return(abilities.upgrade)
      }))
    
    print("debug5")
  })
  
  output$match.serach.ui <- renderUI({
    end.date <- now()
    start.date <- end.date - DAY.IN.SEC * 90
    
    div(
      textInput(ns("match.search.ui.match.count"), label = "Number of Matches"),
      textInput(ns("match.search.ui.start.date"), label = "From (yyyy-mm-dd)", value = ymd_hms(start.date)),
      textInput(ns("match.search.ui.end.date"), label = "To (yyyy-mm-dd)", value = ymd_hms(end.date)),
      textInput(ns("match.search.ui.team.id"), label = "Team ID"),
      textInput(ns("match.search.ui.match.id"), label = "Match ID (<Match ID 1>, <Match ID 2>)"),
      actionButton(ns("match.search.ui.action"), label = "Find Matches")
    )
  })
}

match_overall <- function(input, output, session, match.data) {
  ns <- session$ns
  
  match_overall_win_rate <- reactive({
    if (length(match.data$processed.match.data) > 0) {
      match.data$processed.match.data$basic.match.metric %>%
        select(radiant.win) %>%
        transmute(radiant = sum(as.integer(radiant.win)),
                  dire = n() - radiant) %>%
        unique() %>%
        gather(faction, wins)
    }
  })
  
  output$match.overall.ui <- renderUI({
    if (length(match.data$processed.match.data) > 0) {
      div(h2("Match Statistics"),
          plotOutput(ns("match.overall.faction.win.rate.plot")),
          plotOutput(ns("match.overall.duration.plot"))
      )
    }
  })
  
  output$match.overall.faction.win.rate.plot <- renderPlot({
    if (length(match.data$processed.match.data) > 0) {
      df <- match_overall_win_rate()
      total.wins <- sum(df$wins)
      df$win.rate <- df$wins / total.wins
      ggplot(df) +
        geom_col(aes(x = faction, y = win.rate))
    }
  })
  
  output$match.overall.duration.plot <- renderPlot({
    if (length(match.data$processed.match.data) > 0) {
      match.data$processed.match.data$basic.match.metric %>%
        select(duration) %>%
        transmute(duration = round(duration / MINUTE.IN.SEc, 2) ) %>%
        ggplot() +
        geom_histogram(aes(x = duration))
    }
  })
}

###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
# define the tab panel components
mathes.panel <- tabPanel(
  title = "Matches",
  fluidRow(
    column(3, match_search_Input(PANEL.NAMESPACE)),
    column(9, match_overall_Input(PANEL.NAMESPACE))
    #column(10, h2("Data Table"), dataTableOutput("table"))#,
    #column(7, h2("Graphical Plot"))
  )
)

# call this function to add the tab panel
ADD_PANEL(mathes.panel)

###############################################################################
# Add the tab panel's server code here
###############################################################################
matches.panel.server <- substitute({
  # Server code below
  match.data <- reactiveValues(
    search.match.data = list(),
    processed.match.data = list()
  )
  
  callModule(match_search, PANEL.NAMESPACE, match.data)
  callModule(match_overall, PANEL.NAMESPACE, match.data)
})

# call this function to add your server logic
ADD_SERVER_LOGIC(matches.panel.server)