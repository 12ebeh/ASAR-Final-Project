###############################################################################
# Page Panel's Module Code
# Check here for tutorial: http://shiny.rstudio.com/articles/modules.html
###############################################################################

## NEED TO DEFINE AN UNIQUE PANEL NAMESPACE
PANEL.NAMESPACE <- "player"

###############################################################################
# Panel's UI Modules
###############################################################################
player_metrics_Input <- function(namespace) {
  # Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  div(
    uiOutput(ns("pro.player.metrics")),
    uiOutput(ns("player.metrics"))
  )
}

player_match_metrics_by_hero_Input <- function(namespace) {
  # Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  tagList(
    plotOutput(ns("player.win.rate.by.hero")),
    uiOutput(ns("player.basic.match.metrics.by.hero.control")),
    plotOutput(ns("player.basic.match.metrics.by.hero.plot")),
    hr()
  )
}

player_match_metrics_by_hero_time_series_Input <- function(namespace) {
  ns <- NS(namespace)
  
  tagList(
    uiOutput(ns("player.basic.match.metrics.time.series.control")),
    plotOutput(ns("player.basic.match.metrics.time.series.plot")),
    hr()
  )
}

player_match_metics_table_Input <- function(namespace) {
  # Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  # Your component UI code here, currently using the file import module code
  # from the tutorial
  uiOutput(ns("player.basic.match.metrics"))
}


###############################################################################
# Panel's Server Modules
###############################################################################
player_metrics <- function(input, output, session, player.profile) {
  ns <- session$ns
  
  output$pro.player.metrics <- renderUI({
    if (length(player.profile$pro.data) > 0) {
      tagList(
        h3(player.profile$pro.data$personname, style = "display:inline;"),
        a("Profile URL", href = player.profile$pro.data$profile.url, style = "display:inline;"),
        p(paste("Country:", player.profile$pro.data$countrycode)),
        h5(paste("Team:", player.profile$pro.data$team.name))
      )
    }
  })
  
  output$player.metrics <- renderUI({
    if (length(player.profile$player.data) > 0) {
      tagList(
        p(paste("MMR:", player.profile$player.data$mmr)),
        hr()
      )
    }
  })
}

player_basic_match_metrics_by_hero <- function(input, output, session, player.profile) {
  ns <- session$ns
  
  df_by_hero <- reactive({
    # TODO: Add more metrics here
    if (length(player.profile$match.data) > 0 ) {
      df <- player.profile$match.data$basic.metrics %>%
        group_by(hero.id) %>%
        summarise(win.rate = sum(is.win) / n())
                  #mean.level = mean(level),
                  #mean.kills = mean(kills),
                  #mean.deaths = mean(deaths),
                  #mean.assists = mean(assists),
                  #mean.kda = mean(kda),
                  #mean.gold.per.min = mean(gold.per.min),
                  #mean.xp.per.min = mean(xp.per.min),
                  #mean.actions.per.min = mean(actions.per.min),
                  #)
      
      #df.names <- colnames(df)
      #print(df)
      
      #df %>% gather(metric, value, win.rate, average.level, average.kills, average.deaths, average.assists)
    }
  })
  
  output$player.win.rate.by.hero <- renderPlot({
    if (length(player.profile$match.data) > 0) {
      df_by_hero() %>%
        ggplot() +
        geom_col(aes(x = as.factor(hero.id), y = win.rate))
    }
  })
  
  output$player.basic.match.metrics.by.hero.control <- renderUI({
    if (length(player.profile$match.data) > 0) {
      #df <- df_by_hero()
      metric.names <- 
        player.profile$match.data$basic.metrics %>% 
        select(level, kills, deaths, assists, kda, total.gold, gold, gold.spent, gold.per.min, total.xp, xp.per.min, actions.per.min, last.hits,
               denies, stuns) %>%
        colnames()
      print(metric.names)

      div(
        h3("Match Metrics by Hero"),
        div(style = "display: inline-block; vertical-align:top;",
            selectInput(ns("metric.select"), "Select Metric", choices = metric.names))
      )
    }
  })
  
  output$player.basic.match.metrics.by.hero.plot <- renderPlot({
    if (length(player.profile$match.data) > 0 && !is.null(input$metric.select)) {
      #print(head(player.profile$match.data$basic.metrics))
      player.profile$match.data$basic.metrics %>%
        select(hero.id, input$metric.select) %>%
        #filter(metric == input$metric.select) %>%
        ggplot() +
        geom_boxplot(aes_string(x = "as.factor(hero.id)", y = input$metric.select)) +
        xlab("Heroes Played") + ylab(input$metric.select) + 
        expand_limits(y = 0)
      }
  })
}

player_match_metrics_by_hero_time_series <- function (input, output, session, player.profile) {
  ns <- session$ns
  
  df.ts <- reactive({
    # TODO: Add more metrics
    if (length(player.profile$match.data) > 0) {
      player.profile$match.data$basic.metrics %>%
        group_by(hero.id) %>%
        arrange(hero.id, start.time) %>%
        mutate(time.id = 1:n()) %>%
        select(hero.id, start.time, level, kills, deaths, assists, kda, total.gold, gold, gold.spent,
               gold.per.min, total.xp, xp.per.min, actions.per.min, last.hits, denies, stuns, time.id) %>%
        gather(metric, value, level, kills, deaths, assists, kda, total.gold, gold, gold.spent,
               gold.per.min, total.xp, xp.per.min, actions.per.min, last.hits, denies, stuns)
    }
  })
  
  output$player.basic.match.metrics.time.series.control <- renderUI({
    if (length(player.profile$match.data) > 0) {
      df <- df.ts()
      hero.list <- unique(df$hero.id)
      metrics.list <- unique(df.ts()$metric)
      div(
        div(style = "display:inline-block; vertical-align:top;", 
            selectInput(ns("ts.hero.select"), label = "Select Hero", choices = hero.list)),
        div(style = "display:inline-block; vertical-align:top;",
            selectInput(ns("ts.metric.select"), label = "Select Metric", choices = metrics.list))
      )
    }
  })
  
  output$player.basic.match.metrics.time.series.plot <- renderPlot({
    print(input$ts.hero.select)
    print(input$ts.metric.select)
    if (length(player.profile$match.data) > 0 && !is.null(input$ts.hero.select) && !is.null(input$ts.metric.select)) {
      df <- df.ts() %>%
        filter(hero.id == as.integer(input$ts.hero.select), metric == input$ts.metric.select)
      
      p <- ggplot(df) +
        geom_point(aes(x = as.factor(start.time), y = value)) +
        xlab("Date Played") + ylab(input$ts.metric.select)
      
      if (nrow(df) > 1) {
        p <- p + geom_line(aes(x = as.factor(start.time), y = value, group = 1))
      }
      
      p + expand_limits(y = 0)
    }
  })
}


player_match_metics_table <- function(input, output, session, player.profile) {
  show_match_metrics_table <- eventReactive(input$show.match.metrics.table, {
    player.profile$show.match.data = !player.profile$show.match.data
  })
  
  output$player.basic.match.metrics <-
    renderUI({
      if (length(player.profile$match.data) > 0) {
        button.text <- "Show Match Data"
        ns <- session$ns
        tagList(
          actionButton(ns("show.match.metrics.table"), button.text),
          br(),
          dataTableOutput(ns("player.basic.match.metrics.table")),
          hr()
        )
      }
    })
  
  output$player.basic.match.metrics.table <- renderDataTable({
    if (show_match_metrics_table()) {
      player.profile$match.data$basic.metrics
    }
  }, options = list(scrollX=T, pageLength=20))
}


###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
# define the tab panel components
player.panel <- tabPanel(
  title = "Player",
  fluidRow(
    column(3, h2("Player"),
           uiOutput("pro.player.list"),
           actionButton("search", "Search")),
    column(9, h2("Player Information"), 
           player_metrics_Input(PANEL.NAMESPACE),
           player_match_metrics_by_hero_Input(PANEL.NAMESPACE),
           player_match_metrics_by_hero_time_series_Input(PANEL.NAMESPACE),
           player_match_metics_table_Input(PANEL.NAMESPACE))
           
  )
)

# call this function to add the tab panel
ADD_PANEL(player.panel)

###############################################################################
# Add Panel into the list of panels to be displayed
# TODO: Figure a way to save expressions to be evaluated into a varaible
#       and run them in the server() function
###############################################################################
player.panel.server <- substitute({
  player.profile <- reactiveValues(
    pro.data = list(),
    player.data = list(),
    match.data = list(),
    show.match.data = F
  )
  
  pro_player_account_id <- reactive({
    pro.players <- get_pro_players()
    return(pro.players$account_id)
  })
  
  output$pro.player.list <- renderUI({
    div(
      selectInput("account.id", "Account ID", pro_player_account_id()),
      textInput("last.match.count", "Number of Match to Retrieve", value = 30)
    )
  })
  
  observeEvent(input$search, {
    print(paste("serach reactive called, account id:", input$account.id))
    
    account.id <- input$account.id
    pro.player <- get_pro_players(account.id)
    player <- get_player(account.id)
    player.matches <- get_player_match_details(account.id, limit = 30)#as.integer(input$last.match.count))
    
    player.profile$pro.data <- list()
    if (!is.null(pro.player)) {
      player.profile$pro.data$personname <- ifelse(!is.null(pro.player$personaname),
                                                   as.character(pro.player$personaname),
                                                   as.character(account.id))
      player.profile$pro.data$name <- as.character(pro.player$name)
      player.profile$pro.data$team.name <- as.character(pro.player$team_name)
      player.profile$pro.data$countrycode <- as.character(pro.player$loccountrycode)
      player.profile$pro.data$profile.url <- as.character(pro.player$profileurl)
    }
    
    player.profile$player.data <- list()
    if (!is.null(player)) {
      player.profile$player.data$mmr <- player$mmr_estimate$estimate
    }
    
    player.profile$match.data <- list()
    if (!is.null(player.matches)) {
      player.matches.names <- names(player.matches[[1]])
      print(player.matches.names)
      player.matches.df <- data.table::rbindlist(fill = T, lapply(player.matches, function (x) {
        x[player.matches.names[3:45]]
      }))
      player.profile$match.data$basic.metrics <- player.matches.df
    }
  })
  
  callModule(player_metrics, PANEL.NAMESPACE, player.profile)
  callModule(player_basic_match_metrics_by_hero, PANEL.NAMESPACE, player.profile)
  callModule(player_match_metrics_by_hero_time_series, PANEL.NAMESPACE, player.profile)
  callModule(player_match_metics_table, PANEL.NAMESPACE, player.profile)
})

# call this function to add your server logic
ADD_SERVER_LOGIC(player.panel.server)
