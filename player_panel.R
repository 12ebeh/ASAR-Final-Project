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

player_match_metics_table_Input <- function(namespace) {
  # Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  # Your component UI code here, currently using the file import module code
  # from the tutorial
  uiOutput(ns("player.basic.match.metrics"))
}

player_match_metrics_plot_Input <- function(namespace) {
  # Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  tagList(
    uiOutput(ns("player.match.metrics.control")),
    plotOutput(ns("player.basic.match.metrics.plot"))
  )
}

player_match_metrics_summary_Input <- function(namespace) {
  # Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  
}

###############################################################################
# Panel's Server Modules
###############################################################################
player_metrics <- function(input, output, session, player.profile) {
  ns <- session$ns
  
  output$pro.player.metrics <- renderUI({
    if (length(player.profile$pro.data) > 0) {
      tagList(
        h3(player.profile$pro.data$personname),
        h4(paste("Team Name:", player.profile$pro.data$team.name)),
        p(paste("Country:", player.profile$pro.data$countrycode)),
        a("Profile URL", href = player.profile$pro.data$profile.url)
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

player_match_metics_plot <- function(input, output, session, player.profile) {
  ns <- session$ns
  
  output$player.match.metrics.control <- renderUI({
    if (length(player.profile$match.data) > 0) {
      metric.names <- colnames(player.profile$match.data$basic.metrics)
  
      div(
        div(style = "display: inline-block; vertical-align:top;",
            selectInput(ns("metric.select"), "Select Metric", choices = metric.names[-c(1:3,5:16)])),
        div(style = "display: inline-block; vertical-align:top;",
            sliderInput(ns("metric.bin.size"), "Bin Size", 1, 50, 5))
      )
    }
  })
  
  output$player.basic.match.metrics.plot <- renderPlot({
    if (length(player.profile$match.data) > 0) {
      print(head(player.profile$match.data$basic.metrics))
      ggplot(player.profile$match.data$basic.metrics) +
        geom_histogram(aes_string(x = as.character(input$metric.select)), binwidth = as.integer(input$metric.bin.size))
    }
  })
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
           player_match_metrics_plot_Input(PANEL.NAMESPACE),
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
    player.matches <- get_player_match_details(account.id, limit = as.integer(input$last.match.count))
    
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
      player.matches.df <- data.table::rbindlist(lapply(player.matches, function (x) {
        x[player.matches.names[2:37]]
      }))
      player.profile$match.data$basic.metrics <- player.matches.df
    }
  })
  
  callModule(player_metrics, PANEL.NAMESPACE, player.profile)
  callModule(player_match_metics_table, PANEL.NAMESPACE, player.profile)
  callModule(player_match_metics_plot, PANEL.NAMESPACE, player.profile)
})

# call this function to add your server logic
ADD_SERVER_LOGIC(player.panel.server)
