###############################################################################
# Page Panel's Module Code
# Check here for tutorial: http://shiny.rstudio.com/articles/modules.html
###############################################################################

## NEED TO DEFINE AN UNIQUE PANEL NAMESPACE
PANEL.NAMESPACE <- "template"

###############################################################################
# Panel's UI Modules
###############################################################################
templateInput <- function(namespace) {
  ### Namespace is absolutely required, DO NOT REMOVE!!
  ns <- NS(namespace)
  
  # Your component UI code here, currently using the file import module code
  # from the tutorial
  tagList(
    fileInput(ns("file"), "Import File"),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}

###############################################################################
# Panel's Server Modules
###############################################################################
template <- function(input, output, session, stringsAsFactors = T) {
  # Your component server code here, currently using the file import module code
  # from the tutorial
  
  #print("template function called")
  
  # The selected file, if any
  userFile <- reactive({
    #print("userFile reactive called")
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    #print("dataframe reactive called")
    read.csv(userFile()$datapath,
             header = input$heading,
             quote = input$quote,
             stringsAsFactors = stringsAsFactors)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}

###############################################################################
# Add Panel into the list of panels to be displayed
###############################################################################
# define the tab panel components
template.panel <- tabPanel(
  title = "Template",
  fluidRow(
    column(2, h2("File Import"), templateInput(PANEL.NAMESPACE)),
    column(10, h2("Data Table"), dataTableOutput("table"))#,
    #column(7, h2("Graphical Plot"))
  )
)

# call this function to add the tab panel
ADD_PANEL(template.panel)

###############################################################################
# Add the tab panel's server code here
###############################################################################
template.panel.server <- substitute({
  # Server code below
  datafile <- callModule(template, PANEL.NAMESPACE,
                                    stringsAsFactors = FALSE)
  output$table <- renderDataTable({
    datafile()
    })
})

# call this function to add your server logic
ADD_SERVER_LOGIC(template.panel.server)