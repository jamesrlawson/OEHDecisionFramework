#
# This is a Shiny web application to assist in the selection of sites for species undert the "Save our Species" programme. You can run the application by clicking
# the 'Run App' button above.

#############
#change the maximum file size
#############
options(shiny.maxRequestSize=30*1024^2)#change the maximum file size


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("SOS data loader"),
  
  # Sidebar with where you load csv file and select columns 
  sidebarLayout(
    sidebarPanel(
      fileInput("uploadedfile", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", ".xlsx", ".xls")),
      selectInput("lat", "Column with latitude:", choices = NULL), 
      selectInput("long", "Column with longitude:", choices = NULL)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")  ###TODO change to map
    )
  )
)

# define where data is and what columns to use
server <- function(input, output,session) {
  
  info <- eventReactive(input$uploadedfile, {
    req(input$uploadedfile)
    # read in data 
    fext <- file_ext(input$uploadedfile)[1]#get extension
    if(fext == "csv"){
      df <- read.csv(input$uploadedfile$datapath)
    } 
    if(fext %in% c("xls","xlsx")){
      df <- as.data.frame(read_excel(input$uploadedfile$datapath))
    }
    vars <- colnames(df)
    df
    # Update select input immediately after clicking on the action button.
    observe({
      updateSelectInput(session, "lat",  choices = vars)
      updateSelectInput(session, "long", choices = vars)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

