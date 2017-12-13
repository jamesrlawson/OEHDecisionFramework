#
# This is a Shiny web application to assist in the selection of sites for species undert the "Save our Species" programme. You can run the application by clicking
# the 'Run App' button above.

#############
#change the maximum file size
#############
options(shiny.maxRequestSize=30*1024^2)#change the maximum file size

library(shiny)

# Define UI for application lets you interactively read in csv file
ui <- fluidPage(
   
   # Application title
   titlePanel("SOS data loader"),
   
   #choose the csv file
     sidebarLayout(
       sidebarPanel(
         
         fileInput("datafile", "Choose CSV File",
                   multiple = FALSE,
                   accept = c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")),
         selectInput("lat", "Column with latitude:", choices = NULL), 
         selectInput("long", "Column with longitude:", choices = NULL)
       )
     )
   )

#define where data is and what columns to use
server <- function(input, output,session) {
  
  # info <- eventReactive(input$uploadedfile, {
  #   
  #   req(input$uploadedfile)
  #   
  #   
  #   fext <- file_ext(input$uploadedfile)[1]
  #   
  #   if(fext == "csv"){
  #     df <- read.csv(input$uploadedfile$datapath)
  #   } 
  #   if(fext %in% c("xls","xlsx")){
  #     df <- as.data.frame(read_excel(input$uploadedfile$datapath))
  #   }
  #   
  #   vars <- names(df)
  #   
  #   # Update select input immediately after clicking on the action button. 
  #     updateSelectInput(session, "lat", "Column with latitude:", choices = vars, selected="")
  #     updateSelectInput(session, "long", "Column with longitude:", choices = vars, selected="")
  # 
  # })
  
  
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    mydata <- read.csv(infile$datafile$datapath)
    vars <- names(mydata)

    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "lat", "Column with latitude:", choices = vars, selected="")
    updateSelectInput(session, "long", "Column with longitude:", choices = vars, selected="")

  })

 
}

# Run the application 
shinyApp(ui = ui, server = server)

