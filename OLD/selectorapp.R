library(shiny)

runApp(list(
  
  ui = bootstrapPage(
    selectInput('dataset', 'Choose data set', c('mtcars', 'iris')),
    selectInput('columns', 'Choose variable', ""),
    selectInput('values', 'Show values', "")
  ),
  
  server = function(input, output, session){
    
    # updates variable names based on selected dataset 
    outVar = reactive({
      names(get(input$dataset))
    })
    
    # i want this to update the values of the selected variable
    outVar2 = reactive({
      if (input$columns == "") return()
      sort(unique(get(input$dataset)[, input$columns]))
    })
    
    # create separate observeEvents to 
    observeEvent(input$dataset, {
      updateSelectInput(session, "columns", choices = outVar())
    })
    
    observeEvent(input$columns, {
      updateSelectInput(session, "values", choices = outVar2())
    })
    
  }
))
