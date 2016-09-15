shinyServer(function(input, output) {
  
  st<- state.x77 # matrix
  
  
  st<- as.data.frame(st) # data frame
  names(st)
  
  colnames(st)[4] <- "Life.Exp"
  colnames(st)[6] <- "HS.Grad"
  st$Density = st$Population * 1000 / st$Area 
  
  
  
  
  
  # list of data sets
  datasetInput <- reactive({
    switch(input$dataset,
           "States" = st
    )
  })
  
  # dependent variable
  output$dv = renderUI({
    selectInput('dv', h5('Dependent Variable'), choices = names(datasetInput())[-c(1,7,8,9)])
  })
  
  # regression formula
#  regFormula <- reactive({
#    as.formula(paste(input$dv,'~','.' ))
#  })

  

  
  # multivariate model
  modelorig <- reactive({
#    lm(regFormula(), data = datasetInput())
    
    lm(paste(input$dv,'~','.' ),data = datasetInput())
  })
  
  #finding the optimal model
  
  
  aicmod<- reactive({
    
    step(modelorig(),direction="backward",trace=0)
  })
    
  
  aicmod1<- aicmod #change
  
  aicnames<- reactive({
    
     names(aicmod1()$coefficients[-1])
 
    
  })
  
  pastevecorig<- reactive({
    
    paste(aicnames() ,'+ ',sep=" ",collapse="")
    
  })
  
  
  pastevec<-reactive({
    
  substr(pastevecorig(),1,nchar(pastevecorig())-3)
    
  })
  
  
  modelnew<- reactive({
    
    lm(paste(input$dv,'~',pastevec()),data = datasetInput())
  })
  
  # create graphics 
  
  # data view 
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  #summary stats
  
 # output$summary <- renderPrint({
    
    
  # summary(cbind(datasetInput()[input$dv],datasetInput()[input$iv1],datasetInput()[input$iv2],datasetInput()[input$iv3],datasetInput()[input$iv4]))
  #})
  
  #  model
 # output$model <- renderPrint({
 #   summary(model())
#  })
  
  
  #  model
  # output$model <- renderPrint({
  #  summary(aicmod())
  #    })
   

  
  #model
  
  output$model <- renderPrint({
    summary(modelnew())
  })
  
  # residuals
  output$residuals_scaleloc <- renderPlot({
    plot(modelnew(),3)
  })
  
  output$residuals_scatter <- renderPlot({
    
    plot(modelnew(),1)
    
  })
  
  output$residuals_qqline <- renderPlot({
    
    plot(modelnew(),2)
    
  })
  
  output$residuals_residlev <- renderPlot({
    
    plot(modelnew(),5)
    
  })
  
  output$predView <- renderPrint({

     cbind("observed values:"= datasetInput()[input$dv],"predicted values:"= round(predict(modelnew(),newdata= datasetInput()),2))

  })
  
  
})