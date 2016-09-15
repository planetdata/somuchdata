# Define UI for application 
shinyUI(fluidPage(
  
  # Application title
  
  titlePanel("Multivariate Regression"),
  
  # Sidebar 
  #
  sidebarLayout(
    sidebarPanel(      
      selectInput("dataset", h5("Choose a dataset:"), choices = c("States")),        
      HTML('</br>'),
      uiOutput('dv'),    
      HTML('</br>')),
    
    # main panel 
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Data",
                           HTML("</br>Select a data set from the 'Choose a dataset menu'</br> </br>"),
                           numericInput("obs", label = h5("Number of observations to view"), 10),
                           tableOutput("view")),
                  
    #              tabPanel("Summary Statistics",
    #                       verbatimTextOutput("summary")),
                  
                  
                  tabPanel("Model",                   
                           verbatimTextOutput("model")),
                  
                  
                  tabPanel("Diagnostic plots",                   
                           
                           plotOutput("residuals_scatter"),
                           plotOutput("residuals_qqline"),
                           plotOutput("residuals_scaleloc"),
                           plotOutput("residuals_residlev")),
                  
                  
          
               tabPanel("Predictions",
                           verbatimTextOutput("predView"))

      )                         
    ))
))