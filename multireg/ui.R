
shinyUI(fluidPage(
   titlePanel("Multivariate Regression"),

  sidebarLayout(
    sidebarPanel(      
      selectInput("dataset", h5("Choose a dataset:"), choices = c("States")),        
      HTML('</br>'),
      uiOutput('dv'),    
      HTML('</br>')),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Data",
                           HTML("</br>Select a data set form the dropdown menu on your left'</br> </br>"),
                           numericInput("obs", label = h5("Enter the number of observations to view"), 25),
                           tableOutput("view")),
                  
                  
                  tabPanel("The Model Summary",                   
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
