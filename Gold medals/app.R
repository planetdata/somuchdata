library(ggplot2)
library(shiny)
library(plotly)
library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(readxl)
library(dplyr)
library(maps)
library(mapdata)
library(ggmap)
library(stringi)  # string manipulation
library(grid)     # for 'unit'
library(scales)   # for 'percent'
library(ggthemes) # theme_map
library(httr)     # getting data
library(rgdal)
library(devtools)
library(plotflow)


#path<- file.path("C:","Users","Amita","Downloads","Rio.xlsx")
path<- file.path("Rio.xlsx")
gold<-read_excel(path,sheet="Gold")
gold1<- gold[-(which(gold$Gold==0)),]
gold2 <-data.frame(Country=rownames(gold1), gold1, row.names=NULL)
names(gold2)<- c("Country1","Country","Gold")
ui <- fluidPage(
    mainPanel(plotlyOutput("map",height=500,width=1200))
    )


server <- function(input, output) {
  output$map <- renderPlotly({
    
    
    #gplot<-ggplot(reorder_by(Country.1, ~ Gold, gold2), aes(x=Country.1, y=Gold)) # for smallest to largest
    gplot<-ggplot(reorder_by(Country, ~ -Gold, gold2), aes(x=Country, y=Gold))+ 
      geom_bar(stat="identity",fill="gold1") + ylab(" ") + xlab(" ")+
      ggtitle("The Gold Rush")+
      theme(legend.position="top",legend.title=element_blank(),axis.text.x=element_text(angle=90))  
    
    ggplotly(gplot)

  })
  
}


shinyApp(ui, server)