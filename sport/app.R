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


path<-file.path("Rio.xlsx")
#path<- file.path("C:","Users","Amita","Downloads","Rio.xlsx")
excel_sheets(path)
sport<- read_excel(path,sheet="Sport")
sport1 <- data.frame(Sport.1<-rownames(sport),sport,row.names=NULL)
ui <- fluidPage(
  
  
  mainPanel(plotlyOutput("sport",height=500,width=1200))
  
)

server<- function(input,output){
  
  output$sport <- renderPlotly({

o<-ggplot(reorder_by(Sport,~ -Medals,sport1),aes(x=Sport,y=Medals,fill=Medals,text=paste("Medals:", Medals))) + geom_bar(stat="identity") +
  xlab(" ") + ylab(" ") + scale_fill_gradientn(name="Medal Count", colors=brewer.pal(9,"YlOrRd"),na.value = "White") +
# theme(legend.position="top",legend.title=element_blank(),axis.text.x=element_text(angle=90)) 
  theme(legend.position="none",axis.text.x=element_text(angle=90)) +
  ggtitle("Medal Count by Sport")
  

ggplotly(tooltip = c("text","x"))

})
  
}
  
  
  shinyApp(ui=ui,server=server)
