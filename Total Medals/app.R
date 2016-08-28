

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
library(plotly)


path<- file.path("Rio.xlsx")
tally<-read_excel(path,sheet="Tallyall") # original 

tally1<- select(tally,c(3:6))
str(tally1)

tallytidy<- gather(tally1,Medal,Value,-Country)
remove<-which(tallytidy$Value=="0")
tallytidy<- tallytidy[-remove,]
tallytidy$Medal <- factor(tallytidy$Medal,levels = c("Bronze", "Silver", "Gold"),labels=c("Bronze", "Silver", "Gold"),ordered=TRUE)
tallytidy1 <-data.frame(Country=rownames(tallytidy), tallytidy, row.names=NULL)
names(tallytidy1) <- c("country","Country","Medal","Count")

ui<-fluidPage(

  
 mainPanel(plotlyOutput("totalmedal",height = 500, width = 1200))
  
)

server<- function(input,output){
  
  output$totalmedal <- renderPlotly({
    
    q<-ggplot(reorder_by(Country, ~ -Count, tallytidy1,sum),aes(x=Country,y=Count,fill=Medal,text = paste("Count:", Count))) + 
      
      geom_bar(stat="identity",position="stack") + 
      xlab(" ") + ylab(" ") + ggtitle("2016 Olympic Games Medal Count") +
      # geom_text(aes(label=Value),position="stack",vjust=0.8,hjust=0.5,fontface="bold",size=2) +
      scale_fill_manual(values=c("goldenrod2","lightyellow3","gold")) +
      theme(legend.position="top",legend.title=element_blank(),axis.text.x=element_text(angle=90)) 
    

    ggplotly(tooltip = c("x", "fill","text")) 
    
    
    
  })
}


shinyApp(ui = ui, server = server)





#plotly_POST(q)

#p <- plotly_build(q)
#str(p)



