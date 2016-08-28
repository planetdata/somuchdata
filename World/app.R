library(ggplot2)
#library(shiny)
library(plotly)
#library(Cairo)   # For nicer ggplot2 output when deployed on Linux

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
tally<-read_excel(path,sheet="Tallyall") # original 
worldtally<- select(tally,c(3,7))
str(worldtally)
names(worldtally)<- c("country_txt","Medal")

#--------------------------------------------------------------------------------------------

# Find a field in the shapefile attributes table that maps to a corresponding field in your data. 
# In this case, it is the field "NAME" in the shapefile maps to the field "country_txt" in your file.
#Create an association between ploygon IDs (stored in the row names of the attribute table), and the "NAME" field.
#Merge the result with your data using CNTRY and country_txt.
#Merge the result of that with the data frame created using the fortify(map) - this associates ploygons with medals.

world<-readOGR(dsn = 'C:/Users/Amita/Documents/RProjects/somuchdata/World', layer = 'ne_10m_admin_0_countries')
countries <- world@data
countries <- cbind(id=rownames(countries),countries)
worldtally$country_txt<- tolower(worldtally$country_txt)
countries$NAME<-tolower(countries$NAME)
countries <- merge(countries,worldtally, 
                   by.x="NAME", by.y="country_txt", all.x=T)

map.df <- fortify(world)
map.df <- merge(map.df,countries, by="id")



ui <- fluidPage(
    mainPanel(plotlyOutput("map",height=500,width=1200))
  
)


server <- function(input, output) {
  output$map <- renderPlotly({
     r<- ggplot(map.df, aes(x=long,y=lat,group=group,text = paste("Country:", toupper(map.df$NAME),"|", "Medals:", map.df$Medal,sep='\n'))) +
      geom_polygon(aes(fill=Medal))+
      geom_path(colour="grey50")+
      # scale_fill_gradient(name="Medal Count",low="black", high="pink", limits=c(0,121)) +
        scale_fill_gradientn(name="Medal Count",
                           colours=brewer.pal(9,"YlOrRd"),
                           na.value="white")+
      coord_fixed()+labs(x="",y="") + ggtitle("2016 Olympic Games Medal Count")
    
    
    
    
    ggplotly(r,tooltip=c("text","fill"))
  
  })
  
}


shinyApp(ui, server)