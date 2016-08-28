

install.packages("ggplot2")
install.packages("gdata")
install.packages("tidyr")
install.packages("lubridate")
install.packages("readr")
install.packages("stringr")
install.packages("readxl")
install.packages("dplyr")
install.packages("googleVis")

install.packages("sp")
install.packages("raster")
install.packages("rasterVis")
install.packages("maptools")
install.packages("rgeos")

library(ggplot2)
library(gdata)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(readxl)
library(dplyr)
library(plotly)
install.packages("plotly")
library(googleVis)


library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)



library(gridExtra)
library(grid)


path<- file.path("C:","Users","Amita","Downloads","rio.xlsx")
 
excel_sheets(path)

rio<-read_excel(path,sheet=1)


rio1<-select(rio,Country,Gold,Silver,Bronze)

riotidy<- gather(rio1,Medal,value,-Country)

View(riotidy)


remove<-which(riotidy$value=="0")
riotidy1<- riotidy[-remove,]

View(riotidy1)

#ggplot(riotidy,aes(x=Country,y=Count,fill=Count)) + geom_bar(stat="identity") +
 #  facet_grid(Medals~.) + theme(axis.text.x = element_text(angle=90, )) 






ggplot(riotidy1,aes(x=Country,y=value,fill=Medal)) + geom_bar(stat="identity",position="stack") + 
    scale_fill_manual(values=c("goldenrod2","gold","lightyellow3")) + ylab(" ") + xlab(" ") +
   ggtitle("2016 Olympic Games Medal Count") +
 geom_text(aes(label=value),position="stack",size=2,fontface = "bold", hjust = 0.5,vjust=0.8) + 
   
   theme_minimal() + theme(legend.position="top") +
   guides(fill=guide_legend(title=NULL)) + theme(axis.text.x = element_text(angle=90)) 

makeFootnote <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(3, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}
   

footnote <-"SOURCE: www.rio2016.com.All results as of 12:00 PM AEST , August 20th,2016.\n Created using RStudio by Amita Singh"
makeFootnote(footnote)
                         

