install.packages("ggplot2")
install.packages("gdata")
install.packages("tidyr")
install.packages("lubridate")
install.packages("readr")
install.packages("stringr")
install.packages("readxl")
install.packages("dplyr")

# changes here 
library(ggplot2)
library(gdata)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(readxl)
library(dplyr)

rm(chat)
rm(chat1)
rm(chat2)
rm(chat3)
rm(chat4)
rm(chat5)


path<- file.path("C:","Users","Amita","Downloads","chat.csv")
chat<- read.csv(path,stringsAsFactors = FALSE,header=FALSE) 
chat$V1 <- dmy(chat$V1)
chat<- separate(chat,V1,c("Year","Month","Day"))
chat$V4<- str_replace_all(chat$V4,"You","Amita")
remove<- which(is.na(chat))
chat1<- chat[-remove,]
chat2<- chat1[,c(1:6)]
names(chat2)<- c("Year","Month","Day","Time","AM/PM","Person")
chat2$Person<-gsub("[^[:alnum:][:blank:]+?&/\\]", "", chat2$Person)
chat2$Person<-gsub("['â'|'Â']", "", chat2$Person)
chat2$`AM/PM`<- gsub(":","",chat2$`AM/PM`)

#View(chat2)
remove1<-which(chat2$Person=="Messages")
chat2<-chat2[-remove1,] 
chat2$Person<-str_replace_all(chat2$Person,"^.*2623949$","Person1")
chat2$Person<-str_replace_all(chat2$Person,"^.*7757992$","Person2")
chat2$Person<-str_replace_all(chat2$Person,"^.*53622$","Person3")
chat2$Person<-str_replace_all(chat2$Person,"^.*36992$","Person4")
chat2$Person<-str_replace_all(chat2$Person,"^.*48368$","Person5")
chat2$Person<-str_replace_all(chat2$Person,"^.*67384$","Person6")
chat2$Person<-str_replace_all(chat2$Person,"Amita","Me")
chat2$Person<-str_replace_all(chat2$Person,"Saswati","Person8")


# plotting monthly data across the years

chat3<- unite(chat2,time,Time,`AM/PM`,sep=" ")
chat3$time <- substr(strptime(chat3$time,"%H:%M"),12,16)


ggplot(chat3, aes(x = Month, fill = Person)) +
  stat_count(position = "dodge", show.legend = TRUE) +
  ggtitle("Whatsapp group Traffic") +
  ylab("# of messages") + xlab("month") +
  theme(plot.title = element_text(face = "italic")) + facet_grid(Year~.)

# Number of messages during AM/PM,across months facetted year
chat4<- chat2

chat4$row <- 1:nrow(chat4) # add a unique identifier

ggplot(chat4,aes(x=Month,fill=factor(`AM/PM`))) + 
  stat_count(position = "dodge", show.legend = TRUE) +
  ggtitle("Whatsapp Group conversations by Month") +
  ylab("# of messages") + xlab("month") +
  theme(plot.title = element_text(face = "italic")) + facet_grid(Year~.) + 
  guides(fill=guide_legend(title="AM/PM"))


# Number of messages across hours of the day , facetted by AM/PM


chat5<- select(chat,c(1:6))
names(chat5)<- c("Year","Month","Day","Time","AM/PM","Person")
chat5$`AM/PM`<-gsub(":$","",chat5$`AM/PM`)
chat5$Person<-gsub("[^[:alnum:][:blank:]+?&/\\]","",chat5$Person)
chat5$Person<-gsub("['â'|'Â']","",chat5$Person)
chat5$Person<-str_replace_all(chat5$Person,"^.*2623949$","Person1")
chat5$Person<-str_replace_all(chat5$Person,"^.*7757992$","Person2")
chat5$Person<-str_replace_all(chat5$Person,"^.*53622$","Person3")
chat5$Person<-str_replace_all(chat5$Person,"^.*36992$","Person4")
chat5$Person<-str_replace_all(chat5$Person,"^.*48368$","Person5")
chat5$Person<-str_replace_all(chat5$Person,"^.*67384$","Person6")
chat5$Person<-str_replace_all(chat5$Person,"Amita","Me")
chat5$Person<-str_replace_all(chat5$Person,"Saswati","Person8")
removena<- which(is.na(chat5))
chat5<- chat5[-remove,]
remove2<-which(chat5$Person=="Messages")
chat5<-chat5[-remove2,] 
chat5$Time <- substr(strptime(chat5$Time,"%I:%M:%S"),12,19)
chat5<-unite(chat5,Date,Year,Month,Day,sep="-")
chat5<- unite(chat5,timestamp,Date,Time,sep=" ")
chat5$timestamp<- as.POSIXlt(chat5$timestamp)
#names(chat5$timestamp) :#"sec"   "min"   "hour"  "mday"  "mon"   "year"  
# "wday"  "yday"  "isdst"
chat5$Hour <- chat5$timestamp$hour

ggplot(chat5,aes(x=Hour,fill=Person)) + stat_count(position="dodge", show.legend=TRUE) + 
  ggtitle("Whatsapp group conversations per hour") +
  ylab("# of messages") + xlab("Hour") +
  theme(plot.title = element_text(face = "italic")) + facet_grid(`AM/PM`~.)



