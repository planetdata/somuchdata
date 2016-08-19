#Dataset: Electric power consumption [20Mb]
#Description: Measurements of electric power consumption in one household with a one-minute sampling rate over a period of almost 4 years. 
#             Different electrical quantities and some sub-metering values are available.
#Goal: To examine how household energy usage varies over a 2-day period in February, 2007

install.packages("ggplot2")
install.packages("gdata")
install.packages("tidyr")
install.packages("lubridate")
install.packages("readr")
install.packages("stringr")
install.packages("readxl")
install.packages("dplyr")


library(ggplot2)
library(gdata)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(readxl)
library(dplyr)

rm(power)

path<- file.path("C:","Users","Amita","Downloads","power.txt")
power<-read.delim(path,header=TRUE,sep=";")
dim(power) #[1] 2075259       9
str(power)
head(power)

power$Date<-strptime(power$Date,"%d/%m/%Y")
#power<-unite(power,Timestamp,Date,Time,sep=" ")


View(power)
#power$Date <- as.POSIXlt(power$Timestamp) 
#names(chat5$timestamp) :#"sec"   "min"   "hour"  "mday"  "mon"   "year"  
# "wday"  "yday"  "isdst"

Year<-substr(power$Date,1,4)
Month<-substr(power$Date,6,7)


#subset on year 2007
power$Year<-Year

power2007<- subset(power,Year=="2007")


# select data for 2 days in Feb,2007

powerFeb2007<- power2007[power2007$Date %in% c("2007-02-01","2007-02-02"),]

pow<-select(powerFeb2007,1:9)
View(pow)
dim(pow)    #[1] 2880   10
str(pow)

pow$Global_active_power <- as.numeric(pow$Global_active_power)
 

# HISTOGRAM

ggplot(pow,aes(x=Global_active_power)) + geom_histogram(aes(fill=..count..)) + guides(fill=FALSE) +
ggtitle("Global Active Power") + xlab("Global Active Power (kilowatts)") + ylab("Frequency") +
  theme(plot.title=element_text(face="italic")) +
  scale_fill_gradient(low="midnightblue",high="aquamarine4")
  

#power$Date <- as.POSIXlt(power$Timestamp) 
#names(chat5$timestamp) :#"sec"   "min"   "hour"  "mday"  "mon"   "year"  
# "wday"  "yday"  "isdst"

#pow$Day<-weekdays(pow$Date)






# overlapping plots with custom legend

datetime<- strptime(paste(pow$Date, pow$Time, sep=" "), "%Y-%m-%d %H:%M:%S") 
pow$datetime<-datetime
plot(datetime, pow$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")




pow$Sub_metering_1<-as.numeric(pow$Sub_metering_1)
pow$Sub_metering_2<-as.numeric(pow$Sub_metering_2)
pow$Sub_metering_3<-as.numeric(pow$Sub_metering_3)


plot(datetime,pow$Sub_metering_1,type="l",ylab="Energy Submetering",xlab="")
lines(datetime,pow$Sub_metering_2,type="l",col="red")
lines(datetime,pow$Sub_metering_3,type="l",col="green")
legend("topright",c("sub_metering_1","sub_metering_2","sub_metering_3"),lty=1,lwd=2.5,col=c("black","red","green"),cex=0.5)




# layouts 

par(mfrow=c(2,2)) # 4 plots in one layout

pow$Volatage<-is.numeric(pow$Voltage)


#1.
plot(datetime,pow$Global_active_power,type="l",ylab="Global Active Power",xlab="")

#2.
plot(datetime,pow$Voltage,type='l',ylab="Voltage",xlab="datetime")

#3.
plot(datetime,pow$Sub_metering_1,type="l",ylab="Energy sub metering",xlab="")
lines(datetime,pow$Sub_metering_2,type="l",col="red")
lines(datetime,pow$Sub_metering_3,type="l",col="blue")
legend("topright",c("sub_metering_1","sub_metering_2","sub_metering_3"),lty=1,lwd=2.5,cex=0.5,col=c("black","red","blue"))

#4.
plot(datetime,pow$Global_reactive_power,type="l",xlab="datetime",ylab="Global_Reactive_Power")


