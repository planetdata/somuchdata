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


power<-unite(power,Timestamp,Date,Time,sep=" ")
head(power)

