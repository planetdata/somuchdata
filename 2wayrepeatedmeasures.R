path<- file.path("C:\\Users\\Amita\\Desktop\\diet.csv.xlsx")
diet <- read_excel(path,col_names=F)
View(diet)
names(diet) <- c("subject","exercise","intensity1","intensity2","intensity3","diet")
# 
# each group has 6 subjects. 
# we are interested to find out the response  pulse rate of each of the subjects
# independent variables  : diet, exercise
# So far, two-factor ANOVA with 3 replicates per combo of exercise- type and diet
# BUT
#But: 3 responses for each subject: pulse rate at low, moderate, high intensity in their exercise.
# Makes it repeated measures.
#3 factors:
#  ???diet and exercise type "between subjects",
# ???intensity-of-exercise "within subjects".


diet<- diet[order(diet$subject),]
#diet1<- select(diet,2:6)
require(dplyr)
View(diet)
#diet1$subject<- 1:nrow(diet1)
str(diet)

diet$exercise<- factor(diet$exercise)
diet$diet<- factor(diet$diet)
diet$subject <- factor(diet$subject)

require(reshape2)

#Convert it to long format
diet.long <- melt(diet, id = c("subject","diet","exercise"), # keep these columns the same
                  measure = c("intensity1","intensity2","intensity3"),       # Put these two columns into a new column
                  variable.name="intensity")                # Name of the new column

View(diet.long)
mod <- aov(value ~ diet*exercise*intensity + Error(subject/intensity) , data=diet.long)
summary(mod)

mean_pulse<-with(diet.long,tapply(value,list(diet,intensity),mean))
mp <- stack(as.data.frame(mean_pulse))
mp$diet <- rep(seq_len(nrow(mean_pulse)),ncol(mean_pulse))
mp$diet<-factor(mp$diet,labels = c("Meat","Veg"))
ggplot(mp,aes(ind,values,group=diet,color=diet)) + geom_line() + xlab("Intensity of the exercise") +
  ylab("Mean Pulse Rate") + ggtitle("Mean Pulse rate - \n Exercise Intensity vs Diet")



mean_pulse1<-with(diet.long,tapply(value,list(diet,intensity,exercise),mean))
mean_pulse1
mp1 <- stack(as.data.frame(mean_pulse1))
mp1<- separate(mp1,ind,c("Intensity","Exercise"))
mp1$Diet<- rep(seq_len(nrow(mean_pulse1)),ncol(mean_pulse1))
mp1$Diet <- factor(mp1$Diet,labels = c("Meat","Veg"))
mp1$Intensity<-factor(mp1$Intensity)
mp1$Exercise<-factor(mp1$Exercise)
ggplot(mp1,aes(Intensity,values,group=Diet,color=Diet)) + geom_line(lwd=1) + xlab("Intensity of the exercise") +
  ylab("Mean Pulse Rate") + ggtitle("Mean Pulse rate - \n Exercise Intensity vs Diet") + theme_grey()+
 facet_grid(Exercise ~.)
