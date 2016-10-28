#0.78469



# read in data
dat<-read.csv("C:/Users/Amita/Downloads/train (1).csv",header=T,sep=",",na.strings = c(""))



# replace the missing values in 'dat' - Age and Embarked.

dat[is.na(dat$Age),][6]<- mean(dat$Age,na.rm=T)
dat[is.na(dat$Embarked),][["Embarked"]]<-"S"

# process the 'Name' column.
# check for all the unique titles 

unique <- gsub(".*?,\\s(.*?)\\..*$","\\1",dat$Name)
dat$unique<- unique
dat$unique[dat$unique %in% c("Mlle","Mme")] <-"Mlle"
dat$unique[dat$unique %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
dat$unique[dat$unique %in% c('Dona', 'Lady', 'the Countess','Jonkheer')] <- 'Lady'

table(dat$unique)

# passenger's title 
dat$unique <- factor(dat$unique) 
#
# Build a family
dat$familysize <- dat$SibSp + dat$Parch + 1

# Check family size - train
pass_names <- dat$Name
pass_names


extractsurname <- function(x){
  
  if(grepl(".*?,\\s.*?",x)){
    
    gsub("^(.*?),\\s.*?$","\\1",x)
  }
}

surnames <- vapply(pass_names, FUN=extractsurname,FUN.VALUE = character(1),USE.NAMES = F)
surnames

fam<-paste(as.character(dat$familysize),surnames,sep=" ")
fam

famsize<- function(x){
  if(substr(x,1,2) > 2){
    
    x <- "Big"
  }else{
    x <- "Small"
  }
}

unit <- vapply(fam, FUN=famsize,FUN.VALUE = character(1),USE.NAMES = F)
unit
dat$unit <- unit

# designate Cabins
#dat$Cabin <- substr(dat$Cabin,1,1)
#dat$Cabin[dat$Cabin %in% c("F","G","T",NA)] <- "X"
#dat$Cabin<- factor(dat$Cabin)


# Child ?
dat$ischild <- factor(ifelse(dat$Age<=16,"Child","Adult"))

# Mother?
dat$isMother<- "Not Mother"
dat$isMother[dat$Sex=="female" & dat$Parch>0 & unique!="Miss"] <- "Mother"
dat$isMother<- factor(dat$isMother)



#dat$Survived <- factor(ifelse(dat$Survived==1,"yes","no"))
#dat$Survived <- factor(dat$Survived)

dat$unit <- factor(dat$unit)
dat <- dat[,-c(4,9,11)] # remove Names,ticket and cabin



# creating the split between train and test 

n <- nrow(dat)
shuffle <- dat[sample(n),]

traindat <- shuffle[1:round(0.8*n),]
testdat <- shuffle[(round(0.8*n)+1):n,]
#dim(traindat)
#dim(testdat)


label <- factor(dat$Survived)

testdat_predict <- testdat[,-2]
testdat_survive_compare <- testdat[,2]


library(h2o)

localH2o<- h2o.init(max_mem_size = '6g', nthreads = 8 )

traindat_h2o <- as.h2o(traindat)
testdat_h2o <- as.h2o(testdat)
validate <- as.h2o(testdat_predict)
test_h2o <- as.h2o(test)



modgbm<- h2o.gbm(x=c(1,3:14), # x = include all columns minus the response column
             y= 2, #y = response column
             
             training_frame = traindat_h2o, 
             validation_frame = testdat_h2o,
             ntrees=600,
             learn_rate = 0.05,
             max_depth=4)


pred_gbm<-predict(modgbm, validate)

pred_gbm <- as.matrix(ifelse(pred_gbm>0.5,1,0))

score_gbm=mean(abs(pred_gbm-testdat_survive_compare)) #1101.466
score_gbm  
#0.1966292




## glm

          
modglm<- h2o.glm(x=c(1,3:14), # x = include all columns minus the response column
                           y= 2, #y = response column
                           
                           
          training_frame = traindat_h2o, 
          validation_frame = testdat_h2o,
          
          family = "binomial",lambda_search = TRUE,
          nlambda = 10,
          alpha = c(0, 0.25, 0.5, 0.75, 1))




pred_glm<-predict(modglm, validate)

pred_glm <- as.matrix(ifelse(pred_glm$predict>0.5,1,0))

score_glm=mean(abs(pred_glm-testdat_survive_compare)) 
score_glm #0.1797753




#rf

modrf =
  h2o.randomForest(x = c(1,3:14),  # column numbers for predictors
                   y = 2,   # column number for label
                   training_frame = traindat_h2o, 
                   validation_frame = testdat_h2o,
                   
                   ntrees = 5)





pred_rf<-predict(modrf, validate)

pred_rf <- as.matrix(ifelse(pred_rf>0.5,1,0))

score_rf=mean(abs(pred_rf-testdat_survive_compare)) 
score_rf #0.2247191



#deep learning


moddl <- h2o.deeplearning(x = c(1,3:14),  # column numbers for predictors
                          y = 2,   # column number for label
                          training_frame = traindat_h2o, 
                          validation_frame = testdat_h2o,
                          
                          hidden=c(100,100),
                          epochs = 25)




pred_dl<-predict(moddl, validate)

pred_dl <- as.matrix(ifelse(pred_dl>0.5,1,0))

score_dl=mean(abs(pred_dl-testdat_survive_compare)) 
score_dl #0.2134831







pred_ensemble <- ( pred_glm + pred_gbm)/2
score_ensemble=mean(abs(pred_ensemble-testdat_survive_compare))
score_ensemble


pred_glm_test <- as.matrix(predict(modglm,test_h2o ))
pred_gbm_test <- as.matrix(predict(modgbm,test_h2o))


pred_glm_test <- as.data.frame(pred_glm_test[,1])
pred_gbm_test <- as.data.frame(pred_gbm_test)

pred_ensemble_test <- (pred_glm_test + prediction)/2
pred_ensemble_test<- ifelse(pred_ensemble_test > 0.5,1,0)



#kaggle

submit <- data.frame(PassengerId = test$PassengerId, Survived = pred_ensemble_test)
write.csv(submit, file = "submissionen.csv", row.names = FALSE)





# TEST DATA : check for all the unique titles 
test <- read.csv("C:/Users/Amita/Downloads/test (1).csv",header=T,sep=",",na.strings = c("")) 


test[is.na(test$Age),][5]<- mean(test$Age,na.rm=T) 

test[is.na(test$Fare),][["Fare"]] <- mean(test$Fare,na.rm=TRUE)




unique1 <- gsub(".*?,\\s(.*?)\\..*$","\\1",test$Name)
test$unique<- unique1
test$unique[test$unique %in% c("Mlle","Mme")] <-"Mlle"
test$unique[test$unique %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
test$unique[test$unique %in% c('Dona', 'Lady', 'the Countess','Jonkheer')] <- 'Lady'

table(test$unique)

# passenger's title 
test$unique <- factor(test$unique) 
#
# Build a family
test$familysize <- test$SibSp + test$Parch + 1

# Check family size - train
pass_names_test <- test$Name
pass_names_test


extractsurname <- function(x){
  
  if(grepl(".*?,\\s.*?",x)){
    
    gsub("^(.*?),\\s.*?$","\\1",x)
  }
}

surnames_test <- vapply(pass_names_test, FUN=extractsurname,FUN.VALUE = character(1),USE.NAMES = F)
surnames_test

#table(surnames)

fam_test<-paste(as.character(test$familysize),surnames_test,sep=" ")
fam_test

famsize<- function(x){
  if(substr(x,1,2) > 2){
    
    x <- "Big"
  }else{
    x <- "Small"
  }
}

unit_test<- vapply(fam_test, FUN=famsize,FUN.VALUE = character(1),USE.NAMES = F)
unit_test
test$unit <- unit_test
test$unit<-factor(test$unit)

#designate Cabin
#test$Cabin <- substr(test$Cabin,1,1)
#test$Cabin[test$Cabin %in% c("F","G","T",NA)] <- "X"
#test$Cabin<- factor(test$Cabin)


#child?
test$ischild <- factor(ifelse(test$Age<=16,"Child","Adult"))

#Mother?
test$isMother<- "Not Mother"
test$isMother[test$Sex=="female" & test$Parch>0 & test$unique!="Miss"] <- "Mother"
test$isMother<- factor(test$isMother)



str(test)
test<- test[,-c(3,8,10)] # remove Names and cabin


#--------------------------------------------------------------------------------------------------------------

