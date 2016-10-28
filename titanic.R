# read in data
dat<-read.csv("C:/Users/Amita/Downloads/train (1).csv",header=T,sep=",",na.strings = c(""))

# visually inspect the data for missing values.
require(Amelia)

missmap(dat)
missmap(test) 


# use sapply function to check for missing values.

sapply(dat,function(x){sum(is.na(x))}) 

sapply(test,function(x){sum(is.na(x))}) 

# replace the missing values in 'dat' - Age and Embarked.

dat[is.na(dat$Age),][6]<- mean(dat$Age,na.rm=T)
dat[is.na(dat$Embarked),][["Embarked"]]<-"S"
summary(dat)

dat <- dat[,-9] # remove ticket


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
dat$Survived <- factor(dat$Survived)

dat$unit <- factor(dat$unit)
dat <- dat[,-c(4,10)] # remove Names and cabin

# split dat into training and test dataset
set.seed(50)
n<- nrow(dat)
shuffled <- dat[sample(n),]

traindat <- shuffled[1:round(0.6*n),]
testdat<- shuffled[(round(0.6*n) + 1):n,]

dim(traindat)
dim(testdat)



#nnet

install.packages("neuralnet")
require(neuralnet)

nnet.model <- train(Survived ~ ., 
                    data=traindat, method="nnet")


plot(nnet.model)
res2 = predict(nnet.model, newdata=testdat[,-2])


conf<- table(testdat$Survived,res2)
accuracy<- sum(diag(conf))/sum(conf)
accuracy #83.14






# random forest
require(caret)
require(ranger)



model <- train(
  Survived ~.,
  tuneLength = 50,
  data = traindat, method ="ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)


plot(model)
pred <- predict(model,newdata=testdat[,-2])
conf<- table(testdat$Survived,pred)
accuracy<- sum(diag(conf))/sum(conf)
accuracy #83.14




#glmnet

myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)


modelglmnet <- train(Survived ~.,data=traindat,method="glmnet",trControl=myControl,tuneGrid=expand.grid(alpha=0:1,lambda=seq(0.0001,0.1,length=10)))



pred <- predict(modelglmnet,newdata=testdat[,-2])
conf<- table(testdat$Survived,pred)
accuracy<- sum(diag(conf))/sum(conf)
accuracy #0.82022


#glm



modelglm <- glm(Survived ~ Pclass+Sex +Age +SibSp +Embarked ,
                
                family=binomial(link='logit'),data=traindat)


Anova(modelglm)


pred <- predict(modelglm,newdata=testdat[,-2],type="response")
pred <- ifelse(pred > 0.5,1,0)


conf<- table(testdat$Survived,pred)
accuracy<- sum(diag(conf))/sum(conf)
accuracy #0.80




#gbm

modelgbm <- train(Survived ~.,data=traindat,method="gbm",trControl=myControl,tuneGrid=expand.grid(.interaction.depth = c(1, 5, 9), .n.trees = (1:15)*100, .shrinkage = 0.1 ,.n.minobsinnode =c(10)))


pred <- predict(modelgbm,newdata=testdat[,-2])
conf<- table(testdat$Survived,pred)
accuracy<- sum(diag(conf))/sum(conf)
accuracy #0.808

#rf

modelrf<- train(Survived ~.,data=traindat,method="rf",trControl=myControl,ntree=2000,tuneLength=50)
pred <- predict(modelrf,newdata=testdat[,-2])

conf<- table(testdat$Survived,pred)
accuracy<- sum(diag(conf))/sum(conf)
accuracy #0.8342





# TEST DATA : check for all the unique titles 
test <- read.csv("C:/Users/Amita/Downloads/test (1).csv",header=T,sep=",",na.strings = c("")) 


test[is.na(test$Age),][5]<- mean(test$Age,na.rm=T) 

test[is.na(test$Fare),][["Fare"]] <- mean(test$Fare,na.rm=TRUE)

test <- test[,-8] # remove ticket

summary(test)


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

test<- test[,-c(3,9)] # remove Names and cabin






#1.

predranger <- predict(model, newdata = test)

predrangernum <- as.numeric(levels(predranger))[predranger]

#2.

predglmnet <- predict(modelglmnet,newdata=test)
predglmnet <- ifelse(predglmnet=="yes",1,0)


#3.

predglm <- predict(modelglm,newdata=test,type="response")
predglm <- ifelse(predglm > 0.5,1,0)

#4. 

predgbm <- predict(modelgbm,newdata=test)
predgbm <- ifelse(predgbm=="yes",1,0)


#5.
predgbm <- predict(modelgbm,newdata=test)
predgbm <- ifelse(predgbm=="yes",1,0)

#6. nnet

prednnet<- predict(nnet.model,newdata=test)
prednnetnum <- as.numeric(levels(prednnet))[prednnet]


allPreds<-cbind(predrangernum,prednnetnum)



vote<-function(dat){ # take in a df
  f<-function(mat){     
    a<-table(mat)
    as.numeric(names(which.max(a)))
  }
  apply(dat,1,f)  # dat is the matrix, 1 is the application of the function over the rows, f is the function
}



testPredictions<-vote(allPreds)









#kaggle

submit <- data.frame(PassengerId = test$PassengerId, Survived = testPredictions)
write.csv(submit, file = "submissionensemble.csv", row.names = FALSE)


