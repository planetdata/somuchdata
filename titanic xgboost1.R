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




label <- dat$Survived

dat <- dat[,-2]



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





combi <- rbind(dat,test)






# create a sparse matrix  to dummify the categorical variables
require(Matrix)
data_sparse <- sparse.model.matrix(~.-1, data = as.data.frame(combi))
cat("Data size: ", data_sparse@Dim[1], " x ", data_sparse@Dim[2], "  \n", sep = "")
#data_sparse@Dim = 313864   1075
gc(verbose = FALSE)

dtrain <- xgb.DMatrix(data = data_sparse[1:nrow(dat), ], label = label) # DTRAIN :Create design matrix without intercept
gc(verbose = FALSE)


dtest <- xgb.DMatrix(data = data_sparse[(nrow(dat)+1):nrow(combi), ]) # DTEST : Create design matrix without intercept
gc(verbose = F)






set.seed(12345678)
temp_model <- xgb.cv(data = dtrain,
                     nthread = 8,
                     nfold = 4,
                     #nrounds = 2, # quick test
                     nrounds = 1000000,
                     max_depth = 6,
                     eta = 0.0404096, 
                     subsample = 0.70,
                     colsample_bytree = 0.70,
                     booster = "gbtree",
                     eval_metric = "error",
                     maximize = FALSE,
                     early_stopping_rounds = 25,
                     objective = "reg:logistic",
                     print_every_n = 10,
                     verbose = TRUE)

gc(verbose = FALSE)

# when MAE throws error 
#install.packages("drat", repos="https://cran.rstudio.com")
#drat:::addRepo("dmlc")
#install.packages("xgboost", type = "source")


set.seed(12345678)
temp_model <- xgb.train(data = dtrain,
                        nthread = 8,
                        nrounds = floor(temp_model$best_iteration*1.25),
                        max_depth = 6,
                        eta =  0.0404096,
                        subsample = 0.70,
                        colsample_bytree = 0.70,
                        booster = "gbtree",
                        eval_metric = "error",
                        maximize = FALSE,
                        objective = "reg:logistic",
                        print_every_n = 10,
                        verbose = TRUE,
                        watchlist = list(trainrep = dtrain))



prediction <- predict(temp_model,dtest)

prediction <- ifelse(prediction>0.5,1,0)




# variable importance

importance <- xgb.importance(feature_names = data_sparse@Dimnames[[2]], model = temp_model)  #Grab all important features
xgb.plot.importance(importance)  #Plot 



#kaggle

submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = "submissionxgboost.csv", row.names = FALSE)




# find variable importance 

install.packages("DT")
importance <- xgb.importance(feature_names = data_sparse@Dimnames[[2]], model = temp_model)


datatable(as.data.frame(importance),
          filter = "top",
          class = "cell-border stripe",
          options = list(pageLength = 20,
                         lengthMenu = c(5, 10, 15, 20, 25, 50, 100, 500))
) %>% formatStyle('Gain',
                  background = styleColorBar(range(importance$Gain, na.rm = TRUE, finite = TRUE), 'lightgreen'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
  formatStyle('Cover',
              background = styleColorBar(range(importance$Cover, na.rm = TRUE, finite = TRUE), 'lightblue'),
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>%
  formatStyle('Frequency',
              background = styleColorBar(range(importance$Frequency, na.rm = TRUE, finite = TRUE), 'lightgrey'),
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>%
  formatPercentage(columns = c("Gain"),
                   digits = 6) %>%
  formatPercentage(columns = c("Cover"),
                   digits = 6) %>%
  formatPercentage(columns = c("Frequency"),
                   digits = 6)