install.packages("drat")
require(drat)
drat::addRepo("dmlc")
install.packages("mxnet")
require(mxnet)



# read in the train and the test data
trainorig  <-read.csv("C:/Users/Amita/Downloads/train.csv",header=T,sep=",")
testorig <-  read.csv("C:/Users/Amita/Downloads/test.csv",header=T,sep=",")

testo <-  read.csv("C:/Users/Amita/Downloads/test.csv",header=T,sep=",")


# PLOT THE IMAGES build a 28 * 28 matrix





rotate <- function(x) {
 t(apply(x,2,rev))
}




# single image - row 10


m<- rotate(matrix( unlist(train[10,-1]),nrow=28,byrow = T))
image(m, col=terrain.colors(255))


# plot the first 6 images 

par(mfrow=c(2,3))

plot <- function(x) {
  
  
  m<- rotate(matrix( unlist(train[x,-1]),nrow=28,byrow = T))
  image(m, col=terrain.colors(255))
  
  
}

lapply(1:6,plot)

# reset the plotting layout 
par(mfrow=c(1,1))




n <- nrow(trainorig)
shuffle<- trainorig[sample(n),]
train <- shuffle[1:round(0.8*n),]
test <- shuffle[(round(0.8*n)+1):n,]

dim(train)
dim(test)


train<-data.matrix(train)
test<-data.matrix(test)
train.x<-train[,-1]
train.y<-train[,1]
train.x<-t(train.x/255)

test_org<-test
test<-test[,-1]
test<-t(test/255)
table(train.y)



data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=128)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=64)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
 fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=10)
 softmax <- mx.symbol.SoftmaxOutput(fc3, name="sm")
 devices <- mx.cpu()
 mx.set.seed(0)
 model <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y,
                                        ctx=devices, num.round=10, array.batch.size=100,
                                       learning.rate=0.07, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                      initializer=mx.init.uniform(0.07),
                                     epoch.end.callback=mx.callback.log.train.metric(100))

 
 
 preds <- predict(model, test) 

 pred.label <- max.col(t(preds)) - 1
 
 
 table(test_org[,1],pred.label)
 
  
 sum(diag(table(test_org[,1],pred.label)))/8400   #0.9678571
 
 
 #------------------------------------------------------------------------------------------------------------
 #98.9%
 
 # Convolutional NN
 data <- mx.symbol.Variable('data')
 devices<-mx.cpu()
  # first conv
  conv1 <- mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=20)
  relu1 <- mx.symbol.Activation(data=conv1, act_type="relu")
  pool1 <- mx.symbol.Pooling(data=relu1, pool_type="max",
                                       kernel=c(2,2), stride=c(2,2))
  # second conv
 conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=50)
  relu2 <- mx.symbol.Activation(data=conv2, act_type="relu")
  pool2 <- mx.symbol.Pooling(data=relu2, pool_type="max",
                              kernel=c(2,2), stride=c(2,2))
 # first fullc
   flatten <- mx.symbol.Flatten(data=pool2)
  fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=500)
  relu3 <- mx.symbol.Activation(data=fc1, act_type="relu")
  # second fullc
    fc2 <- mx.symbol.FullyConnected(data=relu3, num_hidden=10)
 # loss
    lenet <- mx.symbol.SoftmaxOutput(data=fc2)
    
    
    
  train.array <- train.x
  dim(train.array) <- c(28, 28, 1, ncol(train.x))
  test.array <- test
  dim(test.array) <- c(28, 28, 1, ncol(test))
  
  
  mx.set.seed(0)
  tic <- proc.time()
  model <- mx.model.FeedForward.create(lenet, X=train.array, y=train.y,
                                    ctx=devices, num.round=20, array.batch.size=100,
                                  learning.rate=0.05, momentum=0.9, wd=0.00001,
                                    eval.metric=mx.metric.accuracy,
                                   epoch.end.callback=mx.callback.log.train.metric(100))
 
 
 
  preds <- predict(model, test.array) 
  
  pred.label <- max.col(t(preds)) - 1
  table(test_org[,1],pred.label)


  
  
  sum(diag(table(test_org[,1],pred.label)))/8400   #0.9884524 with Tahn , 0.9903571 with Relu and maxout
  
  
  
  # predict on the kaggle dataset 
  testorig <- as.matrix(testorig)

  testorig<-t(testorig/255)
  
  testorig.array <- testorig
  dim(testorig.array) <- c(28, 28, 1, ncol(testorig))
 
 
 predtest<-predict(model,testorig.array)
 
 predlabel<-max.col(t(predtest))-1
 
 
 
 
 
 #output test set
 predictions <- data.frame(ImageId=1:nrow(testo), Label=predlabel)
 
 write.csv(predictions, "CNN.csv",row.names=FALSE)
 #0.99086 with Relu
 