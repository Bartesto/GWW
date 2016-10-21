
################################################################################
# Code for visualisation and modelling GWW fire history
#
# by Bart Huntley

wkdir <- "Z:\\DEC\\GreatWesternWoodland\\Working\\Fire_mapping_2016\\R_analysis"
library(dplyr)
library(tidyr)

setwd(wkdir)

files <- list.files(pattern = "*.csv")

#YSLB data
data7 <- read.csv(files[7], header = TRUE, stringsAsFactors = FALSE)
y <- arrange(data7, sites)
y <- y[,-1]
#names(data7)[1] <- "ynames"

#b1
data1 <- read.csv(files[1], header = TRUE, stringsAsFactors = FALSE)
data1 <- data1[,-1]

data2 <- gather(data1, site, index, 2:3210)

data3 <- spread(data2, ynames, index)

#b2
datab2.1 <- read.csv(files[2], header = TRUE, stringsAsFactors = FALSE)
datab2.1 <- datab2.1[,-1]
datab2.2 <- gather(datab2.1, site, index, 2:3210)
datab2.3 <- spread(datab2.2, ynames, index)

#b3
datab3.1 <- read.csv(files[3], header = TRUE, stringsAsFactors = FALSE)
datab3.1 <- datab3.1[,-1]
datab3.2 <- gather(datab3.1, site, index, 2:3210)
datab3.3 <- spread(datab3.2, ynames, index)

#b4
datab4.1 <- read.csv(files[4], header = TRUE, stringsAsFactors = FALSE)
datab4.1 <- datab4.1[,-1]
datab4.2 <- gather(datab4.1, site, index, 2:3210)
datab4.3 <- spread(datab4.2, ynames, index)

#b5
datab5.1 <- read.csv(files[5], header = TRUE, stringsAsFactors = FALSE)
datab5.1 <- datab5.1[,-1]
datab5.2 <- gather(datab5.1, site, index, 2:3210)
datab5.3 <- spread(datab5.2, ynames, index)

#b6
datab6.1 <- read.csv(files[6], header = TRUE, stringsAsFactors = FALSE)
datab6.1 <- datab6.1[,-1]
datab6.2 <- gather(datab6.1, site, index, 2:3210)
datab6.3 <- spread(datab6.2, ynames, index)

#All sat data plus YSLB data (becomes outcome - "out")
data <- cbind(y, data3, datab2.3[,-1], datab3.3[,-1],datab4.3[,-1],datab5.3[,-1],datab6.3[,-1])
names(data)[1] <- "out"
dataALL <- data[,-2]

################################################################################
#Feature Selection
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)

#Sat data without sites
x <- cbind(data3, datab2.3[,-1], datab3.3[,-1],datab4.3[,-1],datab5.3[,-1],datab6.3[,-1])
x <- x[,-1]

normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)
subsets <- c(1:5, 10, 15, 20, 25)

set.seed(10)
ctrl <- rfeControl(functions = lmFuncs,
                 method = "repeatedcv",
                 repeats = 5,
                 verbose = FALSE)
lmProfile <- rfe(x, y,
               sizes = subsets,
               rfeControl = ctrl)
lmProfile
predictors(lmProfile)[1:8]

#Reduced data based on feature selection results top 8
dataRED <- data[,predictors(lmProfile)[1:8]]
dataRED <- cbind(y, dataRED)
names(dataRED)[1] <- "out"

#Check NA's
apply(dataRED,2,function(x) sum(is.na(x)))

#Remove NA's
test <- complete.cases(dataRED)
dataREDclean <- dataRED[test,]

apply(dataREDclean,2,function(x) sum(is.na(x)))

#Data split and simple regression model
index <- sample(1:nrow(dataREDclean),round(0.75*nrow(dataREDclean)))
train <- dataREDclean[index,]
test <- dataREDclean[-index,]
lm.fit <- glm(out~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$out)^2)/nrow(test)

#Data scaling (normalising)
maxs <- apply(dataREDclean, 2, max) 
mins <- apply(dataREDclean, 2, min)

scaled <- as.data.frame(scale(dataREDclean, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

#Neural Network Model
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("out ~", paste(n[!n %in% "out"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=6,linear.output=FALSE, act.fct = 'tanh')
plot(nn)

#predicting "out" using the neural network model
pr.nn <- compute(nn,test_[,2:9])

pr.nn_ <- pr.nn$net.result*(max(dataREDclean$out)-min(dataREDclean$out))+min(dataREDclean$out)
test.r <- (test_$out)*(max(dataREDclean$out)-min(dataREDclean$out))+min(dataREDclean$out)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))

par(mfrow=c(1,2))

plot(test$out,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$out,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

plot(test$out,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$out,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

#Using http://datascienceplus.com/fitting-neural-network-in-r/
