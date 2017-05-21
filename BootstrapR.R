
###################Start###############################

library(MASS) #MASS is the library where Boston data set exists
library(car)  #Used for linear regression function
data("Boston") 
?Boston       
str(Boston)  #looking at the structure of the data
summary(Boston) #performing a casual high level assessment of data

##Split the data set##
set.seed(100) 
index<-sample(1:nrow(Boston), 0.80*nrow(Boston), replace=F)
train<-Boston[index,]
test<-Boston[-index,]

nrow(train)  #contains 404 observations
nrow(test)   #contains 102 observations

##Constructinglinear regression model on train data set##
lin.reg<-lm(medv~., data=train)
summary(lin.reg)  #Getting R-squared of 0.7273

##Validating model on test data set##
predictions<-predict(lin.reg, newdata = test)

##Finding the Mean Square Error of linear regression model##
err_lin.reg<-sqrt((sum((test$medv-predictions)^2))/nrow(test)) #error=4.2513

plot(predictions, test$medv)
abline(0,1)


#implementation of linear regression model with bootsrapped data sets##
library(foreach)  
N<-nrow(train) #Counts the no. of  observations in train sample
iterations<-10 #Counts the no. of samples to be formed

predictions<-foreach(m=1:iterations,.combine=cbind) %do% {  
index<-sample(1:N,N,replace = TRUE)
new_train<-train[index,] #bootstrapped data set
reg<-lm(medv~., data =new_train) #linear regression on bootstrapped sample
predict(reg,newdata=test)  #finding predictions from test data set
}  
predictions<-rowMeans(predictions)  #average of predictions
a01<-sqrt((sum((test$medv-predictions)^2))/nrow(test)) #error=4.2311


## Additional ##
index<-sample(1:404,404,replace=TRUE) 
train<-train[index,]

head(train,20)
