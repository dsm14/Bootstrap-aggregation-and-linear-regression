# Bootstrap-aggregation-and-linear-regression
An experiment to find out whether bootstapping improves the prediction results of a linear regression model
How bagging actually improves the accuracy of a model?
When many samples are introduced and then averaged together, what happens is that the variance among the data points gets lowered without affecting the bias. So this thing helps avoid overfitting, stabilize predictions and increase the accuracy of the model. 
So bagging works best when the model instances are very different from each other, since averaging effect is minimal when the outputs are already close. Hence, the most effective base models for bagging are unstable models, where small changes in the training set can lead to large changes in model parameters. The regression trees works best with bootstrapping but accuracy can still be improved for linear regression models where model instances do not vary that much and this is what I am going to exactly demonstrate in this post.

The focus is to demonstrate the use of bootstrapping here so for sake convenience, I will be using an inbuilt data set in R so that I do not need to perform any data major exploration or preparation. 


I am using the “Boston”, inbuilt housing price data set present in MASS library in R. This data set contains the information about housing values in suburbs of Boston in late 1970s. The data frame has 506 rows and 14 columns with median value of house labelled as medv is the continuous target variable and other 13 continuous variables as predictors. Goal is to find an effective predictive model where median value of house is a function of various predictors.


All the predictors are continuous here and doing a quick assessment revealed no anomalies in data, so there is no such major data preparation to do here as the primary focus here is to compare the prediction accuracy of linear regression model v/s prediction accuracy of bootstrapped linear regression model.

Before finding the prediction of each of the models, let’s divide the data set into training and test samples. To avoid overfitting the model is always built on training set while it is validated on future data set which is the test here. I am dividing the Boston data set into train and test samples, containing 80% and 20% of the whole data respectively. 

##Loading the data##
library(MASS) #MASS is the library where Boston data set exists
library(car)  #Used for linear regression function
data("Boston") 


##Split the data set##
set.seed(100) 
index<-sample(1:nrow(Boston), 0.80*nrow(Boston), replace=F)
train<-Boston[index,]
test<-Boston[-index,]

With train and test samples formed, let’s first construct the linear regression model on train data set. We get R squared value as 0.7273, explaining how well the variation in Y is captured by the X variables.
##Constructinglinear regression model on train data set##
lin.reg<-lm(medv~., data=train)
summary(lin.reg)  #Getting R-squared of 0.7273

Now let’s validate the constructed model on test data set to see how well the model performs. The performance can be measured by finding the error or to be specific the mean square error. The error for linear regression model came out to be 4.2513.

##Validating model on test data set##
predictions<-predict(lin.reg, newdata = test)

##Finding the Mean Square Error of linear regression model##
err_lin.reg<-sqrt((sum((test$medv-predictions)^2))/nrow(test)) #error=4.2513

Our first part of our objective is over where we have found the error for the linear regression model. In second part of our objective, we will now find the error for bootstrapped model and compare both the errors to see which one performs better.

Let’s begin with bootstrapping then. For details please refer to my aticle: http://datasciencemojo.com/does-bagging-work-well-for-linear-regression-models/

#implementation of linear regression model with bootsrapped data sets##
library(foreach)  
N<-nrow(train) #Counts the no. of  observations in train sample
iterations<-450 #Counts the no. of samples to be formed

predictions<-foreach(m=1:iterations,.combine=cbind) %do% {  
index<-sample(1:N,N,replace = TRUE)
new_train<-train[index,] #bootstrapped data set
reg<-lm(medv~., data =new_train) #linear regression on bootstrapped sample
predict(reg,newdata=test)  #finding predictions from test data set
}  
predictions<-rowMeans(predictions)  #average of predictions
err_boot<-sqrt((sum((test$medv-predictions)^2))/nrow(test)) #error=4.2311


The error for linear regression model which we earlier found out was 4.2513. The error for bootstrapping model came out to be 4.2311. And lesser the error, means better is the prediction capability of the model. So we see using bootstrapping with linear regression model has the capability to reduce the error and improve the prediction capability. 


