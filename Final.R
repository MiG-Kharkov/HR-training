# Install packages if we need it 
if (!require("corrplot")) install.packages("corrplot")
if (!require("ggplot2"))  install.packages("ggplot2")
if (!require("caret"))    install.packages("caret")
if (!require("e1071"))    install.packages("e1071")
if (!require("car"))      install.packages("car")
if (!require("stats"))    install.packages("stats")
if (!require("gridExtra"))    install.packages("gridExtra")
if (!require("ROCR"))    install.packages("ROCR")
if (!require("fBasics"))    install.packages("fBasics")
if (!require("randomForest"))    install.packages("randomForest")


# Load librarys
library(caret)
library(ggplot2)
library(corrplot)
library(e1071)
library(car)
library(stats)
library(gridExtra)
library(ROCR)
library(fBasics)
library(randomForest)

# Read database
dataset <- read.csv("HR_comma_sep.csv")

# look at data
sapply(dataset, function(x) length(unique(x)))
#how many unique elements we have. What variable are or can be seen as factors
str(dataset)
summary(dataset)

#satisfaction_level       last_evaluation        number_project  average_montly_hours    time_spend_company         Work_accident 
#92                    65                     6                   215                     8                     2 
#left promotion_last_5years                 sales                salary 
#2                     2                    10                     3 

# 'data.frame':	14999 obs. of  10 variables:
#   $ satisfaction_level   : num  0.38 0.8 0.11 0.72 0.37 0.41 0.1 0.92 0.89 0.42 ...
# $ last_evaluation      : num  0.53 0.86 0.88 0.87 0.52 0.5 0.77 0.85 1 0.53 ...
# $ number_project       : int  2 5 7 5 2 2 6 5 5 2 ...
# $ average_montly_hours : int  157 262 272 223 159 153 247 259 224 142 ...
# $ time_spend_company   : int  3 6 4 5 3 3 4 5 5 3 ...
# $ Work_accident        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ left                 : int  1 1 1 1 1 1 1 1 1 1 ...
# $ promotion_last_5years: int  0 0 0 0 0 0 0 0 0 0 ...
# $ sales                : Factor w/ 10 levels "accounting","hr",..: 8 8 8 8 8 8 8 8 8 8 ...
# $ salary               : Factor w/ 3 levels "high","low","medium": 2 3 3 2 2 2 2 2 2 2 ...

#Change some variables type at factors

dataset$left <- as.factor(dataset$left)
dataset$promotion_last_5years<- as.factor(dataset$promotion_last_5years)
dataset$Work_accident <- as.factor(dataset$Work_accident)
dataset$salary <- ordered(dataset$salary, c("low","medium" ,"high"))


# look at some graphs: historams
par(mfrow=c(2,3))


hist(dataset$last_evaluation, col="lightyellow", freq = FALSE, main = paste('Last Evaluation'), xlab = "x")
hist(dataset$satisfaction_level, col="lightyellow", breaks = 10, freq = FALSE,main = paste('Level of Satisfaction'), xlab = "x")
hist(dataset$average_montly_hours, col="lightyellow",  freq = FALSE, main = paste('Monthly hours'), xlab = "x")
hist(dataset$number_project, col="lightyellow",breaks = 5, freq = FALSE, main = paste('Number of Projects'), xlab = "x")
hist(dataset$time_spend_company, col="lightyellow",  freq = FALSE, main = paste('Time at company'), xlab = "x")

#density for different factors
g1<-ggplot(dataset, aes(x = last_evaluation, colour = factor(left))) + 
  geom_density() + ggtitle("Last Evaluation")
g2<-ggplot(dataset, aes(x = last_evaluation, colour = factor(salary))) + 
  geom_density() + ggtitle(" ")
grid.arrange(g1,g2, nrow=2, ncol=1)

g1<-ggplot(dataset, aes(x = satisfaction_level, colour = factor(left))) + 
  geom_density() + ggtitle("Level of Satisfaction")
g2<-ggplot(dataset, aes(x =  satisfaction_level, colour = factor(salary))) + 
  geom_density() + ggtitle(" ")
grid.arrange(g1,g2, nrow=2, ncol=1)

g1<-ggplot(dataset, aes(x = average_montly_hours, colour = factor(left))) + 
  geom_density() + ggtitle("Monthly hours")
g2<-ggplot(dataset, aes(x =  average_montly_hours, colour = factor(salary))) + 
  geom_density() + ggtitle(" ")
grid.arrange(g1,g2, nrow=2, ncol=1)

#The graphs of density for variables differ for different left factors, but have almost the same type for different salary factors.  
#From graphs none of these variables are normal. We can also check null hypothesis  that the population is normally distributed
#with such tests as ad.test(), shapiro.test(),lillie.test() or others

set.seed(123)
split = createDataPartition(y=dataset$left, p=0.33, list=FALSE)
smallsample <- dataset[split, ]

print(shapiroTest(smallsample$last_evaluation)) # works for size of sample <=5000
print(ksnormTest(unique(dataset$last_evaluation)))# works only for unique elements
print(adTest(dataset$last_evaluation))
print(lillieTest(dataset$last_evaluation))

# Shapiro - Wilk Normality Test
# Test Results:
#   STATISTIC:
#   W: 0.9507
# P VALUE:
#   < 2.2e-16 

#   One-sample Kolmogorov-Smirnov test
# Test Results:
#   STATISTIC:
#   D: 0.6406
# P VALUE:
#   Alternative Two-Sided: < 2.2e-16 
# Alternative      Less: < 2.2e-16 
# Alternative   Greater: 0.03385 
# 
#   Anderson - Darling Normality Test
# Test Results:
#   STATISTIC:
#   A: 221.1229
# P VALUE:
#   < 2.2e-16 
# 
#  Lilliefors (KS) Normality Test

# Test Results:
#   STATISTIC:
#   D: 0.0875
# P VALUE:
#   < 2.2e-16 

# For other variables we have the same result: null hypotesis is rejected 

# analysis of dependencies that affect leaves. 
g1<-ggplot(dataset, aes(x =average_montly_hours, y =  time_spend_company))+ 
  geom_point(color = as.numeric(dataset$left))+
  geom_density2d()+
  labs(title="The probability destribution of leaving \n (red points show who left)", x = "Avrenge hours per month", y = "Years in the company")

g2<-ggplot(dataset, aes(x =last_evaluation, y =  satisfaction_level))+ 
  geom_point(color = as.numeric(dataset$left))+
  geom_density2d()+
  labs(x="The level of the last evaluation", y = "The level of employee satisfaction", 
       title = "The probability destribution of leaving")


grid.arrange(g1,g2, nrow=2, ncol=1)


# check correlation for numeric variables 

par(mfrow=c(1,1))

num.cols <- sapply(dataset,is.numeric)
cor.data <- cor(dataset[,num.cols])
# visualisation of corrolation with corrlot
corrplot(cor.data, method = "pie")
# we can see that pairs (last_evaluation, number_project), (last_evaluation, average_montly_hours)
#(average_montly_hours, number_project) have the biggest correlation, these coefficients really differ from 0:

cor.test(dataset$last_evaluation,dataset$number_project)
cor.test(dataset$last_evaluation,dataset$average_montly_hours)
cor.test(dataset$number_project,dataset$average_montly_hours)
# we remember that coefficient tells us only about linear dependence. And between these variables there are not linear dependence.
# but could be nonlinear

#Predicshion who are going to leave
#Split data training : testing like 3:1
set.seed(123)
split = createDataPartition(y=dataset$left, p=0.75, list=FALSE)
training <- dataset[split, ]
testing <- dataset[-split,]


#MODEL 1 NAIVE BAYES
set.seed(123)
modelFit <- naiveBayes(left ~. , data = training )
prediction_prob <- predict(modelFit,  newdata = testing[-7], type = "raw")
prediction_bayes <- prediction_prob[,2]
y_hat <- ifelse(prediction_bayes<0.5, 0, 1)
y_hat <- as.factor(y_hat)
confusionMatrix(y_hat, testing$left)

# Confusion Matrix and Statistics
# 
#             Reference
# Prediction    0    1
#         0   2625  365
#         1    232  527
# 
# Accuracy : 0.8408          
# 95% CI : (0.8286, 0.8523)
# No Information Rate : 0.7621          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.5371          
# Mcnemar's Test P-Value : 6.575e-08       
# 
# Sensitivity : 0.9188          
# Specificity : 0.5908          
# Pos Pred Value : 0.8779          
# Neg Pred Value : 0.6943          
# Prevalence : 0.7621          
# Detection Rate : 0.7002          
# Detection Prevalence : 0.7975          
# Balanced Accuracy : 0.7548          

#Accuracy : 0.8408 means that we predict correctly 84% of results about who left and stayed 
# Sensitivity : 0.9188   means that we predict correct result 92% of stayed people      
# Specificity : 0.5908 means that we predict correct result 59% of left people

#Build CAP curve

cap_data_bayes <- cbind(left = as.numeric(testing$left)-1, predicted = round(prediction_bayes,5))
cap_data_bayes <- as.data.frame(cap_data_bayes)


attach(cap_data_bayes)
nraws <- length(left)
nleft <- length(left[left==1])
cap_data_bayes <- cbind (cap_data_bayes[order( -predicted),], Percent_of_Sample = (1:nraws)/nraws)
n <-0
for (i in 1:nraws) {
  n <- cap_data_bayes[i,1]+n
  cap_data_bayes[i,4]<- n/nleft
}
perc=nleft/nraws
detach(cap_data_bayes)

plot(x = cap_data_bayes[,3], y = cap_data_bayes[,4], col = "red", lwd =0.3, 
     xlab = "Percent of Sample", ylab = "Predicted Left")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("Percent people left is ", round(perc,2)*100, sep="", "%" ), cex=1.4)
title("CAP Curve")

#Build ROC curve (it shows )

par(mfrow=c(1,1))
ROCRpred = prediction(prediction_bayes, testing$left)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve Bayes")



#MODEL 2 train with random forest model (packege Random Forest) 
set.seed(123)
rf.model <- randomForest(x = training[-7], y = training$left)
rf.predict <- predict(rf.model, testing[-7])
confusionMatrix(rf.predict, testing$left)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
#           0 2852   29
#           1    5  863
# 
# Accuracy : 0.9909          
# 95% CI : (0.9873, 0.9937)
# No Information Rate : 0.7621          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9748          
# Mcnemar's Test P-Value : 7.998e-05       
#                                           
#             Sensitivity : 0.9982          
#             Specificity : 0.9675          
#          Pos Pred Value : 0.9899          
#          Neg Pred Value : 0.9942          
#              Prevalence : 0.7621          
#          Detection Rate : 0.7607          
#    Detection Prevalence : 0.7685          
#       Balanced Accuracy : 0.9829

