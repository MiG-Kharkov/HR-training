# Install packages if we need it 
if (!require("corrplot")) install.packages("corrplot")
if (!require("ggplot2"))  install.packages("ggplot2")
if (!require("caret"))    install.packages("caret")
if (!require("e1071"))    install.packages("e1071")
if (!require("car"))      install.packages("car")
if (!require("nortets"))  install.packages("nortest")
if (!require("pastecs"))  install.packages("pastecs")
if (!require("stats"))    install.packages("stats")
if (!require("MASS"))    install.packages("MASS")
if (!require("leaps"))    install.packages("leaps")
if (!require("gridExtra"))    install.packages("gridExtra")
if (!require("ROCR"))    install.packages("ROCR")
if (!require("fBasics"))    install.packages("fBasics")

# Load librarys
library(caret)
library(ggplot2)
library(corrplot)
library(e1071)
library(car)
library(nortest)
library(pastecs)
library(stats)
library(MASS)
library(leaps)
library(gridExtra)
library(ROCR)
library(fBasics)

# Read database
dataset <- read.csv("HR_comma_sep.csv")

# look at data
sapply(dataset, function(x) length(unique(x)))
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

#Change some variables as factors

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


             
#The density for variables are different for different left factors, and the same for different salary factors.  
#From graphs none of these variables are normal. We can also check null hypothesis  that the population is normally distributed
#with such tests as ad.test(), shapiro.test(),lillie.test() and others

set.seed(123)
split = createDataPartition(y=dataset$left, p=0.33, list=FALSE)
smallsample <- dataset[split, ]

print(shapiro.test(smallsample$last_evaluation))
print(ksnormTest(unique(x)))
print(adTest(sort(dataset$last_evaluation)))
print(cvm.test(dataset$last_evaluation))
print(lillie.test(dataset$last_evaluation))

# Shapiro-Wilk normality test
# data:  x1
# W = 0.95226, p-value < 2.2e-16

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
# Cramer-von Mises normality test
# data:  x
# W = 34.425, p-value = 7.37e-10
# 

# Lilliefors (Kolmogorov-Smirnov) normality test
# data:  x
# D = 0.087476, p-value < 2.2e-16

# For other variables we have the same result

# analysis of dependencies that affect leaves. 
g1<-ggplot(dataset, aes(x =average_montly_hours, y =  time_spend_company))+ 
  geom_point(color = as.numeric(dataset$left))+
  geom_density2d()+
  labs(title="The probability destribution of leaving", x = "Avrenge hours per month", y = "Years in the company")

g2<-ggplot(dataset, aes(x =last_evaluation, y =  satisfaction_level))+ 
  geom_point(color = as.numeric(dataset$left))+
  geom_density2d()+
  labs(x="The level of the last evaluation", y = "The level of employee satisfaction", 
       title = "The probability destribution of leaving")


grid.arrange(g1,g2, nrow=2, ncol=1)


# check correlation and multicollinearity for numeric variables 

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

vif(glm(formula = left ~ . ,family = binomial,data = dataset))
#GVIFs are from 1 to 2 so there is no multicollinearity

#MODEL 1 
set.seed(123)
split = createDataPartition(y=dataset$left, p=0.75, list=FALSE)
training <- dataset[split, ]
testing <- dataset[-split,]


#MODEL 2 NAIVE BAYES
modelFit <- naiveBayes(left ~. , data = training)

summary(modelFit)
# Length Class  Mode     
# apriori  2     table  numeric  
# tables   9     -none- list     
# levels   2     -none- character
# call     4     -none- call 

prediction <- predict(modelFit,  newdata = testing[-7])
confusionMatrix(prediction, testing$left)
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

# Look at data with dummy variables       
modelFit1 <- naiveBayes(left ~. , data = training1)

summary(modelFit1)
# Length Class  Mode     
# apriori  2     table  numeric  
# tables   10     -none- list     
# levels   2     -none- character
# call     4     -none- call 


prediction <- predict(modelFit1,  newdata = testing1[-7])
confusionMatrix(prediction, testing1$left)

# I was trying to improve the model but got the identical result
modelFit <- naiveBayes(left ~. - sales_RandD , data = training)
summary(modelFit)
prediction <- predict(modelFit,  newdata = testing[-7])

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 2566  305
# 1  291  587
# 
# Accuracy : 0.841           
# 95% CI : (0.8289, 0.8526)
# No Information Rate : 0.7621          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.5592          
# Mcnemar's Test P-Value : 0.5944          
# 
# Sensitivity : 0.8981          
# Specificity : 0.6581          
# Pos Pred Value : 0.8938          
# Neg Pred Value : 0.6686          
# Prevalence : 0.7621          
# Detection Rate : 0.6844          
# Detection Prevalence : 0.7658          
# Balanced Accuracy : 0.7781

#we have almost the same accuracy, but a little different sensetivity and specifity

# privious function gives factors Yes No as a resalt. I am going to get probability  
# for drawing CAP plot
prediction_raw <- predict(modelFit,  newdata = testing[-7], type = "raw")

# I got not the same probфbility vector as for logical regression. 
# this vector has two columns with probability for each variant. 
# so folowing expression gets y_hat and it gives absolutely the same result
y_hat <- ifelse(prediction_raw[,1] > prediction_raw[,2], 0, 1)
y_hat <- as.factor(y_hat)
confusionMatrix(y_hat, testing$left)
# Confusion Matrix and Statistics
# 
#             Reference
# Prediction    0    1
#         0   2554  312
#         1   303  580
# 
# Accuracy : 0.836           
# 95% CI : (0.8237, 0.8477)
# No Information Rate : 0.7645          
# No Information Rate : 0.7621          

# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.5461          
# Mcnemar's Test P-Value : 0.747           
#                                           
#             Sensitivity : 0.8939          
#             Specificity : 0.6502          
#          Pos Pred Value : 0.8911          
#          Neg Pred Value : 0.6569          
#              Prevalence : 0.7621          
#          Detection Rate : 0.6812          
#    Detection Prevalence : 0.7645          
#       Balanced Accuracy : 0.7721          
#                                           
#        'Positive' Class : 0      

# next step is to join two columns form prediction_row table in one vector
prediction_bayes <- (prediction_raw[,2]- prediction_raw[,1]+1)/2
summary(prediction_bayes)
y_hat <- ifelse(prediction_bayes > 0.5, 1, 0)
y_hat <- as.factor(y_hat)
confusionMatrix(testing$left, y_hat)

#ROC curve
ROCRpred = prediction(prediction_bayes, testing$left)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]]
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve")
#phi<-performance(ROCRpred, "phi") ??????????????
