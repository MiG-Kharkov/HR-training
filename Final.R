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

# logistic regression
model_Log_Reg <- glm(formula = left ~ . ,
                       training,
                       family = binomial)
summary(model_Log_Reg)

#look at model without sales
model_Log_Reg1 <- glm(formula = left ~ -sales,
                     training,
                     family = binomial)
summary(model_Log_Reg)
summary(model_Log_Reg1)
anova(model_Log_Reg,model_Log_Reg1,test="Chisq")

#We can see for some levels of factor sales coefficients  are not statistic significant

# saleshr                 0.2901505  0.1502195   1.932 0.053420 .  
# salesIT                -0.1607812  0.1411670  -1.139 0.254727    
# salesmanagement        -0.5165982  0.1849407  -2.793 0.005217 ** 
# salesmarketing          0.0077278  0.1513287   0.051 0.959272    
# salesproduct_mng       -0.1045803  0.1492742  -0.701 0.483558    
# salesRandD             -0.5518519  0.1650076  -3.344 0.000825 ***
# salessales              0.0433311  0.1174506   0.369 0.712180    
# salessupport            0.0369344  0.1258566   0.293 0.769168    
# salestechnical          0.0350542  0.1229423   0.285 0.775547 

# But we cann't reject them. According to anova test model with sales variable is better then without

# Null deviance: 12350.7  on 11249  degrees of freedom
# Residual deviance:  9614.7  on 11231  degrees of freedom
# AIC: 9652.7
# 
# Null deviance: 12350.7  on 11249  degrees of freedom
# Residual deviance:  9660.7  on 11240  degrees of freedom
# AIC: 9680.7

# Resid. Df Resid. Dev  Df Deviance  Pr(>Chi)    
# 1     11231     9614.7                           
# 2     11249    12350.7 -18  -2735.9 < 2.2e-16 ***
#



#So we should leave "sales" but we can group sales factors. 
table(dataset$sales)
# accounting          hr          IT  management   marketing product_mng       RandD       sales     support   technical 
# 767         739        1227         630         858         902         787        4140        2229        2720 
#good question for discussion can we leave only management and RandD. They involve powerfuly but it's only small % 

sales_mod <- function(job){
  job <- as.character(job)
  if ( job == "management" | job == "RandD" ) { return(job)}
  else {return("other")}
}
#reform sales using function
dataset1<- dataset
dataset1$sales <- as.factor(sapply(dataset1$sales, sales_mod))

#create dummy variable for sales
dummy_matrix <- model.matrix(~ sales, data = dataset1)
table(dummy_matrix)
#combine dummy and other variable with dataset without sales factor
dataset1 <- cbind(dataset1[, -9], sales_other = dummy_matrix[,2], sales_RandD= dummy_matrix[,3])

#new training and testing data
split = createDataPartition(y=dataset1$left, p=0.75, list=FALSE)
training1 <- dataset1[split, ]
testing1 <- dataset1[-split,]
# logistic regression with dummy variables
model_Log_Reg_dum <- glm(left ~ .,
                         data=training1,
                         family = binomial)

summary(model_Log_Reg_dum)

model_Log_Reg_dum1 <- glm(left ~ .-sales_RandD,
                         data=training1,
                         family = binomial)

summary(model_Log_Reg_dum)
summary(model_Log_Reg_dum1)
anova(model_Log_Reg_dum,model_Log_Reg_dum1,test="Chisq")


# 
# Null deviance: 12350.7  on 11249  degrees of freedom
# Residual deviance:  9692.3  on 11238  degrees of freedom
# AIC: 9716.3
# 
# Null deviance: 12350.7  on 11249  degrees of freedom
# Residual deviance:  9694.4  on 11239  degrees of freedom
# AIC: 9716.4

# Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1     11238     9692.3                     
# 2     11239     9694.4 -1  -2.0614   0.1511

# anova test say that there is no differance between this models. so we can use model without sales_RandD 



# build confution matrix for model_Log_Reg_dum1
prediction <- predict(model_Log_Reg_dum1, type = "response", newdata = testing1[-7])
y_hat <- ifelse(prediction > 0.5, 1, 0)
y_hat <- as.factor(y_hat)

#get a confusion matrix without extra information 
table(y_hat, testing1$left)
# y_hat    0    1
# 0 2656  606
# 1  201  286

confusionMatrix(y_hat, testing1$left)
# Reference
# Prediction    0    1
# 0 2664  598
# 1  193  294
# 
# Accuracy : 0.789          
# 95% CI : (0.7756, 0.802)
# No Information Rate : 0.7621         
# P-Value [Acc > NIR] : 4.73e-05       
# 
# Kappa : 0.3105         
# Mcnemar's Test P-Value : < 2.2e-16      
# 
# Sensitivity : 0.9324         
# Specificity : 0.3296         
# Pos Pred Value : 0.8167         
# Neg Pred Value : 0.6037         
# Prevalence : 0.7621         
# Detection Rate : 0.7106         
# Detection Prevalence : 0.8701         
# Balanced Accuracy : 0.6310         
# 
# 'Positive' Class : 0   

#Accuracy : 0.789 means that we predict correctly 78.9% results about who left and stayed 
# Sensitivity : 0.9324   means that we predict correctly 93.2% results about who stayed      
# Specificity : 0.3296 means that we predict incorrectly 33% results about who left

# plot ROC Curve
ROCRpred = prediction(prediction, testing1$left)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]]
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve for glm-model")

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
