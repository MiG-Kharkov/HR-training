# Exploratory data
# Preliminary analysis real work is supposed to be in main.R
install.packages("corrplot")
install.packages(ROCR)
#Load librarys
library(ggplot2)
library(caret)
library(corrplot)
library(e1071)
library(ROCR)
#clear global environment
rm(list = ls())

dataset <- read.csv("HR_comma_sep.csv")

dataset$left <- as.factor(dataset$left)
dataset$promotion_last_5years<- as.factor(dataset$promotion_last_5years)
dataset$Work_accident <- as.factor(dataset$Work_accident)
dataset$salary <- ordered(dataset$salary, c("low","medium" ,"high"))

summary(dataset)
str(dataset)

# check correlation 
# only for numeric
num.cols <- sapply(dataset,is.numeric)
cor.data <- cor(dataset[,num.cols])
# visualisation of corrolation with corrlot
corrplot(cor.data, method = "color")

# Feature plot (*caret* package) - 
# and save it to a file "pic-01-scatter-plot-matrix.png"
png(filename="pic-01-scatter-plot-matrix.png", 
    width = 14, height = 14, units = 'in', res = 300)
featurePlot(x=dataset[,c("satisfaction_level","last_evaluation", "number_project",
                          "average_montly_hours",
                          "time_spend_company",
                          "promotion_last_5years",
                          "sales",
                          "salary")],
            y = dataset$left,
            plot="pairs")
dev.off() 

featurePlot(x = dataset[,c("satisfaction_level","last_evaluation", "number_project",
                           "average_montly_hours",
                           "time_spend_company")], 
            y = dataset$left, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 2))

# analysis of dependencies that affect leaves. 
ggplot(dataset, aes(x =average_montly_hours, y =  time_spend_company))+ 
  geom_point(color = as.numeric(dataset$left))+
  geom_density2d()+
  labs(title="The probability destribution of leaving", x = "Avrenge hours per month", y = "Years in the company")

ggplot(dataset, aes(x =last_evaluation, y =  satisfaction_level))+ 
  geom_point(color = as.numeric(dataset$left))+
  geom_density2d()+
  labs(x="The level of the last evaluation", y = "The level of employee satisfaction", 
       title = "The probability destribution of leaving")

ggplot(dataset, aes(x =  satisfaction_level, colour = factor(left), fill = factor(left))) + geom_density() +
  geom_vline(xintercept = mean(dataset$satisfaction_level))+
  ggtitle("Satisfaction Level \n density plot \n w. Left") + xlab("Satisfaction level")

ggplot(dataset, aes(x =  salary, y = satisfaction_level, fill = factor(left), colour = factor(left))) + 
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(alpha = 0.1)+
  ggtitle("Geom boxplot \n salary vs sat level") + xlab("Salary") + ylab("Satisfacion level") 

#various ways to visualize information 
#histogram
ggplot(dataset, aes(x = satisfaction_level))+ geom_histogram()
ggplot(dataset, aes(x = last_evaluation))+ geom_histogram()
ggplot(dataset, aes(x = time_spend_company))+ geom_histogram()

#i want to group sales factors. when i creates model for logical regression
#i saw some of sales are not stitistic significant
#so I guess, we can creat dummy variables or group some of them
# saleshr                 0.2901505  0.1502195   1.932 0.053420 .  
# salesIT                -0.1607812  0.1411670  -1.139 0.254727    
# salesmanagement        -0.5165982  0.1849407  -2.793 0.005217 ** 
# salesmarketing          0.0077278  0.1513287   0.051 0.959272    
# salesproduct_mng       -0.1045803  0.1492742  -0.701 0.483558    
# salesRandD             -0.5518519  0.1650076  -3.344 0.000825 ***
# salessales              0.0433311  0.1174506   0.369 0.712180    
# salessupport            0.0369344  0.1258566   0.293 0.769168    
# salestechnical          0.0350542  0.1229423   0.285 0.775547   
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
dataset$sales <- as.factor(sapply(dataset$sales, sales_mod))
# if I use ordered factor model.matrix doen't work properly
# dataset$sales <- ordered(dataset$sales, c("other", "management", "RandD"))
summary(dataset)
str(dataset)
#create dummy variable for it
dummy_matrix <- model.matrix(~ sales, data = dataset)
table(dummy_matrix)
#combine dummy and other variable with dataset without sales factor
dataset <- cbind(dataset[, -9], sales_other = dummy_matrix[,2], sales_RandD= dummy_matrix[,3])

#split the dataset into train and test sets (using caret lib, it gives nuber of records)
#library(caTools) can provide with vector true false for spliting

set.seed(123)
split = createDataPartition(y=dataset$left, p=0.75, list=FALSE)
training <- dataset[split, ]
testing <- dataset[-split,]
names(training)


#train models with different set of independen variables
#after model - resal in comments
#quick start with logical regression
modelFit <- glm(formula = left ~ ., 
                family = binomial , 
                training)
#   glm(formula = left ~ ., family = binomial, data = training)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.2482  -0.6603  -0.4018  -0.1152   3.0534  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -0.8996993  0.2062815  -4.362 1.29e-05 ***
#   satisfaction_level     -4.1619938  0.1134434 -36.688  < 2e-16 ***
#   last_evaluation         0.7911745  0.1720377   4.599 4.25e-06 ***
#   number_project         -0.3223698  0.0248218 -12.987  < 2e-16 ***
#   average_montly_hours    0.0045616  0.0005974   7.636 2.25e-14 ***
#   time_spend_company      0.2656302  0.0178240  14.903  < 2e-16 ***
#   Work_accident1         -1.5948253  0.1043029 -15.290  < 2e-16 ***
#   promotion_last_5years1 -1.2249191  0.2933590  -4.175 2.97e-05 ***
#   salary.L               -1.3793755  0.1050965 -13.125  < 2e-16 ***
#   salary.Q               -0.3762478  0.0685437  -5.489 4.04e-08 ***
#   sales_other             0.5390296  0.1533004   3.516 0.000438 ***
#   sales_RandD            -0.0363993  0.1960747  -0.186 0.852727    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 12351  on 11249  degrees of freedom
# Residual deviance:  9627  on 11238  degrees of freedom
# AIC: 9651
# 
# Number of Fisher Scoring iterations: 5
modelFit <- glm(formula = left ~ . - sales_RandD,
                family = binomial, 
                training)
# glm(formula = left ~ . - sales_RandD, family = binomial, data = training)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.2487  -0.6603  -0.4018  -0.1155   3.0534  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -0.9210179  0.1716128  -5.367 8.01e-08 ***
#   satisfaction_level     -4.1620957  0.1134436 -36.689  < 2e-16 ***
#   last_evaluation         0.7915599  0.1720224   4.601 4.19e-06 ***
#   number_project         -0.3224366  0.0248201 -12.991  < 2e-16 ***
#   average_montly_hours    0.0045606  0.0005974   7.634 2.27e-14 ***
#   time_spend_company      0.2658448  0.0177908  14.943  < 2e-16 ***
#   Work_accident1         -1.5948993  0.1043024 -15.291  < 2e-16 ***
#   promotion_last_5years1 -1.2226420  0.2930786  -4.172 3.02e-05 ***
#   salary.L               -1.3773495  0.1045071 -13.179  < 2e-16 ***
#   salary.Q               -0.3750686  0.0682412  -5.496 3.88e-08 ***
#   sales_other             0.5606547  0.1000875   5.602 2.12e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 12350.7  on 11249  degrees of freedom
# Residual deviance:  9627.1  on 11239  degrees of freedom
# AIC: 9649.1
# 
# Number of Fisher Scoring iterations: 5
summary(modelFit)


# build confution matrix
prediction <- predict(modelFit, type = "response", newdata = testing[-7])
y_hat <- ifelse(prediction > 0.5, 1, 0)
y_hat <- as.factor(y_hat)

#get a confusion matrix without extra information 
table(y_hat, testing$left)
# y_hat    0    1
# 0 2632  575
# 1  225  317

# plot ROC Curve
ROCRpred = prediction(prediction, testing$left)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]]
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve")
#plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

#get a confusion matrix with extra datails with caret packege
confusionMatrix(y_hat, testing$left)
# Confusion Matrix and Statistics
# 
#             Reference
# Prediction  0    1
#         0 2633  575
#         1  224  317
# 
# Accuracy : 0.7869          
# 95% CI : (0.7734, 0.7999)
# No Information Rate : 0.7621          
# P-Value [Acc > NIR] : 0.000166        
# 
# Kappa : 0.3203          
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.9216          
#             Specificity : 0.3554          
#          Pos Pred Value : 0.8208          
#          Neg Pred Value : 0.5860          
#              Prevalence : 0.7621          
#          Detection Rate : 0.7023          
#    Detection Prevalence : 0.8557          
#       Balanced Accuracy : 0.6385          
#                                           
#        'Positive' Class : 0      

# I have probability for previous result so I am going to create CAP pot
cap_data <- cbind(left = as.numeric(testing$left)-1, prediction)

write.csv(cap_data, "cap_data.csv")
# pic-03-cap analysis my model from exel is a screenshort from file cap_analysis.xlsx 
# where I have calculated CAP analysis for the model
# pic-02-cap analysis information.png shows information how to interpret this analysis

# inforamtion for interactive confusion matrix based on threshold 
# table was saved in file cap_data.csv and we will use it in for building web server in shiny
# next code deploying application in cloud
library(shiny)
# as if we use public github i deleted secret inforamtion
rsconnect::setAccountInfo(name='damsgroup', token='УДАЛИЛ', secret='УДАЛИЛ')
# test application 
runApp()
library(rsconnect)
# deploy app, don't do it from your compluter
deployApp()

#=================================================
# create prediction based on Naive Bayes from library e1071
# importent - it works only with factors

# -7 has our left column so we have to take it off 
# modelFit <- naiveBayes(x = training[,-7], y = training$left) #different way for a function call

modelFit <- naiveBayes(left ~. , data = training)

summary(modelFit)
# Length Class  Mode     
# apriori  2     table  numeric  
# tables  10     -none- list     
# levels   2     -none- character
# call     3     -none- call 

# there is no probability for preduction only true false factors
prediction <- predict(modelFit,  newdata = testing[-7])

confusionMatrix(prediction, testing$left)
# Confusion Matrix and Statistics
# 
#             Reference
# Prediction    0    1
#         0   2554  312
#         1   303  580
# 
# Accuracy : 0.836           
# 95% CI : (0.8237, 0.8477)
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


# I was trying to improve the model but got the identical result
modelFit <- naiveBayes(left ~. - sales_RandD , data = training)
summary(modelFit)
prediction <- predict(modelFit,  newdata = testing[-7])


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



confusionMatrix(y_hat, testing$left )

# Cumulative Accuracy Profile (CAP)

# I have probability for previous result so I am going to create CAP pot
cap_data_bayes <- cbind(left = as.numeric(testing$left)-1, predicted = round(prediction_bayes,5))
# усли не преобразовыывать в дата фрейм то не будет можно имя, надо будет order(cap_dat_bayes[,2)]
cap_data_bayes <- as.data.frame(cap_data_bayes)
cap_data_bayes <-  cap_data_bayes[order(cap_data_bayes$predicted, decreasing = TRUE),]

attach(cap_data_bayes)
cap_data_bayes <- cap_data_bayes[order(left, -predicted),]
detach(cap_data_bayes)

write.csv(cap_data_bayes, "cap_data_bayes.csv")
# cap_analysis_bayes.xlsx has information from this model
# pic-04-cap analysis for cap_data_bayes.png is a screenshort with CAP graphic
ggplot(testing, aes(x = prediction_bayes, fill = factor(left), colour = factor(left))) + 
    geom_density() + ggtitle("Predicted dens test set")
# deploy a new app for bayes model
deployApp()


# Density plots probabilities for testing set
ggplot(testing, aes(x = prediction_bayes, fill = factor(left), colour = factor(left))) + 
  geom_density() + ggtitle("Predicted denity for the test set")


# Building ROC plot

# These functions return or set information about the individual slots in an object.
auc <- slot(performance(pred, "auc"), "y.values")[[1]]

# This function is used to transform the input data into a standardized format.
pred <- prediction(prediction_bayes, testing$left)
# All kinds of predictor evaluations are performed using this function.
perf <- performance(pred,"tpr", "fpr")

plot(perf)
lines(c(0,1),c(0,1))
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve")

# train with random forest model
rf.model <- train(left ~., data = training, method = "rf")
summary(rf.model)
randomForest
rf.prediction <- predict(rf.model, newdata = testing[-7], type = "prob")
prediction_rf <- (rf.prediction[,2]- rf.prediction[,1]+1)/2
summary(prediction_rf)
y_hat <- ifelse(prediction_rf > 0.01, 1, 0)
y_hat <- as.factor(y_hat)
cf.05<-confusionMatrix(y_hat, testing$left )
cf.41<-confusionMatrix(y_hat, testing$left )
confusionMatrix(y_hat, testing$left )

# Density plots probabilities for testing set
ggplot(testing, aes(x = prediction_rf, fill = factor(left), colour = factor(left))) + 
  geom_density() + ggtitle("Predicted denity for the test set")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 2852   28
# 1    5  864
# 
# Accuracy : 0.9912          
# 95% CI : (0.9877, 0.9939)
# No Information Rate : 0.7621          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9755          
# Mcnemar's Test P-Value : 0.0001283       
# 
# Sensitivity : 0.9982          
# Specificity : 0.9686          
# Pos Pred Value : 0.9903          
# Neg Pred Value : 0.9942          
# Prevalence : 0.7621          
# Detection Rate : 0.7607          
# Detection Prevalence : 0.7682          
# Balanced Accuracy : 0.9834          
# 
# 'Positive' Class : 0               


# Cumulative Accuracy Profile (CAP)
# I have probability for previous result so I am going to create CAP pot
cap_data_rf <- cbind(left = as.numeric(testing$left)-1, predicted = round(prediction_rf,5))
write.csv(cap_data_rf, "cap_data_rf.csv")
