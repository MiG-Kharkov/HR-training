#Exploratory data
install.packages("corrplot")
#Load librarys
library(ggplot2)
library(caret)
library(corrplot)
library(e1071)

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
table(testing$left, y_hat)
# y_hat
# 0    1
# 0 2632  225
# 1  575  317

#get a confusion matrix with extra datails with caret packege
confusionMatrix(testing$left, y_hat)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 2632  225
# 1  575  317
# 
# Accuracy : 0.7866          
# 95% CI : (0.7731, 0.7996)
# No Information Rate : 0.8554          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.3198          
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.8207          
#             Specificity : 0.5849          
#          Pos Pred Value : 0.9212          
#          Neg Pred Value : 0.3554          
#              Prevalence : 0.8554          
#          Detection Rate : 0.7021          
#    Detection Prevalence : 0.7621          
#       Balanced Accuracy : 0.7028          
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
# so is a good question how to adjust this prediction using threshold
# answer for this questin is a paramert threshold - порог :)
# I have added threshold with 0.5 и получил эпик фейл. 
# не понятно, что надо сделать, что бы настройить, так что задача для всех
prediction <- predict(modelFit,  newdata = testing[-7])

confusionMatrix(testing$left, prediction)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 2554  303
# 1  312  580
# 
# Accuracy : 0.836           
# 95% CI : (0.8237, 0.8477)
# No Information Rate : 0.7645          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.5461          
# Mcnemar's Test P-Value : 0.747           
# 
# Sensitivity : 0.8911          
# Specificity : 0.6569          
# Pos Pred Value : 0.8939          
# Neg Pred Value : 0.6502          
# Prevalence : 0.7645          
# Detection Rate : 0.6812          
# Detection Prevalence : 0.7621          
# Balanced Accuracy : 0.7740          

# 
# 'Positive' Class : 0       

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
confusionMatrix(testing$left, y_hat)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 2554  303
# 1  312  580
# 
# Accuracy : 0.836           
# 95% CI : (0.8237, 0.8477)
# No Information Rate : 0.7645          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.5461          
# Mcnemar's Test P-Value : 0.747           
#                                           
#             Sensitivity : 0.8911          
#             Specificity : 0.6569          
#          Pos Pred Value : 0.8939          
#          Neg Pred Value : 0.6502          
#              Prevalence : 0.7645          
#          Detection Rate : 0.6812          
#    Detection Prevalence : 0.7621          
#       Balanced Accuracy : 0.7740          
#                                           
#        'Positive' Class : 0    

# next step is to join two columns form prediction_row table in one vector
# ???????
