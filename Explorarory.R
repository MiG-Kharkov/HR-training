#Exploratory data
install.packages("corrplot")

#clear global environment
rm(list = ls())
#Load librarys
library(ggplot2)
library(caret)
library(corrplot)

dataset <- read.csv("HR_comma_sep.csv")

dataset$left <- as.factor(dataset$left)
dataset$promotion_last_5years<- as.factor(dataset$promotion_last_5years)
dataset$Work_accident <- as.factor(dataset$Work_accident)
dataset$salary <- ordered(dataset$salary, c("low","medium" ,"high"))

summary(dataset)
str(dataset)

#check correlation 
#only for numeric
num.cols <- sapply(dataset,is.numeric)
cor.data <- cor(dataset[,num.cols])
#visualisation of corrolation with corrlot
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



#quick start with logical regression
modelFit <- glm(formula = left ~ ., 
                family = binomial , 
                training)
summary(modelFit)
