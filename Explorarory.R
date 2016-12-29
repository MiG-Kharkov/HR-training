#Exploratory data

#clear global environment
rm(list = ls())
install.packages("corrplot")
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