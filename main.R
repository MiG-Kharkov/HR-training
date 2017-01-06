# Первый файл Exploratory.R написаный с ошибкой
# Прееписываем весь код в чистовую уже красиво и она оснве информации с книги 
# R в действии

# Библиотеки грузим

#clear global environment
rm(list = ls())

# Load dataset and attach it to work wiht less code
dataset <- read.csv("HR_comma_sep.csv")

dataset$left <- as.factor(dataset$left)
dataset$promotion_last_5years<- as.factor(dataset$promotion_last_5years)
dataset$Work_accident <- as.factor(dataset$Work_accident)

str(dataset)
dataset$salary <- factor(dataset$salary, order = TRUE, levels =  c("low","medium" ,"high"))
dataset$salary <- ordered(dataset$salary, c("low","medium" ,"high"))
dataset$salary <- factor(dataset$salary, order = TRUE)

dummy_matrix <- model.matrix(~ salary+sales, data = dataset)
table(dummy_matrix)


