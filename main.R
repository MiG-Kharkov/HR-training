# Первый файл Exploratory.R написаный с ошибкой
# Прееписываем весь код в чистовую уже красиво и она оснве информации с книги 
# R в действии

# Библиотеки грузим

#clear global environment
rm(list = ls())

# Load dataset and attach it to work wiht less code
dataset <- read.csv("HR_comma_sep.csv")
attach(dataset)



# it's useless command but why not :)
# detach dataset
detach(dataset)

