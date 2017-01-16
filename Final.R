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

# Read database
dataset <- read.csv("HR_comma_sep.csv")

