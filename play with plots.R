#clear global environment
rm(list = ls())


#experiments
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
plot(dose, drugA, type = "b", lty = 2, pch = 17)
abline(v=seq(30, 40, 5), lty=2, col= 2)
legend("top",legend = "Test")

x = matrix(c(1,1,2,3), 2, 2, byrow = TRUE)
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

manager <- c(1, 2, 3, 4, 5)
date <- c('10/24/08', '10/28/08', '10/1/08', '10/12/08', '5/1/09')
country <- c('US', 'S', 'UK', 'UK', 'UK')
gender <- c('M', 'F', 'F', 'M', 'F')
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age,
                         q1, q2, q3, q4, q5, stringsAsFactors=FALSE)

leadership$age[leadership$age == 99] <- NA

leadership <- within(leadership, {
  agecat <- NA
  agecat[age >75] <- "Elder"
  agecat[age >= 55 & age <= 75] <- "Middle Age"
  agecat[age < 55 ] <- "Young"
})
fix(leadership)

names(leadership)[2]<- "testDate"
is.na(leadership[,6:10])

leadership$date <- as.Date(leadership$date, '%m/%d/%y')
startdate <- as.Date('2009-01-01')
enddate <- as.Date('2009-10-31')
newdata <- leadership[which(leadership$date >= startdate &
                              leadership$date <= enddate),]
newdata <- subset(leadership, age >= 35 | age < 24,
                  select=c(q1, q2, q3, q4))
