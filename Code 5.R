# Clean workspace
rm(list=ls())
cat("\014")
graphics.off()
set.seed(123)
#install.packages("GGally")
library(ggplot2)
library(GGally)

# Download the data
setwd("~/Desktop/Georgia Tech Classes/ISyE 6501/Week 5 - Basic Regression/Homework 5/Data")
uscrime <- read.csv("uscrime.csv", sep="")

head(uscrime)
str(uscrime) ## look at the structure of the variables 

# Visualize the data and look to the correlation
ggpairs(data=uscrime, columns=1:16, title="uscrime data")

cor(uscrime)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(uscrime, method="pearson", histogram=TRUE,pch=16)[1]


# First step: conduct a full regression:
model.full <- lm(Crime ~., data = uscrime)
summary(model.full)
summary(model.full)$coefficient

data.point = data.frame(
  M = 14.0,
  So = 0,
  Ed = 10.0,
  Po1 = 12.0,
  Po2 = 15.5,
  LF = 0.640,
  M.F = 94.0,
  Pop = 150,
  NW = 1.1,
  U1 = 0.120,
  U2 = 3.6,
  Wealth = 3200,
  Ineq = 20.1,
  Prob = 0.04,
  Time = 39.0
)
crime_prediction = predict.lm(model.full, data.point)
crime_prediction
# 155 is really low, we are probably overfit, so let's look at the p-values of each point.

model.reduced <- lm(Crime ~  M + Ed + Ineq + Prob, data = uscrime)
summary(model.reduced)
summary(model.reduced)$coefficient

crime_prediction_adj = predict.lm(model.reduced, data.point)
crime_prediction_adj
mean(uscrime$Crime)

model.final <- lm(Crime ~  M + Ed + Ineq + Prob + Po1 + U2, data = uscrime)
summary(model.final)
summary(model.final)$coefficient

crime_prediction_adj_final = predict.lm(model.final, data.point)
crime_prediction_adj_final

anova(model.final, model.reduced) 
#install.packages("car")
library(car)
Anova(model.final, Type="II")

uscrime$predy = predict(model.final)


plot(predy ~ Crime,
     data=uscrime,
     pch = 16,
     xlab="Actual response value",
     ylab="Predicted response value")

abline(0,1, col="blue", lwd=2)


hist(residuals(model.reduced), 
     col="darkgray")

plot(fitted(model.reduced), 
     residuals(model.reduced)
)

model.1  = lm(Crime ~ M, data=uscrime)
model.2  = lm(Crime ~ Ed, data=uscrime)
model.3  = lm(Crime ~ Ineq, data=uscrime)
model.4  = lm(Crime ~ Prob, data=uscrime)
model.5  = lm(Crime ~ M  + Ed, data=uscrime)
model.6  = lm(Crime ~ M  + Ineq, data=uscrime)
model.7  = lm(Crime ~ M + Prob, data=uscrime)
model.8  = lm(Crime ~ Ed  + Ineq, data=uscrime)
model.9  = lm(Crime ~ Ed  + Prob, data=uscrime)
model.10  = lm(Crime ~ Prob + Ineq, data=uscrime)
model.11  = lm(Crime ~ M  + Ed + Ineq, data=uscrime)
model.12  = lm(Crime ~ M  + Ineq + Prob, data=uscrime)
model.13  = lm(Crime ~ M  + Ed + Prob, data=uscrime)
model.14  = lm(Crime ~ Ed + Prob + Ineq, data=uscrime)
model.15  = lm(Crime ~ Ed + Prob + Ineq +M, data=uscrime)
model.16  = lm(Crime ~ Ed + Prob + Ineq +M + Po1 + U2, data=uscrime)
#install.packages("rcompanion")
library(rcompanion)

compareLM(model.1, model.2, model.3, model.4, model.5, model.6,
          model.7, model.8, model.9, model.10, model.11, model.12, model.13, model.14, model.15, model.16)

Result = compareLM(model.1, model.2, model.3, model.4, model.5, model.6,
                    model.7, model.8, model.9, model.10, model.11, model.12, model.13, model.14, model.15, model.16)

plot(Result$Fit.criteria$AICc,
     xlab = "Model number",
     ylab = "AICc")

lines(Result$Fit.criteria$AICc)


