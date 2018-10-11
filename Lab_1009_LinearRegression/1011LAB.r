#######################################################################
# This lab is for IS590DT.                                            #
# Contact Yingjun (yingjun2@illinois.edu) if you have any questions.  #
# Check for more information on https://github.com/yingjun2/IS590DT   #
#######################################################################


library(MASS)
data(Boston)
?Boston
head(Boston)

dim(Boston)

names(Boston)

# The data frame contains the following columns:
# crim: per capita crime rate by town.
# zn: proportion of residential land zoned for lots over 25,000 sq.ft.
# indus: proportion of non-retail business acres per town.
# chas: Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# nox: nitrogen oxides concentration (parts per 10 million).
# rm: average number of rooms per dwelling.
# age: proportion of owner-occupied units built prior to 1940.
# dis: weighted mean of distances to five Boston employment centres.
# rad: index of accessibility to radial highways.
# tax: full-value property-tax rate per $10,000.
# ptratio: pupil-teacher ratio by town.
# black: 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# lstat: lower status of the population (percent).
# medv: median value of owner-occupied homes in $1000s.
# Change the response variable name to be “Y”. Next take some transformations on Y and X’s, suggested in the literature.




myData <- Boston
names(myData)[14] <- "Y"
iLog <- c(1, 3, 5, 6, 8, 9, 10, 14)
myData[, iLog] <- log(myData[, iLog])
myData[, 2] <- myData[, 2]/10
myData[, 7] <- myData[, 7]^2.5/10^4
myData[, 11] <- exp(0.4 * myData[, 11])/1000
myData[, 12] <- myData[, 12]/100
myData[, 13] <- sqrt(myData[, 13])

summary(myData)

# Produce a pair-wise scatter plot. Caution: a big figure.
pairs(myData, pch='.')

# Fit a Linear Model
# Fit a linear regression model using all the predictors.
lmfit <-  lm(Y ~ ., data = myData)

# Check what have been returned by lm.
names(lmfit)  # What have been returned by "lm"?

# Check how to retrieve various LS results.
lmfit$residuals[1]

length(lmfit$residuals)

sqrt(sum(lmfit$residuals^2)/(506 - 14))  # residual standard error

1 - sum(lmfit$residuals^2)/(var(myData$Y)*505)  # R-square

lmfit$coef    # 13 regression cofficients including the intercept

# Print the summary of the LS results
summary(lmfit)

# Predict the price for two new houses.
newx <-  apply(myData, 2, median)
newx <-  rbind(newx, newx)
newx

newx[, 4] <-  c(1,0)
newx

newx[, -14] %*% lmfit$coef[-1] + lmfit$coef[1]

# or use the "predict" function, then new data should 
# be a data frame. 
row.names(newx) = NULL
newx <- data.frame(newx)
predict(lmfit, newdata = newx)

