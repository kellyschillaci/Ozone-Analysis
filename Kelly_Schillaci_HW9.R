
#install.packages("kernlab")
library(kernlab)

#install.packages("e1071")
library(e1071)

#install.packages("gridExtra")
library(gridExtra)

library(ggplot2)

# Step 1: Load the data - use the airquality dataset
aq <- airquality
aq <- na.omit(aq)

# Step 2: Create a train and test datasets 
nrows <- nrow(aq)            # count the number of rows in the dataset
cutPoint <- floor(nrows/3*2) # take 2/3 of the dataset
rand <- sample(1:nrows)      # generate a random index
aq.train <- aq[rand[1:cutPoint],]        # create a train dataset
aq.test <- aq[rand[(cutPoint+1) : nrows],] # create a test dataset

# Step 3: Build a Model using KSVM & visualize the results
# 1) Build a model (using the 'ksvm' function, trying to predict ozone).
# You can use all the possible attributes, or select the attributes that
# you think would be the most helpful.
model.best <- ksvm(Ozone ~ Solar.R + Temp + Wind, data = aq.train) #use this because it has the lowest root mean squared error
model <- ksvm(Ozone ~ Solar.R + Temp, data = aq.train)
model <- ksvm(Ozone ~ Solar.R + Wind, data = aq.train)
model <- ksvm(Ozone ~ Wind + Temp, data = aq.train)


# 2) Test the model on the testing dataset, and compute the Root Mean Squared Error
pred <- predict(model.best, aq.test)
aq.test$Ozone.pred <- pred
aq.test$pred.error <- aq.test$Ozone - aq.test$Ozone.pred
sqrt(mean((aq.test$pred.error)^2))
# 3) Plot the results. Use a scatter plot. Have the x-axis represent temperature,
# the y-axis represent wind, the point size and color represent the error, as defined
# by the actual ozone level minus the predicted ozone level).
ksvm.chart <- ggplot(aq.test, aes(x = Temp, y = Wind, size = pred.error, color = pred.error)) + 
  geom_point()

# 4) Compute models and plot the results for 'svm'(in the e1071 package) and 'lm'.
# Generate similar charts for each model
# using the svm() function
model2.best <- svm(Ozone ~ Solar.R + Wind + Temp, data = aq.train, kernel="radial") #use this because it has the lowest root mean squared error
model2 <- svm(Ozone ~ Solar.R + Wind, data = aq.train)
model2 <- svm(Ozone ~ Solar.R + Temp, data = aq.train)
model2 <- svm(Ozone ~ Wind + Temp, data = aq.train)


pred2 <- predict(model2.best, aq.test)
aq.test$Ozone.pred2 <- pred2
aq.test$pred.error2 <- aq.test$Ozone - aq.test$Ozone.pred2
sqrt(mean((aq.test$pred.error2)^2))
svm.chart <- ggplot(aq.test, aes(x = Temp, y = Wind, size = pred.error2, color = pred.error2)) + 
  geom_point()

# using the lm() function
model3.best <- lm(Ozone ~ Solar.R + Wind + Temp, data = aq.train) #use this because it has the lowest root mean squared error
model3 <- lm(Ozone ~ Solar.R + Temp, data = aq.train)
model3 <- lm(Ozone ~ Solar.R + Wind, data = aq.train)
model3 <- lm(Ozone ~ Wind + Temp, data = aq.train)


pred3 <- predict(model3.best, aq.test)
aq.test$Ozone.pred3 <- pred3
aq.test$pred.error3 <- aq.test$Ozone - aq.test$Ozone.pred3
sqrt(mean((aq.test$pred.error3)^2))
lm.chart <- ggplot(aq.test, aes(x = Temp, y = Wind, size = pred.error3, color = pred.error3)) + 
  geom_point()

# 5) Show all three results (charts) in one window, using the grid.arrange function
grid.arrange(ksvm.chart, svm.chart, lm.chart, nrow = 1)

# Step 4: Create a 'goodOzone' variable. This variable should be either 0 or 1.
# It should be 0 if the ozone is below the average for all the data observations,
# and 1 if it is equal to or above the average ozone observed.
aq.train$goodOzone <- ifelse(aq.train$Ozone >= mean(aq$Ozone), 1, 0)
aq.test$goodOzone <- ifelse(aq.test$Ozone >= mean(aq$Ozone), 1, 0)

# Step 5: See if we can do a better job predictings 'good' and 'bad' days
# 1) Build a model (using the 'ksvm' function, trying to predic 'goodOzone').
# You can use all the possible attributes, or select the attributes that you think
# are most helpful
goodOzone.model <- ksvm(goodOzone ~ Solar.R + Wind + Temp, data = aq.train)

# 2) Test the model on the testing dataset, and compute the percent of 'goodOzone' that
# was correctly predicted
goodOzone.pred <- predict(goodOzone.model, aq.test)           # predict goodOzone
result.tbl <- table(aq.test$goodOzone, round(goodOzone.pred)) # save the result
(result.tbl[1,1] + result.tbl[2,2]) / sum(result.tbl)         # percent of correctly predicted

# 3) Plot the results. Use a scatter plot. Have the x-axis represent temperature, the
# y-axis represent wind, the shape represent what was predicted (good or bad day), the
# color represent the actual value of'goodOzone' (i.e if the actual ozone level was good)
# and the size represent if the prediction was correct (larger symbols should be the 
# observatios the model got wrong)
aq.test$goodOzone.pred <- round(goodOzone.pred)
aq.test$correctly.pred <- ifelse(aq.test$goodOzone.pred == aq.test$goodOzone, 1, 0)
ksvm.goodOzone.chart <- ggplot(aq.test, aes(x = Temp, y = Wind, 
                                            shape = factor(goodOzone.pred), 
                                            color = factor(goodOzone), 
                                            size = -correctly.pred)) + geom_point() + ggtitle("Using ksvm algorithm")

# 4) Compute models and plot the results for 'svm' (in the e1071 package) and 'np' (Naive Bayes, also in the e1071 package)
# using the svm() function
goodOzone.model2 <- svm(goodOzone ~ Solar.R + Wind + Temp, data = aq.train)
goodOzone.pred2 <- predict(goodOzone.model2, aq.test)
result.tbl2 <- table(aq.test$goodOzone, round(goodOzone.pred2))
(result.tbl2[1,1] + result.tbl2[2,2]) / sum(result.tbl2)
aq.test$goodOzone.pred2 <- round(goodOzone.pred2)
aq.test$correctly.pred2 <- ifelse(aq.test$goodOzone.pred2 == aq.test$goodOzone, 1, 0)
svm.goodOzone.chart <- ggplot(aq.test, aes(x = Temp, y = Wind, 
                                           shape = factor(goodOzone.pred2), 
                                           color = factor(goodOzone), 
                                           size = -correctly.pred2)) + geom_point() + ggtitle("Using svm algorithm")

# using the naiveBayes() function
aq.train$goodOzone <- as.factor(aq.train$goodOzone) # convert goodOzone to factor
aq.test$goodOzone <- as.factor(aq.test$goodOzone)   # convert goodOzone to factor
goodOzone.model3 <- naiveBayes(goodOzone ~ Solar.R + Wind + Temp, data = aq.train)
goodOzone.pred3 <- predict(goodOzone.model3, aq.test)           # predict goodOzone
result.tbl3 <- table(aq.test$goodOzone, (goodOzone.pred3))      # save the result
(result.tbl3[1,1] + result.tbl3[2,2]) / sum(result.tbl3)
aq.test$goodOzone.pred3 <- goodOzone.pred3
aq.test$correctly.pred3 <- ifelse(aq.test$goodOzone.pred3 == aq.test$goodOzone, 1, 0)
nb.goodOzone.chart <- ggplot(aq.test, aes(x = Temp, y = Wind, 
                                          shape = factor(goodOzone.pred3), 
                                          color = factor(goodOzone), 
                                          size = -correctly.pred3)) + geom_point() + ggtitle("Using Naive Bayes algorithm")

# 5) Show all the three results(charts) in one window, using the grid.arrange function (have 2 charts in one row)
grid.arrange(ksvm.goodOzone.chart, svm.goodOzone.chart, nb.goodOzone.chart, nrow = 1)

# Step 6 - Which are the best Models for this data? review what you have done and state which is the best and why
#For predicting Good ozone:  The Naive Bayes algorithm results were over 97% accurate, the SVM and KSVM both correctly predicted 89%.
#The orignal models of predicting ozone level, the SVM model had the lowest Root Mean Squared Error of 19.75 which makes it the best model.  The KSVM Root Mean Squared Error was 20.33 and LM was 21.95, making it the worst.
