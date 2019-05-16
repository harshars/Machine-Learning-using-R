# Objective
# Closing stock prices for microsoft, google and Apple
# Increase is TRUE if Apple's Price increase for previous Day
# Objective : Predict whether price of Apple stock will increase or decrease.



library(class)
library(dplyr)
library(lubridate)

stocks <- read_csv("C:/Users/jasksing/Desktop/segmentation COE/segmentation COE/cd2/stocks.csv")


set.seed(100)

stocks$Date <- ymd(stocks$Date)
stocksTrain <- year(stocks$Date) < 2014

# training set: It will consist of the prices of stocks of Apple, Google, and Microsoft on the previous day
predictors <- cbind(lag(stocks$Apple, default = 210.73),
                    lag(stocks$Google, default = 619.98),
                    lag(stocks$MSFT, default = 30.48))

# For k=1
prediction <- knn(predictors[stocksTrain, ],
                  predictors[!stocksTrain, ],
                  stocks$Increase[stocksTrain], k = 1)


# Measuring accuracy
table(prediction, stocks$Increase[!stocksTrain])

mean(prediction == stocks$Increase[!stocksTrain])


# We can use a for loop to see how the algorithm performs for different values of k
accuracy <- rep(0, 10)
k <- 1:10
for(x in k){
  prediction <- knn(predictors[stocksTrain, ], predictors[!stocksTrain, ],
                    stocks$Increase[stocksTrain], k = x)
  accuracy[x] <- mean(prediction == stocks$Increase[!stocksTrain])
}

plot(k, accuracy, type = 'b')

# we see that maximum accuracy is obtained when k=5
