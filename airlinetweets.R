getwd()
install_github("jacobkap/fastDummies")

#======truncating the data to get only useful varaibles
airlineDF <- read.csv("airlinetweets.csv")
summary(airlineDF)
View(airlineDF)
dim(airlineDF)
data.binary <- airlineDF
data.binary <- fastDummies::dummy_cols(data.binary, select_columns = c("airline","user_timezone", "airline_sentiment"))
View(data.binary)

plot(airlineDF$airline_sentiment, airlineDF$airline_sentiment_confidence)
which(is.na())
lmAirlineSentiment <- lm(data.binary$airline_sentiment_negative ~ data.binary$airline_Southwest + data.binary$airline_American + data.binary$airline_Delta + data.binary$airline_United + data.binary$`airline_US Airways` + data.binary$`airline_Virgin America`)
lmTimeZone <- lm(data.binary$airline_sentiment_negative ~ data.binary$`user_timezone_Eastern Time (US & Canada)` + data.binary$`user_timezone_Mountain Time (US & Canada)` + data.binary$`user_timezone_Pacific Time (US & Canada)` + data.binary$`user_timezone_Central Time (US & Canada)` + data.binary$`user_timezone_Atlantic Time (Canada)` + data.binary$user_timezone_London)
summary(lmAirline)
summary(lmTimeZone)

cor(data.binary$airline_sentiment_negative, data.binary$`airline_Virgin America`)
cor(data.binary$airline_sentiment_negative, data.binary$airline_United)
cor(data.binary$airline_sentiment_negative, data.binary$airline_Delta)
cor(data.binary$airline_sentiment_negative, data.binary$airline_Southwest)
cor(data.binary$airline_sentiment_negative, data.binary$airline_American)
cor(data.binary$airline_sentiment_negative, data.binary$`airline_US Airways`)

dim(data.binary)
summary(data.binary)

datareal <- airlineDF[ , -which(names(airlineDF) %in% c("airlineDF$tweet_id", "airlineDF$negativereason_gold", "airlineDF$tweet_coord", "airlineDF$tweet_location", "airlineDF$airline_sentiment_gold", "airlineDF$tweet_created", "airlineDF$user_timezone", "airlineDF$text", "airlineDF$name"))]
datareal <- airlineDF[, -c(1,3,5,7,8,9,11,12,13,14)]
names(datareal)
dim(datareal)
which(is.na(datareal))
# creating dummy varaibles, ending up with 24 variables
realDF <- fastDummies::dummy_cols(datareal, select_columns = c("airline","user_timezone", "airline_sentiment", "negativereason"))
cor(realDF)
which(is.na(realDF))
realDF2 <- realDF[ , -c(1,2,3,5)]

dim(realDF2)
realDF3 <- realDF2[ , which(names(realDF2) %in% c("airline_Virgin America", "airline_United", "airline_Delta", "airline_Southwest", "airline_American", "airline_US Airways", "retweet_count", 
                                                  "user_timezone_Eastern Time (US & Canada)", "user_timezone_Pacific Time (US & Canada)", "user_timezone_London", "user_timezone_Atlantic Time (Canada)",
                                                  "user_timezone_Central Time (US & Canada)", "user_timezone_Mountain Time (US & Canada)", "airline_sentiment_negative",
                                                  "negativereason_Bad Flight", "negativereason_Can't Tell", "negativereason_Late Flight", "negativereason_Customer Service Issue", "negativereason_Flight Booking Problems",
                                                  "negativereason_Lost Luggage", "negativereason_Flight Attendant Complaints", "negativereason_Cancelled Flight", "negativereason_Damaged Luggage", "negativereason_longlines"))]
View(realDF3)
dim(realDF3)
# correlation coefficients
cor(realDF3$airline_sentiment_negative,realDF3)
which(is.na(realDF3$`airline_US Airways`))
which(is.na(realDF3))

#ols model
lmDF3 <- lm(airline_sentiment_negative ~. , data = realDF3)
summary(lmDF3)

#linear model airline
lmAirline3 <- lm(airline_sentiment_negative ~ `airline_Virgin America` + airline_United + airline_Delta + airline_Southwest + airline_American + `airline_US Airways`, data = realDF3)
summary(lmAirline3)

#logit model airline
glmAirline3 <- glm(airline_sentiment_negative ~ `airline_Virgin America` + airline_United + airline_Delta + airline_Southwest + airline_American + `airline_US Airways`, data = realDF3)
summary(glmAirline3)
exp(glmAirline3$coefficients)

#linear model time zone
lmTimeZoneDF3 <- lm(airline_sentiment_negative ~ `user_timezone_Eastern Time (US & Canada)` + `user_timezone_Pacific Time (US & Canada)`+ `user_timezone_Central Time (US & Canada)`+ `user_timezone_Atlantic Time (Canada)`+ `user_timezone_Mountain Time (US & Canada)`+ user_timezone_London, data = realDF3)
summary(lmTimeZoneDF3)

#logit model time zone
logitTimeZone3 <- glm(airline_sentiment_negative ~ `user_timezone_Eastern Time (US & Canada)` + `user_timezone_Pacific Time (US & Canada)`+ `user_timezone_Central Time (US & Canada)`+ `user_timezone_Atlantic Time (Canada)`+ `user_timezone_Mountain Time (US & Canada)`+ user_timezone_London, data = realDF3)
summary(logitTimeZone3)
exp(logitTimeZone3$coefficients)
plot(logitTimeZone3)

#creating training and testing set and MSE formula
set.seed(1861)
trainSize <- 0.7
trainInd <- sample(1:nrow(realDF3), size = floor(nrow(realDF3) * trainSize))
realDF3Train <- realDF3[trainInd, ]
realDF3Test <- realDF3[-trainInd, ]

MSE <- function(ytrue, ypreds){
  return(mean((ytrue - ypreds)^2))
}

# logit model best 
logitBest3 <- glm(airline_sentiment_negative ~ `negativereason_Customer Service Issue` + `negativereason_Late Flight` + `negativereason_Cancelled Flight` + `negativereason_Lost Luggage` + airline_Delta, data = realDF3Train)
summary(logitBest3)
help(exp)
exp(logitBest3$coefficients)
predLogit <- predict(logitBest3, realDF3Test)
MSElogitBest <- MSE(realDF3Test$airline_sentiment_negative, predLogit)
MSElogitBest

#lasso best
install.packages("glmnet")
library(useful)
library(glmnet)
myForm <- as.formula(airline_sentiment_negative ~.)
Xvar <- build.x(formula = myForm, data = realDF3Train)
Yvar <- build.y(formula = myForm, data = realDF3Train)
LassoRealDF3 <- cv.glmnet(x = Xvar, y = Yvar, alpha = 1)
coef(LassoRealDF3, s = "lambda.min")
coef(LassoRealDF3, s = "lambda.1se")

plot(LassoRealDF3)
LassoRealDF3

r2LassoReal3.1se <- LassoRealDF3$glmnet.fit$dev.ratio[which(LassoRealDF3$glmnet.fit$lambda == LassoRealDF3$lambda.1se)]
r2LassoReal3.1se

myFormula <- as.formula(airline_sentiment_negative~.)
Xvar2 <- build.x(formula = myFormula, data = realDF3Test)
Yvar2 <- build.y(formula = myFormula, data = realDF3Test)

lasso.pred.real3 <- predict(LassoRealDF3, s = 'lambda.1se', newx = Xvar2)
MSELasso <- MSE(realDF3Test$airline_sentiment_negative, lasso.pred.real3)
MSELasso

#================#
#never got to work, problems with naming conventions of columns
#random forest best
require('randomForest')
require('tree')
require('rpart')

realDF3Tree <- tree(airline_sentiment_negative ~ `negativereason_Customer Service Issue` + airline_Delta, data = airlineTrain, method = "class")

airlineTrain <- sample(1:nrow(realDF3), 9500)
View(realDF3)

randomForest3 <- randomForest(airline_sentiment_negative ~., data = realDF3, subset = airlineTrain)
randomForest3

oob.err=double(13)
test.err=double(13)
for(mtry in 1:13)
{
  rF3 <- randomForest(airline_sentiment_negative ~., data = realDF3, subset= airlineTrain, mtry = 3, ntree = 500)
  oob.err[mtry] = RF3$mse[500]
  pred <- predict(RF3, realDF3[-airlineTrain,])
  test.err[mtry] = with(realDF3[-airlineTrain,], mean((medv-pred)^2))
  cat(mtry," ")
}

# matplot(x,y) plots the columns of one matrix against the columns of another
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))

