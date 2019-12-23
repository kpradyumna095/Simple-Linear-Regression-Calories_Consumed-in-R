#Housekeeing
#Import data set
library(readr)
wg_cc <- read_csv("E:/data sci/excelr/Assignments/Simple Linear Regression/calories_consumed.csv")

View(wg_cc) #View dataset

summary(wg_cc) # Summary of data/EDA

#Scatterplot of input Vs otput
plot( wg_cc$`Calories Consumed`,wg_cc$`Weight gained (grams)`)  # plot(X,Y)
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is positive

#attached dataset
attach(wg_cc)
 
#Correlation between output to input
cor(`Calories Consumed`,`Weight gained (grams)`) #cor(x,y)
#from value of correlation coe.(r) we can say that very good correlation between o/p & i/p

# Simple Linear Regression model
reg <- lm(`Weight gained (grams)`~ `Calories Consumed`) # lm(Y ~ X)

#Summary of regression model
summary(reg)
#first thing is that variable is siginificant as value is less than 0.05
#R^2 value is very good as it is more than 0.80 so we can say that model is bestfit as of now
#We can write eq. as WG=-625.75+0.42016(CC)

#check fitted values(predicted)
reg$fitted.values
reg$residuals

#but we have to check prdicted values 
pred <- predict(reg)

#check for associated errors
reg$residuals
sum(reg$residuals)
#check for mean of sum of errors is equal to 0
mean(reg$residuals)
hist(reg$residuals) # check errors are normally distributed or not.

#Check for RMSE value
sqrt(sum(reg$residuals^2)/nrow(wg_cc))  #RMSE

sqrt(mean(reg$residuals^2))

#interval for 5% of confidence
confint(reg,level=0.95)

predict(reg,interval="predict")

#visualising model
library(ggplot2)

ggplot(data = wg_cc, aes(x = `Calories Consumed`, y =`Weight gained (grams)`)) + 
  geom_point(color='blue') +
  geom_line(color='green',data = wg_cc, aes(x=`Calories Consumed`, y=pred))

# From all above  value or correlatio coe.r is 0.94 which is good ,
# function is linear in nature i.e. lm(WG ~ CC), 
# Coe. are significant and coe.of Determination value is R^2 is 0.89 which is also good
# sum of errors is 6.750156e-14 which is almost 0 ans errors are alost normally distributed.
# RMSE value is 103.30 which is nearest to lower range value of weight gain
# so model is best fit

