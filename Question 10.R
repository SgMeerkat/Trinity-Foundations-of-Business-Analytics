#upload the data using the upload button first
library(readxl)
file_path <- "C:/Users/Dell/Downloads/IPL Pricing_HBR_Dataset.xls"

#Read the Excel file into a data frame
IPL_data <- read_excel(file_path)

#Check variable names in your dataset
variable_names <- names(IPL_data)
print(variable_names)

#Q10 A

library(dplyr)

#create a new data frame with selected variables
model_data <- IPL_data[c("Sold Price(US$)", "Base Price(US$)", "RUNS-S", "WKTS", "INDIA", "L25")]

#Fit the multiple regression model
model <- lm(`Sold Price(US$)` ~ `Base Price(US$)` + `RUNS-S` + `WKTS` + `INDIA` + `L25`, data = model_data)

# Display the summary of the regression model
summary(model)
library(ggplot2)

#Create a data frame with the actual and predicted prices
plot_data <- data.frame(Actual = model_data$`Sold Price(US$)`, Predicted = predict(model))
library(ggplot2)

#Check 1
# Create a scatterplot of actual versus predicted sold prices
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual Sold Price", y = "Predicted Sold Price") +
  ggtitle("Actual vs. Predicted Sold Prices")

#check2
#Create a Q-Q plot for residuals to check for normality
qqnorm(residuals(model))
qqline(residuals(model))

#check3
library(car)

#Check VIF for multicollinearity
vif(model)

#check4
#Check for autocorrelation using Durbin-Watson test
library(car)
durbinWatsonTest(model)



#Q10B
#Calculate the 'Difference' variable
IPL_data <- IPL_data %>%
  mutate(Difference = `Sold Price(US$)` - `Base Price(US$)`)

#Create a data frame with selected variables, excluding the constant term
model_data <- data.frame(
  Difference = IPL_data$Difference,
  `RUNS-S` = IPL_data$`RUNS-S`,
  `WKTS` = IPL_data$`WKTS`,
  `INDIA` = IPL_data$`INDIA`,
  `ODI-WKTS` = IPL_data$`ODI-WKTS`,
  `L25` = IPL_data$`L25`
)

#Fit the multiple regression model without a constant term
model <- lm(Difference ~ `RUNS-S` + `WKTS` + `INDIA` + `ODI-WKTS` + `L25` - 1, data = model_data)

#Display the summary of the regression model
summary(model)

#Check 1
#Create a scatterplot to check linearity
ggplot(model_data, aes(x = model$fitted.values, y = model$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Linearity Check")

#Check 2
#Create a Q-Q plot for residuals to check normality
qqnorm(model$residuals)
qqline(model$residuals)

#CHECK 3
#Check VIF for multicollinearity
vif(model)

#CHECK 4
durbinWatsonTest(model)


#Q10D
# Calculate the 'Difference' variable
IPL_data <- IPL_data %>%
  mutate(Difference = `Sold Price(US$)` - `Base Price(US$)`)

#Create a data frame with only main effect variables
main_data <- data.frame(
  Difference = IPL_data$Difference,
  `RUNS-S` = IPL_data$`RUNS-S`,
  `INDIA` = IPL_data$`INDIA`,
  `ODI-RUNS` = IPL_data$`ODI-RUNS`,
  `L25` = IPL_data$`L25`
)

#Create a data frame with interaction terms separately
interaction_data <- data.frame(
  `SR -B` = IPL_data$`SR -B`,
  `BAT:SR-B` = IPL_data$`BAT:SR-B`,
  `BAT:RUNS-S` = IPL_data$`BAT:RUNS-S`,
  `BAT:ODI-RUNS` = IPL_data$`BAT:ODI-RUNS`,
  `BOW:WK-I` = IPL_data$`BOW:WK-I`,
  `BOW:ECON` = IPL_data$`BOW:ECON`,
  `BOW:SR-BL` = IPL_data$`BOW:SR-BL`,
  `ALL:SR-B` = IPL_data$`ALL:SR-B`,
  `ALL:ECON` = IPL_data$`ALL:ECON`,
  `ALL:SR-BL` = IPL_data$`ALL:SR-BL`,
  `BOW:WK-O` = IPL_data$`BOW:WK-O`
)

#Fit Model 3 by adding interaction terms to the main model
model3 <- lm(Difference ~ . - 1, data = main_data)

#Display the summary of Model 3
summary(model3)



#Now 'data' contains your Excel data

#after you upload the MODIFIED data
#check whether the data has any missing values
#also check the mean, variance, 


summary(IPL_data)
str(IPL_data)
#remove any missing values in Y outcome variable
# and overwrite on the same file
IPL_data<-IPL_data[!is.na(IPL_data$`Sold Price(US$)`),]

#do sumamry again
summary(IPL_data)

attach(IPL_data)

# MODEL 01
# perform histogram to check whether the data looks normal
hist(IPL_data$`Sold Price(US$)`)
hist(IPL_data$`SR -B`)


summary(`Sold Price(US$)`)
#pairwise correlation check
cor(`Sold Price(US$)`,`SR -B`)

model01<-lm(`Sold Price(US$)`~ `SR -B`,
                                data = IPL_data)

summary(model01)

#CHECK 1
#Plot actual Y-points versus predictions
library(ggplot2)
qplot(IPL_data$`SR -B`,
      IPL_data$`Sold Price(US$)`, 
      main="Regression Line",
      xlab="Batting Strike Rate",
      ylab="Price of Player") +
  geom_abline(slope = coef(model01)[2], 
              intercept = coef(model01)[1],
              colour = "red")

#Plot actual Y-points versus predictions
plot(predict(model01),
     IPL_data$`Sold Price(US$)`,
     xlab="predicted",
     ylab="actual")
abline(a=0,b=1)
#get residuals 
res1 <- resid(model01)
#produce residual vs. fitted plot , 
# which is helpful for visually detecting heteroscedasticity
# e.g. a systematic change in the spread of residuals over a range of values. 
plot(fitted(model01), 
     res1,     
     xlab="fitted Y",
     ylab="residuals")

#add a horizontal line at 0 
abline(0,0)

#CHECK 2 - produce a Q-Q plot, 
# which is useful for determining if the residuals follow a normal distribution. 
# If the data values in the plot fall along a roughly straight line at a 45-degree angle, 
# then the data is normally distributed.

#create Q-Q plot for residuals
qqnorm(res1)

#add a straight diagonal line to the plot
qqline(res1)


#CHECK 3 - whether VIF is greater than 4
library(car)
vif(model01) 
# this will give an error because only one independent variable
#so no need of VIF check or correlation check

#CHECK 4 - DUrbin Watson check for multi-collinearity
durbinWatsonTest(model01)
#If p-value is less than 0.05
# we can conclude that the residuals 
# in this regression model are autocorrelated.



# MODEL 02

#pairwise correlation check
cor(IPL_data[, c('Sold Price(US$)','SR -B', 'SIXERS', 'RUNS-S')])


model02<-lm(`Sold Price(US$)`~ `SR -B` + SIXERS + `RUNS-S`,
            data = IPL_data)

summary(model02)

#CHECK 1
#Plot actual Y-points versus predictions
library(ggplot2)
qplot(IPL_data$`RUNS-S`,
      IPL_data$`Sold Price(US$)`, 
      main="Regression Line",
      xlab="Runs Scored",
      ylab="Price of Player") +
  geom_abline(slope = coef(model02)[4], # choose slope for a significant variable
              intercept = coef(model02)[1],
              colour = "red")

#Plot actual Y-points versus predictions
plot(predict(model02),
     IPL_data$`Sold Price(US$)`,
     xlab="predicted",
     ylab="actual")
abline(a=0,b=1)
#get residuals 
res2 <- resid(model02)

#produce residual vs. fitted plot , 
# which is helpful for visually detecting heteroscedasticity
# e.g. a systematic change in the spread of residuals over a range of values. 
plot(fitted(model02), 
     res2,     
     xlab="fitted Y",
     ylab="residuals")

#add a horizontal line at 0 
abline(0,0)

#CHECK 2 - produce a Q-Q plot, 
# which is useful for determining if the residuals follow a normal distribution. 
# If the data values in the plot fall along a roughly straight line at a 45-degree angle, 
# then the data is normally distributed.

#create Q-Q plot for residuals
qqnorm(res2)

#add a straight diagonal line to the plot
qqline(res2)


#CHECK 3 multi-collinearity- whether VIF is greater than 4 
library(car)
vif(model02) 
 

#CHECK 4 - Durbin Watson check for autocorrelation
durbinWatsonTest(model02)
#If p-value is less than 0.05
# we can conclude that the residuals 
# in this regression model are autocorrelated
# otherwise not


#revised MODEL 2
model02a<-lm(`Sold Price(US$)`~ `SR -B` + `RUNS-S`,
            data = IPL_data)

summary(model02a)
vif(model02a)  # VIF is now low

#revised MODEL 2
model02b<-lm(`Sold Price(US$)`~ `SR -B` + SIXERS,
             data = IPL_data)

summary(model02b)
vif(model02b) # VIF is now low


#Using Stepwise regression for MODEL 2

library(MASS)

# Stepwise regression model to get the best set of predictors 
#at one shot
step.model02 <- stepAIC(model02, direction = "both", 
                      trace = TRUE) 
# can change trace to FALSE 
# if you do not want to see the intermediate results
summary(step.model)


# MODEL 03

#pairwise correlation check - anything alarming?
cor(IPL_data[, c('Sold Price(US$)',
                 'SR -B', 
                 'RUNS-S', 
                 'ODI-RUNS',
                 'WKTS')])
 

model03<-lm(`Sold Price(US$)`~ `SR -B` + `RUNS-S` + `ODI-RUNS` + WKTS,
            data = IPL_data)

summary(model03)

# Stepwise regression model to get the best set of predictors 
#at one shot
step.model03 <- stepAIC(model03, direction = "both", 
                      trace = FALSE)
summary(step.model03)


#CHECK 1
#Plot actual Y-points versus predictions
library(ggplot2)
qplot(IPL_data$`ODI-RUNS`,
      IPL_data$`Sold Price(US$)`, 
      main="Regression Line",
      xlab="ODI Runs",
      ylab="Price of Player") +
  geom_abline(slope = coef(model03)[4], # choose slope for a significant variable
              intercept = coef(model03)[1],
              colour = "red")

#Plot actual Y-points versus predictions
plot(predict(model03),
     IPL_data$`Sold Price(US$)`,
     xlab="predicted",
     ylab="actual")
abline(a=0,b=1)

#get residuals 
res3 <- resid(model03)

#produce residual vs. fitted plot , 
# which is helpful for visually detecting heteroscedasticity
# e.g. a systematic change in the spread of residuals over a range of values. 
plot(fitted(model03), 
     res3,     
     xlab="fitted Y",
     ylab="residuals")

#add a horizontal line at 0 
abline(0,0)

#CHECK 2 - produce a Q-Q plot, 
# which is useful for determining if the residuals follow a normal distribution. 
# If the data values in the plot fall along a roughly straight line at a 45-degree angle, 
# then the data is normally distributed.

#create Q-Q plot for residuals
qqnorm(res3)

#add a straight diagonal line to the plot
qqline(res3)


#CHECK 3 multi-collinearity- whether VIF is greater than 4 
library(car)
vif(model03) 


#CHECK 4 - Durbin Watson check for autocorrelation
durbinWatsonTest(model03)
#If p-value is less than 0.05
# we can conclude that the residuals 
# in this regression model are autocorrelated
# otherwise not


# MODEL 03 with revised response variable - SQRT of (S-B)

#pairwise correlation check - anything alarming?
cor(IPL_data[, c("SQRT(S-B)",
                 'SR -B', 
                 'RUNS-S', 
                 'ODI-RUNS',
                 'WKTS')])


model03a<-lm(`SQRT(S-B)`~ `SR -B` + `RUNS-S` + `ODI-RUNS` + WKTS,
            data = IPL_data)

summary(model03a)

# Stepwise regression model to get the best set of predictors 
#at one shot
step.model03a <- stepAIC(model03a, direction = "both", 
                        trace = FALSE)
summary(step.model03a)


#CHECK 1
#Plot actual Y-points versus predictions
library(ggplot2)
qplot(IPL_data$`RUNS-S`,
      IPL_data$`SQRT(S-B)`, 
      main="Regression Line",
      xlab="Runs",
      ylab="Price of Player") +
  geom_abline(slope = coef(model03a)[3], # choose slope for a significant variable
              intercept = coef(model03a)[1],
              colour = "red")

#Plot actual Y-points versus predictions
plot(predict(model03a),
     IPL_data$`SQRT(S-B)`,
     xlab="predicted",
     ylab="actual")
abline(a=0,b=1)

#get residuals 
res3a <- resid(model03)

#produce residual vs. fitted plot , 
# which is helpful for visually detecting heteroscedasticity
# e.g. a systematic change in the spread of residuals over a range of values. 
plot(fitted(model03a), 
     res3a,     
     xlab="fitted Y",
     ylab="residuals")

#add a horizontal line at 0 
abline(0,0)

#CHECK 2 - produce a Q-Q plot, 
# which is useful for determining if the residuals follow a normal distribution. 
# If the data values in the plot fall along a roughly straight line at a 45-degree angle, 
# then the data is normally distributed.

#create Q-Q plot for residuals
qqnorm(res3a)

#add a straight diagonal line to the plot
qqline(res3a)


#CHECK 3 multi-collinearity- whether VIF is greater than 4 
library(car)
vif(model03a) 


#CHECK 4 - Durbin Watson check for autocorrelation
durbinWatsonTest(model03a)
#If p-value is less than 0.05
# we can conclude that the residuals 
# in this regression model are autocorrelated
# otherwise not



 