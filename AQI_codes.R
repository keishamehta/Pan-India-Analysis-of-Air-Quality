#ANOVA
library(readxl)
data = read_excel("ANOVA.xlsx", sheet = 3)

# converting data into factor
data$Station = as.factor(data$Station)

model = lm(data$Obs1~data$Station)
anova(model)

# Tukey's HSD
TukeyHSD(aov(data$Obs1~data$Station))

plot(model$residuals)
plot(model)

library("car")
leveneTest(model)

shapiro.test(model$residuals)

model = lm(data$Obs1~data$Station)
anova(model)

#--------------------------------------------------------------------------
# Linear and Multinomial Logistic Regression Models

# Importing Dataset and Libraries -----------------------------------------

a = read.csv(file.choose(),header=T)
View(a)

library(olsrr)
library(MASS)
library(lmtest)
library(car)
library(ggplot2)
library(drc)
library(corrplot)

# Assumption 1: Normality ---------------------------------------------------------------

#fit linear regression model
model1 <- lm(a$AQI~a$TC+a$PD+a$PP+a$T+a$H+a$WS+a$AP+a$E+a$C+a$LAT+a$LON)
hist(a$AQI)
hist(model1$residuals)

#find optimal lambda for Box-Cox transformation 
bc <- boxcox(a$AQI~a$TC+a$PD+a$PP+a$T+a$H+a$WS+a$AP+a$E+a$C+a$LAT+a$LON)
lambda <- bc$x[which.max(bc$y)]
lambda 

#fit new linear regression model using log transformation on DV
a$lnaqi = log(a$AQI)
model2 <- lm(a$lnaqi~a$TC+a$PD+a$PP+a$T+a$H+a$WS+a$AP+a$E+a$C+a$LAT+a$LON)
hist(a$lnaqi)
hist(model2$residuals)

#Shapiro-Wilk test for Normality
#H0: Residuals are normally distributed        
#H1: Residuals are non-normally distributed
shapiro.test(model2$residuals)
#p-value = 0.15
#p-value > alpha
#Fail to reject H0
#Residuals are normally distributed

# Assumption 2: Linearity ---------------------------------------------------------------

ols_plot_reg_line(a$TC,log(a$AQI))
ols_plot_reg_line(a$PD,log(a$AQI))
ols_plot_reg_line(a$PP,log(a$AQI))
ols_plot_reg_line(a$T,log(a$AQI))
ols_plot_reg_line(a$H,log(a$AQI))
ols_plot_reg_line(a$WS,log(a$AQI))
ols_plot_reg_line(a$AP,log(a$AQI))
ols_plot_reg_line(a$E,log(a$AQI))
ols_plot_reg_line(a$LAT,log(a$AQI))
ols_plot_reg_line(a$LON,log(a$AQI))
#Moderate linear relationships can be seen in all

# Assumption 3: Multicollinearity -------------------------------------------------------

ols_coll_diag(model2)
#VIF for LAT >5
z = cor(a)
corrplot(z,method = "number")

#Remove the highly correlated variable (Latitude)
model3 <- lm(a$lnaqi~a$TC+a$PD+a$PP+a$T+a$H+a$WS+a$AP+a$E+a$C+a$LON)
ols_coll_diag(model3)
#VIF for all independent variables <5
#Multicollinearity is not present

# Regression Analysis -----------------------------------------------------

#Qualititative Variable
a$C = as.factor(a$C)

ols_regress(model3)

#Stepwise Regression based on p-values
ols_step_both_p(model3)

#fit new regression model based on stepwise regression
new_model <- lm(a$lnaqi~a$E+a$C+a$PP+a$LON+a$H+a$PD+a$T)
ols_regress(new_model)

# Assumption 4: Homoskedasticity --------------------------------------------------------

plot(new_model,1)
#approximately horizontal straight line might indicate variance is constant

#Breusch-Pagan Test for Heteroskedasticity
#H0: the variance is constant (Homoskedastic)          
#H1: the variance is not constant (Heteroskedastic)
bptest(new_model)
#p-value = 0.3391
#p-value > alpha
#Fail to reject H0
#Variance is constant (Homoskedastic)

# Assumption 5: Autocorrelation ------------------------------------------------------------

plot(new_model$residuals)
#No pattern in residuals

#Durbin-Watson Test
#H0:There is no correlation among the residuals.
#H1:The residuals are autocorrelated.
durbinWatsonTest(new_model)
#p-value = 0.358
#p-value > alpha
#Fail to reject H0
#There is no correlation among the residuals
#Autocorrelation = -0.0795786

# Testing Model for Panaji ---------------------------------------------

coeffs = coefficients(new_model)

e = 7
c = 1
pp = 96.37
lon = 73.8278
h = 0.42
pd = 1111.583
t = 31

AQIpanaji = coeffs[1] + coeffs[2]*e + coeffs[3]*c + coeffs[4]*pp + coeffs[5]*lon + 
  coeffs[6]*h + coeffs[7]*pd + coeffs[8]*t
AQIpanaji
exp(AQIpanaji)

# Multinomial Logisitic Regression -----------------------------------------

df = read.csv("C:/Keisha/AQI Files/Regression.csv")
hist(df$AQI, 10)

df$Bands = as.factor(df$Bands)
df$TC = scale(df$TC)
df$PD = scale(df$PD)
df$PP = scale(df$PP)
df$T = scale(df$T)
df$H = scale(df$H)
df$WS = scale(df$WS)
df$AP = scale(df$AP)
df$E = scale(df$E)
df$C = as.factor(df$C)
df$LAT = scale(df$LAT)
df$LON = scale(df$LON)
df = subset(df, select = -c(AQI))

library(dplyr)
train = sample_n(df, 170*0.8)

library(nnet)
model_fit = multinom(Bands~TC+PD+PP+T+H+WS+AP+E+C+LAT+LON, data = train)
summary(model_fit)
z = summary(model_fit)$coefficients/summary(model_fit)$standard.errors
p = (1 - pnorm(abs(z), 0, 1)) * 2
pred = predict(model_fit, df)
tab = table(pred, df$Bands); tab
sum(diag(tab))/sum(tab)
