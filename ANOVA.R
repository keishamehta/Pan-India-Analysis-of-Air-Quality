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














