library(tidyverse)
library(MASS)

adv <- read_csv('Advertising.csv')

adv2 = adv[-131,]
adv2 = adv2[-155,]


#  feature scaling
adv.scaled <-as.data.frame(scale(adv2))


adv$X1 <- NULL
pairs(adv)
pairs(adv.scaled)

model <-lm(Sales~TV+Radio+Newspaper,data=adv2)
model2 <-lm(log(Sales)~TV+Radio+Newspaper,data=adv2)
model3 <-lm(Sales~I(TV^2)+TV+Radio:TV,data=adv2)
model4 <-lm(Sales~TV+Radio+Newspaper,data=adv.scaled)
model5 <-lm(Sales~TV+Radio+Radio:TV,data=adv.scaled)
model6 <-lm(Sales~TV+Radio+Radio:TV,data=adv2)


model <-lm(Sales^1.7~TV+Radio:TV,data=adv2)
round(cor(adv,method = "pearson"),3)

summary(model)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
plot(model)
plot(model2)
plot(model3)
plot(model4)
plot(model5)
plot(model6)


par(mfrow=c(2,2))
par(mfrow=c(1,1))
plot(model)

confint(model)

vif(model)

kappa(model)


new.adv <- data.frame('TV'=c(149, 149),'Radio'=c(22, 60), 'Newspaper'=c(25, 25))


predict(model,new.adv, interval='confidence')

bc<-boxcox(model)
bc$x[which.max(bc$y)]


adv2$error = model$residuals^2
adv2$fit = model$fitted.values

model5 <-lm(error~fit,data=adv2)

adv2$sig = model5$fitted.values

model4 <-lm(Sales~TV+Radio+Newspaper,data=adv2, weights = 1/sig)
summary(model4)
plot(model4)

plot(bodymass, height, pch = 16, cex = 1.3, col = "blue", main = "HEIGHT PLOTTED AGAINST BODY MASS", xlab = "BODY MASS (kg)", ylab = "HEIGHT (cm)")
abline(lm(height ~ bodymass))

