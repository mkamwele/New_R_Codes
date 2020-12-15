cars
cor.test(cars$speed,cars$dist)
min(cars$speed)
##linear regression
linearmod <- lm(dist~speed,data=cars)
summary(linearmod)
summary(aov(cars$dist~cars$speed,data=cars))
predict(linearmod ,cars)
cars$predvar=predict(linearmod,cars)
#mse
cars <- cars %>%
  mutate(diff=(dist-predvar)^2)
mean(cars$diff)
##########################
marketing <- read.csv("R training/DirectMarketing (1).csv")
marketing
lm(AmountSpent~Gender,data=marketing)
t.test(AmountSpent~Gender,data=marketing)

lm(AmountSpent~Age,data=marketing)
anova <- aov(AmountSpent~Age,data=marketing)

TukeyHSD(anova)

