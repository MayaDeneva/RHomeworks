fish <- read.csv("FishDatabase.csv")
###1
library(dplyr)
fish$Species <- factor(fish$Species)
str(fish)

heavierThan100 <- filter(fish, fish$Weight > 100 & fish$Species == 'Perch')
nrow(heavierThan100) / nrow(fish)
nrow(heavierThan100) / nrow(filter(fish, fish$Species == 'Perch'))

install.packages("UsingR")


##1.2
sorted_data <- fish[order(fish$Weight, decreasing = TRUE), ]
View(sorted_data)
unique(sorted_data$Species[1:8])



attach(fish)
##1.3
library(ggplot2)
plot(fish$Species, fish$Weight)
ggplot(Species~Weight, x = Species, y = Weight)

ggplot(fish, aes(Species,Weight, color = 'red')) + geom_line()

##



###2.1
bream <- fish[Species == 'Bream',]
View(bream)
shapiro.test(bream$Weight)
ggplot(bream, aes(x = Weight)) + geom_bar()
##2.2
var(bream$Weight)
sd(bream$Weight)
mean(bream$Weight)


##3
t.test(bream$Weight, conf.level = 0.92)

##4.1

roach<- fish[Species == 'Roach',]
View(roach)
t.test(roach$Width, bream$Width, alternative = "greater", na.rm = TRUE)


##4.2
pikeperch <- fish[Species == 'Pike' | Species == 'Perch',]
breamroach <- fish[Species == 'Bream' | Species == 'Roach',]
perch <- fish[Species == 'Perch',]
fish4 <- filter(fish, Species == "Perch" | Species == "Roach" |Species == "Pike" | Species == "Bream")

result <- t.test(Height ~ Species, data = pikeperch, alternative = "two.sided")
result
result <- t.test(Height ~ Species, data = breamroach, alternative = "two.sided")
result

##5
lm_model <- lm(Weight ~ Width + Length3 + Species + Height, data = fish)
summary(lm_model)
bream_smelt_parkki <- filter(fish, Species == 'Bream' | Species == 'Smelt' | Species == 'Parkki')
View(bream_smelt_parkki)
lm_weight <- lm(Weight ~ Width + Length3 + Species + Height, data = bream_smelt_parkki)
summary(lm_weight)
# weight = -795.7530 + Height*82.2951 + Parkki*115 + Smelt * 581
coefs <- coef(lm_weight)
coefs


##5.5
residuals <- residuals(lm_weight)
shapiro.test(residuals)

plot(fitted(lm_weight), residuals, pch = 20)  # График на остатъците срещу предсказаните стойности
abline(h = 0, col = "red")  # Хоризонтална линия на 0


##5.6
cor(bream_smelt_parkki$Weight, bream_smelt_parkki$Height, method = "pearson")
cor(bream_smelt_parkki$Weight, bream_smelt_parkki$Width, method = "pearson")
cor(bream_smelt_parkki$Weight, bream_smelt_parkki$Length3, method = "pearson")
cor(bream_smelt_parkki$Weight, bream_smelt_parkki$Species, method = "pearson")
cor(bream_smelt_parkki$Weight, bream_smelt_parkki$Height)
cor(bream_smelt_parkki$Weight, bream_smelt_parkki$Width)


##6.1
p_insurance <- 0.27
n <- 10
probability <- sum(dbinom(0:4, size = n, prob = p_insurance))
probability

##6.2

probability <- pbinom(8, size = n, prob = p_insurance) - pbinom(2, size = n, prob = p_insurance)
probability
