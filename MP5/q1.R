# setwd("C:/Project5")
# read the data
data = read.csv(file = "MP5/bodytemp-heartrate.csv", header = TRUE, sep = ',')

data.male=subset(data, gender=="1")
data.female=subset(data, gender=="2")

# Q1.a) analyzing the body temperature data

par(mfrow = c(2,2))

#boxplots of body temparature vs gender
boxplot(data.male$body_temperature, data.female$body_temperature, names=c("male","female"))

#qq plots for male and female body temperature
qqnorm(data.male$body_temperature, main="Normal Q-Q Plot for males")
qqnorm(data.female$body_temperature, main="Normal Q-Q Plot for females")

# t-test for body temperature
t.test(data.male$body_temperature, data.female$body_temperature, alternative = "two.sided", var.equal = F)


#Q1.b)analyzing the heart rate data

par(mfrow = c(2,2))

#boxplots ofheart rate vs gender
boxplot(data.male$heart_rate, data.female$heart_rate, names=c("male","female"))

#qq plots for male and female heart rates
qqnorm(data.male$heart_rate, main="Normal Q-Q Plot for males")
qqnorm(data.female$heart_rate, main="Normal Q-Q Plot for females")

# t-test for heart rate
t.test(data.male$heart_rate, data.female$heart_rate, alternative = "two.sided", var.equal = F)

#Q1.c) finding relationship between body temperature and heart rate

par(mfrow = c(1, 1))

#scatterplots between heart rate and body temperature for males and females
plot(body_temperature ~ heart_rate, data = data.male,ylim = range(data$body_temperature), xlim = range(data$heart_rate))
points(body_temperature ~ heart_rate, data = data.female, pch =19)

#correlation between body temperature and heart rate for males
cor(data.male$body_temperature, data.male$heart_rate)

#getting the fitted regression line
male.reg=lm(data.male$body_temperature~data.male$heart_rate)
male.reg

#correlation between body temperature and heart rate for females
cor(data.female$body_temperature, data.female$heart_rate)

#getting the fitted regression line
female.reg=lm(data.female$body_temperature~data.female$heart_rate)
female.reg

#Adding the 2 regression lines to the scatter plot
abline(male.reg, lty="dotted")
abline(female.reg)

