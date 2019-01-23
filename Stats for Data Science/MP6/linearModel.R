
# Reading Data from the input file
inputData = read.csv(file="MP6/prostate_cancer(1).csv", header=TRUE, sep = ",")

inputData$vesinv = as.factor(inputData$vesinv)
#attaching the data so that we are able to access columsn via direct names
attach(inputData)

head(inputData)

#boxplot(inputData$psa)
#qqnorm(inputData$psa)

par(mfrow=c(1,1))
boxplot(psa)
qqnorm(psa)
qqline(psa)

# as we see there are a some outliers, so we wil transform, as its asked in the
# question to use log transformation if need be, we will use log transform only,
# whereas a sqrt transformer could have also done the job

logPSA = log(psa)
qqnorm(logPSA)
qqline(logPSA)

# So now we see that the number of outliers are reduced

#we will check response wrt cancervol
plot(cancervol, logPSA)
#Regress PSA on cancervol
fit1=lm(logPSA~cancervol)
abline(fit1)
#summary(fit1)

#we will check response wrt weight
plot(weight, logPSA)
#Regress PSA on weight
fit2=lm(logPSA~weight)
abline(fit2)
#summary(fit2)



#we will check response wrt age
plot(age, logPSA)
#Regress PSA on age
fit3=lm(logPSA~age)
abline(fit3)





#checking response wrt benpros
plot(benpros, logPSA)
#Regress PSA on benpros
fit4=lm(logPSA~benpros)
abline(fit4)


#checking response wrt vesinv
plot(vesinv, logPSA)
#Regress PSA on vesinv
fit5=lm(logPSA~vesinv)
abline(fit5)

#checking response wrt capspen
plot(capspen, logPSA)
#Regress PSA on capspen
fit6=lm(logPSA~capspen)
abline(fit6)
#from the summary of fit6 model we can see that capspen is significant



#checking response wrt gleason
plot(gleason, logPSA)
#Reress PSA on gleason
fit7=lm(logPSA~gleason)
abline(fit7)

# We observe a positive trend in each case
# from the predictors given it looks like cancercol, benpros, gleason might be important predictors to predict psa level
# so we will build a model with these first
fit8=lm(logPSA~cancervol+benpros+gleason)
summary(fit8)

# adding age and weight to the model as a predictor
fit9=lm(logPSA~cancervol+benpros+gleason+age+weight)
summary(fit9)

#we can see that age and weight does not seem important
#so we can drop those from the model

fit10=lm(logPSA~cancervol+benpros+gleason+capspen+vesinv)
summary(fit10)

#we can see that capspen is insignificant, so we remove it
fit11=lm(logPSA~cancervol+benpros+gleason+vesinv)
summary(fit11)

fit12=lm(logPSA~cancervol+benpros+gleason+vesinv+age+weight+capspen)
summary(fit12)

# Perform a partial F-test to check significance of age weight capspen
anova(fit11, fit12)

# We don't need any of the three variables, age, weight, capspen. Also, the model in fit11 does not
# have any non-significant predictors. Therefore, we take this as our
# preliminary model for the data. However, we need to perform the diagnostics
# before accepting this model.

# Let us compare fit11 with automatic stepwise model selection procedures based on AIC
# In the output below '+' means 'add variable' and '-' means 'drop variable.'

fit13.forward <- step(lm(logPSA ~ 1), scope = list(upper = ~cancervol+benpros+gleason+vesinv+age+weight+capspen), direction = "forward")
fit14.backward <- step(lm(logPSA ~ cancervol+benpros+gleason+vesinv+age+weight+capspen), scope = list(lower = ~1), direction = "backward")
fit15.both <- step(lm(logPSA ~ 1), scope = list(lower = ~1, upper = ~cancervol+benpros+gleason+vesinv+age+weight+capspen), direction = "both")

# Based on AIC we can easily see that the forward backward and both directions give the exact same model as we predicted earlier(fit11)

# Lets make some Residual Plots and QQPlots
plot(fitted(fit11), resid(fit11),main="Residual plot of the model")
abline(h = 0)
# No Trend
# No Change in vertical spread ! Good news !

qqnorm(resid(fit11), main="QQPlot for the best model")
qqline(resid(fit11))

# This preliminary model(fit11) passes the diagnostics. So we can take this as our final model.
# Therefore fit 11 is our best model
summary(vesinv)

# we see that 0s are 76 and 1s are 21

# calculate all means
(mean.cancervol = mean(cancervol))
(mean.benpros = mean(benpros))
(mean.gleason = mean(gleason))

# here 0 beign most frequentin vesinv
(psa = - 0.65 + (0.06 * mean.cancervol)+(0.09*mean.benpros)+(0.33*mean.gleason)+(0.68*0))
