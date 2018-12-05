
# Reading Data from the input file
inputData = read.csv(file="MP6/prostate_cancer(1).csv", header=TRUE, sep = ",")


#attaching the data so that we are able to access columsn via direct names
attach(inputData)

head(inputData)

#boxplot(inputData$psa)
#qqnorm(inputData$psa)

par(mfrow=c(2,2))
boxplot(psa)
qqnorm(psa)

#as we see there are a some outliers, so we wil transform, as its asked in the question to use log transformation if need be, we will use log transform only
logPSA = log(psa)
qqnorm(logPSA)

#So now we see that the number of outliers are reduced
# Also in the
