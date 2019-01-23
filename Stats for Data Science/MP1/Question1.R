set.seed(5)
#Q1.b.i 
#first satelite
FirstSat.Xa <- rexp(1,0.1)
#second satelite
SecondSat.Xb <- rexp(1, 0.1)
#we need the one that lasts the longest so 
Xt <- max(FirstSat.Xa, SecondSat.Xb) 

#Q1.b.ii
x <- replicate(10000, max(rexp(1,0.1), rexp(1,0.1)))

#Q1.b.iii
hist(x, probability = T)
curve((0.2*exp(-0.1*x)-0.2*exp(-0.2*x)), add = T, xlab = "x", ylab = "density")

#Q1.b.iv 
#here we are calculating E(T) Expected value
mean(x)

#Q1.b.v 
#estimation of probability that the satellite lasts more than 15 years.
mean(abs(x) >15)

#Q1.b.vi
#run the program till here 4 more times as asked in the question
#this is the expected value
mean(x)
#this is the monte carlo estimate
mean(abs(x) > 15)

#Q1.c.
# here we were asked to change the monte carlo replications and five times
# so rather than running the program 5 times we have created a function to that job
calculateProbabilityExpectedValue <- function(n){
  y <- replicate(n, max(rexp(1,0.1), rexp(1,0.1)))
  tempEV<- mean(y)
  tempP <- mean(abs(y) > 15)
  return(c(tempEV, tempP))
}

#changing the monte carlo estimates to 1000 and repeating 5 times
replicate(5, calculateProbabilityExpectedValue(1000))

#changing the monte carlo estimates to 100000 and repeating 5 times
replicate(5, calculateProbabilityExpectedValue(100000))


