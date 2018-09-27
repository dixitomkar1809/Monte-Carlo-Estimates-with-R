# Reading roadrace.csv
roadrace = read.csv("Documents/Fall18/Stats/R_Mini_Projects/MP2/roadrace.csv", na.strings = "*")
summary(roadrace)

#Storing the Maine column of the data
a = table(roadrace[["Maine"]])

# Question 1(a)
# Creating bar graph for variable Maine
barplot(a, main="Question 1(a)", xlab="Maine or Away", ylim = c(0,5000))
maine = subset(roadrace, select = "Maine")
(maine.summary = summary(maine))

# Question 1(b)
# Creating a subset of the values of Column "Maine" to create two separate histograms
b.maine = subset(roadrace, roadrace$Maine=="Maine")
b.away = subset(roadrace, roadrace$Maine=="Away")

# Storing just the running times (in minutes)
b.maine.time = b.maine$Time..minutes.
b.away.time = b.away$Time..minutes.

# Plotting Histograms

#overlapping Histograms
par(mfrow=c(1,1))
hist(b.maine.time, main = "Question 1(b)", xlab = "Running Times Dark Gray: Maine, Light Gray: Away  ", ylim = c(0,2000), col=rgb(0.1,0.1,0.1,0.5))
hist(b.away.time, main = "Question 1(b) ",xlab = "Running Times", ylim=c(0,2000), add=T,  col=rgb(0.8,0.8,0.8,0.5))
box()

#side by side histograms
par(mfrow=c(1,2))
hist(b.away.time, main = "Runners from Other Places",xlim=c(20,120),ylim=c(0,2000), xlab = "Running Time(in mins)")
box()
hist(b.maine.time, main = "Runners from Maine" ,xlim=c(20,120),ylim=c(0,2000), xlab = "Running Time(in mins)")
box()


# Five number summary
(b.maine.time.summary = summary(b.maine.time))
b.maine.time.q1 = b.maine.time.summary[2]
b.maine.time.q3 = b.maine.time.summary[5]
(b.away.time.summary = summary(b.away.time))
b.away.time.q1 = b.away.time.summary[2]
b.away.time.q3 = b.away.time.summary[5]

# mean
(b.maine.time.mean = mean(b.maine.time))
(b.away.time.mean = mean(b.away.time))

# standard deviation
(b.maine.time.sd = sd(b.maine.time))
(b.away.time.sd = sd(b.away.time))

# range
(b.maine.time.range = range(b.maine.time))
(b.away.time.range = range(b.away.time))

# median
(b.maine.time.median = median(b.maine.time))
(b.away.time.median = median(b.away.time))

# inter-quartile range
(b.maine.time.iqr = IQR(b.maine.time))
(b.away.time.iqr = IQR(b.away.time))

# Question 1(c)
par(mfrow=c(1,1))
boxplot(b.maine.time, b.away.time,  main="Running Time (Maine and Away)")
#boxplot(b.maine.time,  main="Running Time (Maine)")
#boxplot(b.away.time, main="Running Time (Away)")
(b.maine.time.outliers = subset(b.maine, ((b.maine$Time..minutes. < (b.maine.time.q1 - 1.5 * b.maine.time.iqr)  ) | (b.maine$Time..minutes. > (b.maine.time.q3 + 1.5 * b.maine.time.iqr)))))
(b.away.time.outliers = subset(b.away, ((b.away$Time..minutes. < (b.away.time.q1 - 1.5 * b.away.time.iqr)  ) | (b.away$Time..minutes. > (b.away.time.q3 + 1.5 * b.away.time.iqr)))))

#Question 1(d)
# Creating subset of values on Column Sex
b.male = subset(roadrace, roadrace$Sex=="M")
b.female = subset(roadrace, roadrace$Sex=="F")
boxplot(b.male$Age, b.female$Age, main="Age of Male and Female")

# Five Number summary
(b.male.age.summary = summary(b.male$Age))
(b.male.age.q1 = b.male.age.summary[2])
(b.male.age.q3 = b.male.age.summary[5])

(b.female.age.summary = summary(b.female$Age))
(b.female.age.q1 = b.female.age.summary[2])
(b.female.age.q3 = b.female.age.summary[5])

# Mean
(b.male.age.mean = mean(b.male$Age))
(b.female.age.mean = mean(b.female$Age))

# Standard Deviation
(b.male.age.sd = sd(b.male$Age))
(b.female.age.sd = sd(b.female$Age))

# Range
(b.male.age.range = range(b.male$Age))
(b.female.age.range = range(b.female$Age))

# Median
(b.male.age.median = median(b.male$Age))
(b.female.age.median = median(b.female$Age))

# Interquartile range
(b.male.age.iqr = IQR(b.male$Age))
(b.female.age.iqr = IQR(b.female$Age))

(b.male.age.outliers = subset(b.male, (b.male$Age > (b.male.age.q3 + 1.5*b.male.age.iqr)) | (b.male$Age < (b.male.age.q1 - 1.5*b.male.age.iqr))))
(b.female.age.outliers = subset(b.female, (b.female$Age > (b.female.age.q3 + 1.5*b.female.age.iqr)) | (b.female$Age < (b.female.age.q1 - 1.5*b.female.age.iqr))))

# Question 2
# Reading the csv into c
c = read.csv("Documents/Fall18/Stats/R_Mini_Projects/MP2/motorcycle.csv")
summary(c)

# Storing just the number of accidents
c.noOfAcc = c$Fatal.Motorcycle.Accidents

# Boxplot for the number of accidents in various counties
boxplot(c.noOfAcc, main="Number of Accidents")

# Summary of the distribution
(c.noOfAcc.summary =summary(c.noOfAcc))

# Storing q1, q3 and iqr for finding out the outliers
(c.noOfAcc.q1 = c.noOfAcc.summary[2])
(c.noOfAcc.q3 = c.noOfAcc.summary[5])
(c.noOfAcc.iqr = IQR(c.noOfAcc))

# Finding the outliers
(c.noOfAcc.county.outliers = subset(c, ((c.noOfAcc < (c.noOfAcc.q1 - 1.5 * c.noOfAcc.iqr))|(c.noOfAcc > (c.noOfAcc.q3 + 1.5 * c.noOfAcc.iqr)))))

