# Author: Omkar Dixit
# R for Data Science
# Class Assignment 1

car_worth_train <- read.csv("http://www.utdallas.edu/~axn112530/R/datasets/CarWorth_Train.csv")

# Find out the dimensions of this dataset
(dim(car_worth_train))

# Find out the column names
(colnames(car_worth_train))

# Use the View() function to look at the dataset
(View(car_worth_train))

# Output column with the name "Model"
(car_worth_train$Model)

# Display the summary of the "Price"" column
(summary(car_worth_train$Price))

par(mfrow=c(2,1))
# Create a histogram of the "Price" column
hist(car_worth_train$Price, main = "Histogram(Price)", xlab = "Price", ylab = "Count")

# Find count of rows where the \textit{Doors} value is greater than 2
sum(car_worth_train$Doors > 2)

# Find count of rows where the \textit{Cruise} value is equal to 1
sum(car_worth_train$Cruise==1)

# Are there any rows where the \textit{Cruise} value is null. Hint: use is.na function
sum(is.na(car_worth_train$Cruise))

# New Dataset

ames_housing <- read.csv("http://www.utdallas.edu/~axn112530/R/datasets/AmesHousing.csv")

colnames(ames_housing)
# Find percent of nulls in each column
for(i in 1:ncol(ames_housing)) {
  colName <- colnames(ames_housing[i])
  pctNull <- sum(is.na(ames_housing[,i]))/length(ames_housing[,i])
  if (pctNull > 0.50) {
    print(paste("Column ", colName, " has ", round(pctNull*100, 3), "% of nulls"))
    ames_housing[, colName] <- NULL
  }
}
colnames(ames_housing)
# Drop columns as specified above
# Done in the Function itself

# Get rid of rows with null values and call the clean dataset as ames_housing_clean
ames_housing_clean <- na.exclude(ames_housing)
dim(ames_housing_clean)
dim(ames_housing)

# Create a plot as required above
plot(ames_housing_clean$Garage.Area, ames_housing_clean$SalePrice, main = "Garage Area vs Sales Price", xlab = "Garage Area", ylab = "Sales Price")


# Working with Built-in R Datasets
data()

#  Swiss Fertility and Socioeconomic Indicators (1888) Data
dim(swiss)
View(swiss)
colnames(swiss)

# Checking for any Null Values
for(i in 1:ncol(swiss)) {
  colName <- colnames(swiss[i])
  pctNull <- sum(is.na(swiss[,i]))/length(swiss[,i])
  if (pctNull > 0.50) {
    print(paste("Column ", colName, " has ", round(pctNull*100, 3), "% of nulls"))
    swiss[, colName] <- NULL
  }
}
# Doesn't Look like the dataset has any Null Values

str(swiss)
summary(swiss)

par(mfrow=c(3,2))
hist(swiss$Fertility, main = "Histogram(Fertility)", xlab = "Fertility")
hist(swiss$Agriculture, main = "Histogram(Agriculture)", xlab = "Agriculture")
hist(swiss$Examination, main = "Histogram(Examination)", xlab = "Examination")
hist(swiss$Education, main = "Histogram(Education)", xlab = "Education")
hist(swiss$Catholic, main = "Histogram(Catholic)", xlab = "Catholic")

par(mfrow=c(3,2))
plot(swiss$Infant.Mortality~swiss$Fertility, ylab = "Infant Mortality", xlab="Fertility", main="Plot(Infant Mortality vs Fertility)")
plot(swiss$Infant.Mortality~swiss$Agriculture, ylab = "Infant Mortality", xlab="Agriculture", main="Plot(Infant Mortality vs Agriculture)")
plot(swiss$Infant.Mortality~swiss$Examination, ylab = "Infant Mortality", xlab="Examination", main="Plot(Infant Mortality vs Examination)")
plot(swiss$Infant.Mortality~swiss$Education, ylab = "Infant Mortality", xlab="Education", main="Plot(Infant Mortality vs Education)")
plot(swiss$Infant.Mortality~swiss$Catholic, ylab = "Infant Mortality", xlab="Catholic", main="Plot(Infant Mortality vs Catholic)")

