input = read.csv(file = "MP4/VAPOR.csv")

# Checking the summary of the data to have idea about what we are dealing with
#summary(input)
input

par(mfrow=c(1,2))
boxplot(input$theoretical, input$experimental)
# Boxplot shows that the data distribution is not very different
# The means are almost the same

t.test(input$theoretical, input$experimental, alternative = "two.sided", conf.level=0.95, var.equal = FALSE)
