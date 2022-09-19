# splitData.r
# split data set into training and testing set
# kurtis bertauche
# 19 september 2022

cleanData <- read.csv(file = "data/clean/processed.csv")

set.seed(37)

setAssignments <- sample(1:2, size = nrow(cleanData), prob = c(0.8, 0.2), replace = TRUE)
trainingData <- data[setAssignments == 1,]
testingData <- data[setAssignments == 2,]

write.csv(trainingData, file = "data/clean/training.csv", quote = FALSE, row.names = FALSE)
write.csv(testingData, file = "data/clean/testing.csv", quote = FALSE, row.names = FALSE)
