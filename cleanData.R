# cleanData.R
# for cleaning and arranging data set
# kurtis bertauche
# 19 septmber 2022

data <- read.csv("data/raw/ccRCC_DIA_protein.tsv", sep = "\t")

# remove peptide sequences and num peptides (not necessary for this task)
data$NumPeptides <- NULL
data$PeptideSequences <- NULL

# replace with easier to read labels
colnames(data)[endsWith(colnames(data), "_NAT.mzML")]<-"Natural"
colnames(data)[endsWith(colnames(data), "_T.mzML")]<-"Tumor"

# need to add these to top so they are not lost in transposition
data <- rbind(colnames(data), data)

# flip rows and columns
data <- t(data)

# turn into data frame
data <- as.data.frame(data, row.names = 1:nrow(data))

# correct column names
colnames(data) <- data[1,]
colnames(data)[1] <- "Outcome"

# remove extra names that were saved during transposition process
data <- data[-1,]

# 1 = tumor, 0 = not tumor
data$Outcome[data$Outcome == "Tumor"] <- 1   
data$Outcome[data$Outcome == "Natural"] <- 0

# write to file
write.csv(data, file = "data/clean/processed.csv", quote = FALSE, row.names = FALSE)

