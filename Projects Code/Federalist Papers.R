library(tm)
library(stringr)
library(wordcloud)
library(stringi)
library(Matrix)
install.packages("tidytext")
library(tidytext) 
library(dplyr)
library(ggplot2)
library(factoextra)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(cluster)
install.packages("NbClust")
library(NbClust)
install.packages("wordspace")
library(wordspace)
install.packages("dendextend")
library(dendextend)
install.packages("randomcoloR")
library(randomcoloR)
library(tidyverse)

file <- file.choose()
papers <- read.csv(file)
table(papers[,1])
authorFrame <- data.frame(
  group=levels(papers[,1]),
  value=table(papers[,1])
)
ggplot(authorFrame, aes(x="", y=value.Freq, fill = group)) + 
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  ggtitle("Authorship breakdown") +
  theme_void()

papersHam <- papers[papers$author == "Hamilton",]
papersH_and_M <- papers[papers$author == "HM",]
papersJay <- papers[papers$author == "Jay",]
papersMadison <- papers[papers$author == "Madison",]
createWordMean <- function(x) {
  y <- ncol(x)
  x <- colMeans(x[,3:y])
  newVec_1 <- c()
  for (i in 1:length(x)){
    newVec_1[i] <- x[[i]]
  }
  print(newVec_1)
}
hamiltonVector <- createWordMean(papersHam)
ham_n_mad_Vector <- createWordMean(papersH_and_M)
jayVector <- createWordMean(papersJay)
madVector <- createWordMean(papersMadison)
columns <- colnames(papersHam)
newFrame <- papersHam
newFrame <- data.frame(rbind(hamiltonVector, ham_n_mad_Vector, jayVector, madVector))
colnames(newFrame2) <- columns[3:length(columns)]
newFrame[,1:3]
wordVariance<- sapply(newFrame, var)
ordered <- sort(wordVariance, decreasing = TRUE)
plot(ordered)
goodWords<- data.frame(ordered[1:(length(ordered)*2/5)])
length(rownames(goodWords))
rownames(goodWords)
smallFrame<- papers[,c("author", "filename", rownames(goodWords))]
rownames(smallFrame) <- smallFrame$filename
rownames(papers) <- papers$filename
write.csv(smallFrame,"/Users/johnjefferson/downloads/fedPapers_varianceframe.csv")
write.csv(papers,"/Users/johnjefferson/downloads/fedPapers_FullPapersframe.csv")
set.seed(74)
treeData_small <- read.csv("/Users/johnjefferson/downloads/fedPapers_varianceframe.csv")
treeData_small <- treeData_small[,-1]
treeData_small <- treeData_small[-c(which(treeData_small$author=='Jay'), which(treeData_small$author=="HM"))]
treeData_small <- droplevels(treeData_small)
treeData_small <- treeData_small[,-2]
treeData_small <- treeData_small[,c(2:ncol(treeData_small),1)]
treeData_small_disputed <- treeData_small[1:11,]
treeData_small_full_noD <- treeData_small[-c(1:11),]
indexes <- sample(1:55, .65*length(1:55))
indexes <- c(indexes, sample(56:nrow(treeData_small_full_noD), .65*length(56:nrow(treeData_small_full_noD))))  
tree_model1 <- rpart(author ~ ., data = treeData_small_full_noD[indexes,]
                    , method = "class"
                    , control = rpart.control(minbucket = 1, minsplit = 1, cp = -1)
                    , model = T
)
rsq.rpart(tree_model1)
rpart.plot(tree_model1)
preds1 <- predict(tree_model1, treeData_small_full_noD[-indexes,], type = "class")
table(treeData_small_full_noD$author[-indexes],preds1)
View(treeData_small)
tree_model3 <- rpart(author ~ . , data = treeData_small_full_noD[indexes,], method = 'class', model = TRUE)
rsq.rpart(tree_model3)
