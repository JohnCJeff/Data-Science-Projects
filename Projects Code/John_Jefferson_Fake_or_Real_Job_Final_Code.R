#John Jefferson
#IST 707 final project

library(ggplot2)
library(cluster)
library(factoextra)
library(dendextend)
library(dplyr)
library(tidyr)
library(arules)
library(e1071)
library(naivebayes)
library(caret)
library(rpart)
library(rpart.plot)
library(FactoMineR)
library(sqldf)
library(class)
library(randomForest)
library(RColorBrewer)
library(wordcloud)
library(stringr)
library(stopwords)
library(wordcloud2)
library(corpus)

## EDA
# load data
file<- "C:\\Users\\John\\Desktop\\School\\IST 707 Data Analytics ML\\Project\\fake_job_postings.csv"
Jobs <- read.csv(file, na.strings = c(""))
str(Jobs)
sum(is.na(Jobs))
colSums(is.na(Jobs))

head(Jobs)

# count real vs fake jobs
length(which(Jobs$fraudulent==0))
length(which(Jobs$fraudulent==1))
barplot(table(Jobs$fraudulent), main= "Number Of Job Postings", names.arg = c("Real", "Fake"))

#Create Fake Jobs DF, analyze rate of NAs
fake_jobs <- Jobs[which(Jobs$fraudulent==1),]
colSums(is.na(fake_jobs))
round((colMeans(is.na(fake_jobs))*100), digits=2)

#Create Real jobs DF, analyze rate of NAs
real_jobs <- Jobs[which(Jobs$fraudulent==0),]
colSums(is.na(real_jobs))
round((colMeans(is.na(real_jobs))*100), digits=2)

#Analysis of binary variables, real vs fake postings
barplot(table(fake_jobs$telecommuting), names.arg = c("No", "Yes"))
barplot(table(real_jobs$telecommuting), names.arg = c("No", "Yes"))

barplot(table(fake_jobs$has_company_logo), names.arg = c("No", "Yes"))
barplot(table(real_jobs$has_company_logo), names.arg = c("No", "Yes"))

barplot(table(fake_jobs$has_questions), names.arg = c("No", "Yes"))
barplot(table(real_jobs$has_questions), names.arg = c("No", "Yes"))


# calculate length of job description

split <- function(x) {
  y <- strsplit(x, " ")
  z <- sapply(y, length)
  return(z)
}

#create column for description length
fake_jobs$desc_length <- split(fake_jobs$description)
real_jobs$desc_length <- split(real_jobs$description)

#average description length real vs fake
avg_fake_len <- mean(fake_jobs$desc_length)
avg_real_len <- mean(real_jobs$desc_length)

#Distribution of description lengths
hist(fake_jobs$desc_length, main= "Fake Job Description Lengths", xlim = c(0,2000))
hist(real_jobs$desc_length, main= "Real Job Description Lengths", xlim = c(0,2000))

boxplot(fake_jobs$desc_length, ylim = c(0,2100))
boxplot(real_jobs$desc_length, ylim = c(0,2100))


# subset for desc_length less than 600
fake2 <- fake_jobs[fake_jobs$desc_length < 500, ]
real2 <- real_jobs[real_jobs$desc_length < 500, ]

boxplot(fake2$desc_length, ylim = c(0,500))
boxplot(real2$desc_length, ylim = c(0,500))

########
########
#####     DATA PREP
########

#this script creates a refined data set for the IST707 final project and feeds it to a new csv

jobs <- Jobs
split <- function(x) {
  y <- strsplit(x, " ")
  z <- sapply(y, length)
  return(z)
}
jobs$desc_length <- split(jobs$description)
cols<- c('fraudulent','has_questions','has_company_logo','company_profile','desc_length')
jobs<- jobs[,cols]
jobs[,"company_profile"][!is.na(jobs[,"company_profile"])]<- "1"
jobs[,"company_profile"][is.na(jobs[,"company_profile"])]<- "0"
jobs$company_profile<- as.integer(jobs$company_profile)
jobs$fraudulent <- as.factor(jobs$fraudulent)

Jobs<- jobs
print("Data Prep Complete")
#write.csv(Jobs, 'C:\\Users\\John\\Desktop\\School\\IST 707 Data Analytics ML\\Project\\JobsBinary.csv', row.names=FALSE)


########
########
#####     DATA Analysis
########

#K Means analysis

jobs_k<- jobs
jobs_k[,1]<-NULL

# Set seed for fixed random seed
set.seed(10)

# run k-means
Clusters <- kmeans(jobs_k, 3) #set number of clusters
jobs_k$Clusters <- as.factor(Clusters$cluster)


# Add clusters to dataframe original dataframe with author name
jobs_k2 <- jobs
jobs_k2$Clusters <- as.factor(Clusters$cluster)

# Plot results
#clusplot(jobs_k, jobs_k$Clusters, color=TRUE, shade=TRUE, labels=0, lines=0)

ggplot(data=jobs_k2, aes(x=fraudulent, fill=Clusters))+
  geom_bar(stat="count") +
  labs(title = "12 K-Means Clusters") +
  theme(plot.title = element_text(hjust=0.5), text=element_text(size=15))

#######
#Take sample of real jobs 3x number of fake jobs, see if clustering works better

#table(jobs$fraudulent)
fake_jobs <- jobs[which(jobs$fraudulent==1),]
n=nrow(fake_jobs)*3 #set size for sample
real_jobs <- jobs[which(jobs$fraudulent==0),]
sampleReal <- real_jobs[sample(nrow(real_jobs), n, replace = F),]
smallJobs <- rbind(sampleReal, fake_jobs)
smallJobs <- smallJobs[sample(1:nrow(smallJobs), replace = F),] #shuffle the order of the data frame
table(smallJobs$fraudulent)
write.csv(smallJobs, 'C:\\Users\\John\\Desktop\\School\\IST 707 Data Analytics ML\\Project\\smallJobs.csv', row.names=FALSE)


#Clustering with sample data
jobs_k<- smallJobs
jobs_k[,1]<-NULL

#Start clustering here
set.seed(1)  # Set seed for fixed random seed
Clusters <- kmeans(jobs_k, 6) # set number of clusters
jobs_k$Clusters <- as.factor(Clusters$cluster)
jobs_k2 <- smallJobs
jobs_k2$Clusters <- as.factor(Clusters$cluster)
ggplot(data=jobs_k2, aes(x=fraudulent, fill=Clusters))+
  geom_bar(stat="count") +
  labs(title = "12 K-Means Clusters, Reduced Sample of Real Jobs") +
  theme(plot.title = element_text(hjust=0.5), text=element_text(size=15))


###

##HAC analysis
jobs_HAC <- smallJobs

# Calculate distance in a variety of ways
distance  <- dist(jobs_HAC, method = "euclidean")
distance2 <- dist(jobs_HAC, method = "maximum")
distance3 <- dist(jobs_HAC, method = "manhattan")
distance4 <- dist(jobs_HAC, method = "canberra")
distance5 <- dist(jobs_HAC, method = "binary")
distance6 <- dist(jobs_HAC, method = "minkowski")

HAC <- hclust(distance, method="complete")
plot(HAC, cex=0.6, hang=-1)
rect.hclust(HAC, k =9, border=2:5)

HAC2 <- hclust(distance2, method="complete")
plot(HAC2, cex=0.6, hang=-1)
rect.hclust(HAC2, k =9, border=2:5)

HAC3 <- hclust(distance3, method="complete")
plot(HAC3, cex=0.6, hang=-1)
rect.hclust(HAC3, k =9, border=2:5)

HAC4 <- hclust(distance4, method="complete")
plot(HAC4, cex=0.6, hang=-1) 
rect.hclust(HAC4, k =9, border=2:5)

HAC5 <- hclust(distance5, method="complete")
plot(HAC5, cex=0.6, hang=-1)
rect.hclust(HAC5, k =9, border=2:5)

HAC6 <- hclust(distance6, method="complete")
plot(HAC6, cex=0.6, hang=-1)
rect.hclust(HAC6, k =9, border=2:5)




#Rule Association Mining

smallJobs_D <-smallJobs
smallJobs_D$desc_length <- cut(smallJobs_D$desc_length, breaks = c(0,25,50,75,100,150,200,250,300,400,500),
                labels= c("0-25", "26-50", "51-75", "76-100","101-150","151-200","201-250","251-300","301-400","lots!"),
                right = FALSE)
for (col in colnames(smallJobs_D)) {
  if (col != "fraudulent") {
    smallJobs_D[, c(col)] <- as.factor(smallJobs_D[, c(col)])
  }
}
#convert to transactions
transactions <- as(smallJobs_D, "transactions")
#transactionInfo(transactions)[["transactionID"]]<- tid

itemFrequencyPlot(transactions, topN=20, type="absolute")

#Right hand side = real posting

rules <- apriori(data=transactions, parameter = list(supp=0.01, conf=0.3, minlen=2),
                 appearance = list(default="lhs", rhs="fraudulent=0"),
                 control = list(verbose=F))


rules<- sort(rules, decreasing=TRUE, by='lift') 
inspect(rules[1:9])

summary(rules)

# Right hand side = fake posting 

rules <- apriori(data=transactions, parameter = list(supp=0.01, conf=0.3, minlen=2),
                 appearance = list(default="lhs", rhs="fraudulent=1"),
                 control = list(verbose=F))


rules<- sort(rules, decreasing=TRUE, by='lift') 
inspect(rules[1:9])

summary(rules)



##########
##########

#   Naive Bayes

rm(list=ls())  #I found that I need to clear the environment at this stage
#If I don't clear the environment, I get an error on building the holdout below. 
# The error is that 1:kfolds is an unused argument

file<- "C:\\Users\\John\\Desktop\\School\\IST 707 Data Analytics ML\\Project\\smallJobs.csv"
Jobs <- read.csv(file, na.strings = c(""))
Jobs$fraudulent <- factor(Jobs$fraudulent)
jobs<- Jobs
for (col in colnames(jobs)) {
  if (col != "fraudulent") {
    jobs[, c(col)] <- as.factor(jobs[, c(col)])
  }
}
# Create k-folds for k-fold cross validation 
## Number of observations
N <- nrow(jobs)
## Number of desired splits
kfolds <- 8
## Generate indices of holdout observations
## Note if N is not a multiple of folds you will get a warning, but is OK.
holdout <- split(sample(1:N), 1:kfolds)


percent <- 1 #I set percent to 1 to use the full data set

set.seed(275)
jobsSplit <- sample(nrow(jobs),nrow(jobs)*percent)
job2 <- jobs[jobsSplit,]
dim(job2)
table(job2$fraudulent)
(nrow(job2))
## Crossvalidation results


#Run training and Testing for each of the k-folds
AllResults<-list()
AllLabels<-list()
for (k in 1:kfolds){
  job2_Test <- job2[holdout[[k]], ]
  job2_Train <- job2[-holdout[[k]], ]
  ## View the created Test and Train sets
  #(head(job2_Train))
  #(table(job2_Test$DigitTotalDF.label))
  ## Make sure you take the labels out of the testing data
  #
  job2_Test_noLabel<-job2_Test[-c(1)]
  job2_Test_justLabel<-job2_Test$fraudulent
  #(head(job2_Test_noLabel))
  
  #### Naive Bayes prediction ussing e1071 package
  #Naive Bayes Train model
  train_naibayes<-naiveBayes(fraudulent~., data=job2_Train, na.action = na.pass)
  #train_naibayes
  #summary(train_naibayes)
  #Naive Bayes model Prediction 
  nb_Pred <- predict(train_naibayes, job2_Test_noLabel)
  #nb_Pred
  #Testing accurancy of naive bayes model with Kaggle train data sub set
  (confusionMatrix(nb_Pred, job2_Test$fraudulent))
  # Accumulate results from each fold, if you like
  AllResults<- c(AllResults,nb_Pred)
  AllLabels<- c(AllLabels, job2_Test_justLabel)
  
  ##Visualize
  #plot(nb_Pred, ylab = "Density", main = "Naive Bayes Plot")
}

## Confusion Matrix (across all folds)
(table(unlist(AllResults),unlist(AllLabels)))



#############
#############
#  K Nearest Neighbor (KNN)


trainset <- Jobs
percent <- 1
dimReduce <- .10
set.seed(275)
DigitSplit <- sample(nrow(trainset),nrow(trainset)*percent)
trainset <- trainset[DigitSplit,]
dim(trainset)

table(trainset$fraudulent)


# Setting static variables used throughout the Models section
N <- nrow(trainset)
kfolds <- 2
set.seed(30)
holdout <- split(sample(1:N), 1:kfolds)


k_guess =1
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- trainset[holdout[[k]], ]
  new_train <- trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  pred <- knn(train=new_train, test=new_test, cl=new_train$fraudulent, k=k_guess, prob=FALSE)
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$fraudulent, pred=pred))
}
table(all_results$orig, all_results$pred)

# Function for model evaluation
get_accuracy_rate <- function(results_table, total_cases) {
  diagonal_sum <- sum(c(results_table[[1]], results_table[[4]]))
  (diagonal_sum / total_cases)*100
}

get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))

#######
#######
## SVM

svm_trainset <- Jobs

#The first attempt will be a straight baseline using the data preprocessed for SVM.

# Baseline SVM - no changes to data
N <- nrow(svm_trainset)
kfolds <- 8
set.seed(30)
holdout <- split(sample(1:N), 1:kfolds)

all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- svm_trainset[holdout[[k]], ]
  new_train <- svm_trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  test_model <- svm(fraudulent ~ ., new_train, na.action=na.pass)
  pred <- predict(test_model, new_test_no_label, type=c("class"))
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$fraudulent, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))




##Polynomial Kernel

all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- svm_trainset[holdout[[k]], ]
  new_train <- svm_trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  test_model <- svm(fraudulent ~ ., new_train, kernel="polynomial", na.action=na.pass)
  pred <- predict(test_model, new_test_no_label, type=c("class"))
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$fraudulent, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))


##Radial
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- svm_trainset[holdout[[k]], ]
  new_train <- svm_trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  test_model <- svm(fraudulent ~ ., new_train, kernel="radial", na.action=na.pass)
  pred <- predict(test_model, new_test_no_label, type=c("class"))
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$fraudulent, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))

##Sigmoid


all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- svm_trainset[holdout[[k]], ]
  new_train <- svm_trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  test_model <- svm(fraudulent ~ ., new_train, kernel="sigmoid", na.action=na.pass)
  pred <- predict(test_model, new_test_no_label, type=c("class"))
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$fraudulent, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))



### Random Forest
jobs<- Jobs
jobs$fraudulent <- factor(jobs$fraudulent)
jobs$desc_length <- cut(jobs$desc_length, breaks = c(0,25,50,75,100,125,150,175,200,250,300,400,5000),
                        labels= c("0-25", "26-50", "51-75", "76-100","101-125","126-150","151-175","176-200","201-250","251-300","301-400","lots!"),
                        right = FALSE)
for (col in colnames(jobs)) {
  if (col != "fraudulent") {
    jobs[, c(col)] <- as.factor(jobs[, c(col)])
  }
}

trainset <- jobs

all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- trainset[holdout[[k]], ]
  new_train <- trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  test_model <- randomForest(fraudulent ~ ., new_train, na.action=na.pass)
  pred <- predict(test_model, new_test_no_label, type=c("class"))
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$fraudulent, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))


#Lets try varying the number of trees. Tree selection will be automated moving up in multiples of 5.
prev_result <- 0
best_result <- 0
best_number_trees <-0
for (trees in 1:15) {
  if (trees %% 1 == 0) {
    all_results <- data.frame(orig=c(), pred=c())
    for (k in 1:kfolds) {
      new_test <- trainset[holdout[[k]], ]
      new_train <- trainset[-holdout[[k]], ]
      
      new_test_no_label <- new_test[-c(1)]
      new_test_just_label <- new_test[c(1)]
      
      test_model <- randomForest(fraudulent ~ ., new_train, replace=TRUE, na.action=na.pass)
      pred <- predict(test_model, new_test_no_label, type=c("class"))
      
      all_results <- rbind(all_results, data.frame(orig=new_test_just_label$fraudulent, pred=pred))
    }
    #table(all_results$orig, all_results$pred)
    new_result <- get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))
    
    if (new_result > prev_result) {
      prev_result <- new_result
    } else {
      best_number_trees <- trees
      best_result <- new_result
      break
    }
  }
}  
paste("Best Number of Trees:", best_number_trees, "- Best Result:", best_result, sep=" ")
table(all_results$orig, all_results$pred)




##############
##############

# Word Clouds
#rm(list=ls())   #clean things up

file<- "C:\\Users\\John\\Desktop\\School\\IST 707 Data Analytics ML\\Project\\fake_job_postings.csv"
jobs <- read.csv(file, na.strings = c(""))

cols<- c("fraudulent", "description")
jobs<- jobs[,cols]

minWords <- 3 #set minimum words- words less frequent than this will be removed shortly
myStopWords <- stopwords('english')
moreStops<- c('will','one','two','may','less','well','might','without',
              'small','single','several','but','very','can','must','also',
              'any','and','are','however','into','almost','can','for','add','amp')
myStopWords<- c(myStopWords, moreStops)

fake_jobs <- jobs[which(jobs$fraudulent==1),]
fake_words<- as.data.frame(table(unlist(strsplit(tolower(fake_jobs$description), " "))))
fake_words$Var1<- str_replace_all(fake_words$Var1, regex("\\W+"), "")
fake_words<- fake_words[order(-fake_words$Freq),]
fake_words <- fake_words[which(fake_words$Freq > minWords),]
fake_words <- fake_words[-which(fake_words$Var1 ==""),]
fake_words <- fake_words %>% 
  filter(!(Var1 %in% myStopWords))
fake_words$word_length<- nchar(fake_words$Var1)
fake_words <- fake_words[which(fake_words$word_length < 20),]
fake_words$word_rate<- fake_words$Freq/nrow(fake_jobs)*100
names(fake_words)[names(fake_words) == 'Var1'] <- 'word'

head(fake_words, 20)


#fake word wordcloud

as_tibble(fake_words)

par(bg = "#b4545b")
wordcloud(words = fake_words$word, freq = fake_words$Freq, min.freq = 50,
          max.words=175, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Greys"))


##########
#Real words

real_jobs <- jobs[which(jobs$fraudulent==0),]
real_words<- as.data.frame(table(unlist(strsplit(tolower(real_jobs$description), " "))))
real_words$Var1<- str_replace_all(real_words$Var1, regex("\\W+"), "")
real_words<- real_words[order(-real_words$Freq),]
real_words <- real_words[which(real_words$Freq > minWords),]
real_words <- real_words[-which(real_words$Var1 ==""),]
real_words <- real_words %>% 
  filter(!(Var1 %in% stopwords(source = "snowball")))
real_words <- real_words[-which(real_words$Var1 =="amp"),]
real_words$word_length<- nchar(real_words$Var1)
#boxplot(real_words$word_length)
real_words <- real_words[which(real_words$word_length < 20),]
real_words$word_rate<- real_words$Freq/nrow(real_jobs)*100
names(real_words)[names(real_words) == 'Var1'] <- 'word'

#View(real_words)
head(real_words, 20)
#tail(real_word, 20)

#real word wordcloud

as_tibble(real_words)

par(bg = "#539a48")
wordcloud(words = real_words$word, freq = real_words$Freq, min.freq = 50,
          max.words=175, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Greys"))


################
################
# End of project
