dataFile <- file.choose()
smSales<-read.csv(file = dataFile
                , header = TRUE
                , stringsAsFactors = FALSE)
summary(smSales)
barplot(smSales$Unit.price)
View(smSales)
library(tidyverse)
library(RColorBrewer)
install.packages("ggpubr")
library(ggpubr)
sumagg <- aggregate(smSales$Quantity, by = list(smSales$Product.line, smSales$Branch,smSales$City), sum)
cols<- c("#147D42", "#62A22C", "#B1C816", "#FFED00")
cols(4)
ggplot(sumagg, aes(Group.3, x, fill = Group.1)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Number of Units Sold per Product Line for each City ") +xlab("Cities")+ 
  ylab("Number of Units Sold")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Product Lines"))+
  scale_fill_manual(values=c("#147D42", "#3B9037", "#62A22C", "#8AB521", "#B1C816", "#D8DA0B", "#FFED00"))

library(alluvial)
alluv.df<-aggregate(smSales$Quantity, list(smSales$Gender, smSales$Product.line), sum)
colnames(alluv.df)[1] <- "Gender"
colnames(alluv.df)[2] <- "Product Lines"
colnames(alluv.df)[3] <- "Number of Goods Sold"
alluv.df
alluvialplot<- alluvial(alluv.df, freq = alluv.df$`Number of Goods Sold`, alpha = .5, cex = .9, col = cols, blocks = FALSE, border = "gray")

is.numeric(smSales$Quantity)
ggplot(smSales) + geom_boxplot(aes(x=Gender, y=Quantity, fill = Gender))+xlab("") +ylab("")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values=c( "#3B9037", "#D8DA0B"))

mean(smSales$Rating)
meanagg <- aggregate(smSales$Rating, by = list(smSales$Gender, smSales$City), mean)
colnames(meanagg)[2] <- "Cities"
meanagg
smSales

ggplot(meanagg, aes(Group.1, x, fill = Cities)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Average Rating of each Gender per City ")+xlab("Gender") +ylab("Average Rating")+
  scale_fill_manual(values=c("#147D42",  "#8AB521",  "#D8DA0B"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
