#set main str connections
setwd("C:/Users/Marino Del Carpio/Desktop/R Datasets")

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

View(test)
View(train)

#create a df with a column named survived and set values as none
test.survived <- data.frame(survived = rep("None", nrow(test)),test[,])
View(test.survived)

# change columns order
test.survived <- test.survived[,c(2,1,3,4,5,6,7,8,9,10,11,12)]

#Change name of a Column
names(test.survived)[2] <- "Survived"

#Combine train with survived column of test
data.combined <- rbind(train,test.survived)

#get data.combined info 
str(data.combined)

#change value type of columns
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

table(data.combined$Pclass)
library(ggplot2)
install.packages("ggplot2")
install.packages("stringr")

#plot a hystogram
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + geom_bar(width = 0.5) + xlab("Pclass") + ylab("Total Counts") + labs(fill = "Survived")
length(unique(as.character(data.combined$Name)))
head(as.character(train$Name))

#get duplicated names
duponames <-as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
data.combined[which(data.combined$Name %in% duponames),]

#get misses
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

#get number of misses, masters, mrs and mss
extractTitle <- function(name) {
  name <- as.character(name) 
  if(length(grep("Miss.", name)) > 0){
    return("Miss") } 
  else if(length(grep("Master.", name)) > 0){ 
  return("Master") } 
  else if(length(grep("Mr.", name)) > 0){
  return("Mr.") } 
  else if(length(grep("Ms.", name)) > 0){
  return("Ms.")} 
  else{return("Other")}
}

titles <- NULL

#fill titles with the titles of data.combined

for (t in 1:nrow(data.combined)) {
  
  titles <- c(titles,extractTitle(data.combined[t,"Name"]))
  
}

data.combined$title <- as.factor(titles)

plot.new()

#Get the amount that did and did not survived acording to their title and Passenger Class  
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) + 
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") + 
  xlab("Titles") +
  ylab("Total Count") +
  title("Survived")


  