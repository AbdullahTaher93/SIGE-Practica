library(dplyr)
library(funModeling)
library(tidyverse)
library(mice)
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
require(caTools)


#read the datase
data <- read.csv('./train_ok.csv')

head(data)

summary(data)
#info about No.of.entries and Var's
dim(data)
#Previews of train
sapply(data[1, ], class)
#hist of 3 vars
hist(data$var_0,xlab = "Weight",col = "yellow",border = "blue")
hist(data$var_99,xlab = "Weight",col = "yellow",border = "blue")
hist(data$var_170,xlab = "Weight",col = "yellow",border = "blue")




# Check missing values and  Remove columns that have lots of missing values
column_names = names(data)
data_withreovevars<-data
for (i in 1:length(column_names)) {
  col = column_names[i]
  null_values = sum((is.na(data[col])))
  msg = sprintf("column %s has %d missing values", col, null_values)
  print(msg)
  if(null_values>=20){
    data_withreovevars<-data_withreovevars[,-i]
  }
}

#or///Check missing values 
md.pattern(data_withreovevars)


#or///Check missing values it works just with NA's values
print(sum(is.na(data_withreovevars)))

print(column_names)





#remove NA's
column_names<-names(data_withreovevars)
print(column_names)
for (i in 2:length(column_names)) {
  data_withreovevars[,i]<-ifelse(is.na(data_withreovevars[,i]),median(data_withreovevars[,i],na.rm = TRUE),data_withreovevars[,i])
}


print("The sum of NA's")
#or///Check missing values 
print(sum(is.na(data_withreovevars)))

#or///Check missing values 
md.pattern(data_withreovevars)



data_n<-data_withreovevars 

##method for normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

for (i in 2:length(column_names)) {
  col = column_names[i]
  data_n[col]<-normalize(data_withreovevars[col])
 
}
##now all variables  between 0-1.






#remove Idcode from dataset
data_n<-data_n[-1]
data_withreovevars<-data_withreovevars[-1]
summary(data_n)

column_names=names(data_n)
#remove zero Values from dataset
for (i in 2:length(column_names)) {
  col = column_names[i]
  data_n=subset(data_n,data_n[,col]>0.009)
}
hist(data_n$var_0,xlab = "Weight",col = "yellow",border = "blue")
hist(data_n$var_99,xlab = "Weight",col = "yellow",border = "blue")
hist(data_n$var_170,xlab = "Weight",col = "yellow",border = "blue")



## set the seed to make your partition reproducible
set.seed(123)

sample = sample.split(data_n$target, SplitRatio = .7)
train_s = subset(data_n, sample == TRUE)
test_s  = subset(data_n, sample == FALSE)


dim(train_s)
dim(test_s)





library(OneR)
train <- optbin(target ~ ., data = train_s)
model <- OneR(train, verbose = TRUE)
summary(model)
plot(model)
print(train[1][1])


hist(train[130],xlab = "Weight",col = "yellow",border = "blue")

pred <- predict(model, test)
eval_model(pred, test$target)
print(pred[55])           
           


data_withreovevars<-data_withreovevars[-1]

train_lables <- data_withreovevars[sample,1]
test_lables <- data_withreovevars[140001:200000,1]

length(train_s$target)
length(test_s$target)
length(train_lables)
length(test_lables)


md.pattern(train)

library(class)
library(gmodels)
#k=64
knn <- knn(train = train_s,test = test_s ,cl = train_lables,k = 64)
CrossTable(x=test_lables,y=knn,prop.chisq = FALSE )
##k=10
knn1 <- knn(train = train_s,test = test_s ,cl = train_lables,k = 10)
CrossTable(x=test_lables,y=knn,prop.chisq = FALSE )







####################################################################################


