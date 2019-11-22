# Refresh the environment
rm(list = ls(all = TRUE)) 


directory = 'C:/Users/maret/OneDrive/Documents/Formation Science des Données/Données manquantes/projet'
# Set the folder path to download datasets
setwd(directory)

columns = cbind("age", "workClass", "fnlwgt", "education", "education-num","marital-status", "occupation", "relationship","race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "income")

Train_data = read.csv("adult.data", header=F,col.names = columns,na.strings = cbind(""," "," ?","NA",NA))
View(Train_data)
summary(Train_data)
str(Train_data)
dim(Train_data)

typeof(Train_data$workClass)
typeof(as.character(Train_data$workClass))

library(FactoMineR)
install.packages("VIM")
library(VIM)

# Creation of a categorical data set with "o" when observed and "m" when missing
pattern = matrix("o",nrow=nrow(Train_data),ncol=ncol(Train_data))
pattern[is.na(Train_data)] = "m"

View(as.character(Train_data$workClass)==" ?")

pattern = as.data.frame(pattern)

dimnames(pattern) = dimnames(Train_data)

print(pattern)
View(pattern)
dimnames(pattern)
summary(pattern)
# MCA
res.mca = MCA(pattern,graph=F)
plot(res.mca,selectMod=grep("_m",rownames(res.mca$var$coord)),invisible="ind")

#Visualisation données manquantes
aggr(Train_data)
matrixplot(Train_data)

# Deuxième MCA

str(Train_data)

Train_data.cat<-as.data.frame(Train_data)

#set column classes
factcols <- c(2,4,6:10,14:15)
numcols <- setdiff(1:15,factcols)

Train_data[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
Train_data[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

#subset categorical variables
cat_train <- train[,factcols, with=FALSE]

#subset numerical variables
num_train <- train[,numcols,with=FALSE]

for(i in 1:7){
  breaks = cbind(-Inf,quantile(Train_data.cat[[i]],na.rm=T)[-1])
  Train_data.cat[[i]]<-cut(Train_data.cat[[i]],breaks=breaks,labels=F)
  Train_data.cat[[i]]<-addNA(Train_data.cat[[i]],ifany=T)
  }

for(i in 8:10){
  Train_data.cat[[i]]<-as.factor(Train_data.cat[[i]])
  }

summary(Train_data.cat)

