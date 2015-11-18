# load all relevant packages
library(caTools)
library(ROCR)
library(mice)
library(tm)
library(SnowballC)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(randomForest)
library(caret)
library(e1071)
library(rattle)
library(RColorBrewer)
library(Amelia)

# read training and test input data 
train_input=read.csv("train.csv",stringsAsFactors=FALSE)
test_input=read.csv("test.csv",stringsAsFactors=FALSE)


train=train_input
test=test_input

str(train)
str(test)
# There are missing values in Age, Embarked,Fare variables. Let us combine train and
# test data and try to fill the missing values with appropriate values based on other variables


table(train$Pclass,train$Survived) # this shows that most passengers who died were of 3rd class
table(train$Pclass,train$Survived,train$Sex) # this shows that almost all male passengersof 3rd class died

Survived=train$Survived

train$Survived=NULL
colnames(train)

alldata=rbind(train,test)

str(alldata)
summary(alldata)

table(alldata$Embarked)
which(alldata$Embarked=="") # to find out which rows has missing Embarked values
alldata$Embarked[c(62,830)]="S"   # fill missing values with the most frequent value

which(is.na(alldata$Fare)) # to find which row has missing Fare value
alldata[1044,]  # it is found that row 1044 is a 3rd class passenger MALE of AGE 60years. so it doenot matter what fare he paid. He will probably die.
# lets fill his fare with the mean Fare of all class 3 passengers
tapply(alldata$Fare,alldata$Pclass,mean,na.rm=T) 
alldata$Fare[1044]= 13.3
summary(alldata)

# next missing vale is Age which is missing for 263 passengers
#let us explore the names of the Passengers
alldata$Name[1] #  "Braund, Mr. Owen Harris"
# We see that the passengers name is Given as his Sirname, followed by Title
# and them his First name. The title is given accordin to the rank in old England
# we will use the Title to find the missing age of the passengers
# we will fill the missing age with the mean of the Age for the same Title.
# another option is we can run a Linear Regression on the Dataset with the 
# age as the independent variable. WE WILL TRY BOTH OPTIONS.

# before that we need to segregate the names into Title, First and last names.


# To extract the Title out of the name use the following 
#title=strsplit(alldata$Name[1],"[,.]")
# title[[1]][2]
#Title=sub(" ","",title[[1]][2]) # to remove the space from the Title

# To get the titles of each row into a new variable we use sapply function
alldata$Title=sapply(alldata$Name,function(x) {strsplit(x,split="[,.]")[[1]][2]})

alldata$Title[1]
# [1] " Mr"

#To remove the spaces which comes from the Title,
alldata$Title=sub(" ","",alldata$Title)
table(alldata$Title)

alldata2=alldata

# FILLING MISSING AGE VALUES


#============================
# ************   METHOD 1: From the Title
#=============================
#We will fill the remaining Age value with the mean of the Age of the same Title.
# First let us see which Titles has missing age values
table(alldata$Title,is.na(alldata$Age))

#A function to calculate the Mean Age among the titles passed to the function

Impute.age=function(title){
  c=which(alldata$Title==title)
  mean=mean(alldata$Age[c],na.rm=TRUE)
  return(mean)
}
# e.g Impute.age("Dr")
#  43.57

alldata$Age[ which(alldata$Title=="Dr" & (is.na(alldata$Age)))]=Impute.age("Dr")  # This fills the missing Age of DR
# To verify
table(alldata$Title,is.na(alldata$Age))

alldata$Age[ which(alldata$Title=="Master" & (is.na(alldata$Age)))]=Impute.age("Master") 
alldata$Age[ which(alldata$Title=="Miss" & (is.na(alldata$Age)))]=Impute.age("Miss") 
alldata$Age[ which(alldata$Title=="Mr" & (is.na(alldata$Age)))]=Impute.age("Mr") 
alldata$Age[ which(alldata$Title=="Mrs" & (is.na(alldata$Age)))]=Impute.age("Mrs") 
alldata$Age[ which(alldata$Title=="Ms" & (is.na(alldata$Age)))]=Impute.age("Ms") 

# To verify again
table(alldata$Title,is.na(alldata$Age))

str(alldata)
summary(alldata)

#============================
# ************   METHOD 2: by doing a linear regression with Age as theindependent variable
#=============================
str(alldata2)
colnames(alldata2)
summary(alldata2)
linregAge=lm(Age~ Pclass+Sex+SibSp+Parch+Embarked+Title+Fare,data=alldata2)
summary(linregAge)
linregAge.pred=predict(linregAge,alldata2)
length(linregAge.pred)
alldata2$Age[which(is.na(alldata2$Age))]=linregAge.pred[which(is.na(alldata2$Age))]
summary(alldata2)

# Thus we have A dataset alldata2 which has all Age values filled by Liner regression
# as explained above for Method 2

# we will run all algorithms on the datasets formed by both Method 1 and Mehtod2

#================================================================

#Let us divide the alldata into training and test set as before

train1=head(alldata,nrow(train_input))
test1=tail(alldata,nrow(test_input))
train1$Survived= Survived
str(train1)

#-----------
# similarly for the dataset created by Method 2
train2=head(alldata2,nrow(train_input))
test2=tail(alldata2,nrow(test_input))
train2$Survived= Survived
str(train2)
#=========================================================
#                 ********* TABLEAU ***********
#=========================================================
# as can be seen in --->  https://public.tableau.com/profile/publish/Titanic_Female_Pclass3/Dashboard1#!/publish-confirm
# All female passengers of Class 3 who payed above $25 have not survived. We will do the same in the test set

# we will overwrite this logic above all our Output of the Models


#=====================
#LOGISTIC REGRESSION :#
#=====================
# Dataset obtained by Method 1

logreg1=glm(Survived~ Pclass+Sex+Age+SibSp+Embarked,data=train1)
summary(logreg1)
logreg1.predtrain=predict(logreg1,type="response")

#Finding appropriate threshold from ROC
ROCR=prediction(logreg1.predtrain,train1$Survived)
ROCCurve=performance(ROCR,"tpr","fpr")
plot(ROCCurve,colorize=T)
# The ROC curve shows that 0.6 will be a good threshold value

table(train1$Survived,logreg1.predtrain>=0.6) # to test the train set accuracy
# 81.8%
# Apply the threshold to the test set
logreg1.predtest=(predict(logreg1,type="response",newdata=test1)>=0.6)

test1$Survived=ifelse(logreg1.predtest>=0.6,1,0)
test1$Survived[which(test1$Pclass==3 & test1$Fare>=25)]=0

PassengerId =test1$PassengerId
Survived=test1$Survived

submit=data.frame(PassengerId,Survived)

write.csv(submit,file="submit.csv",row.names=FALSE)
table(submit$Survived)


#-----------------------------------------------------------------
# Dataset obtained by Method 2

logreg2=glm(Survived~Pclass+Sex+Age+SibSp+Embarked,data=train2)
summary(logreg2)
logreg2.predtrain=predict(logreg2,type="response")
#Finding appropriate threshold from ROC
ROCR2=prediction(logreg2.predtrain,train2$Survived)
ROCCurve2=performance(ROCR2,"tpr","fpr")
plot(ROCCurve2,colorize=T)
# The ROC curve2 shows that 0.6 will be a good threshold value

# to test the train set accuracy
table(train2$Survived,logreg2.predtrain>=0.6)
# 81.9%
# Apply the threshold to the test set
logreg2.predtest=(predict(logreg2,type="response",newdata=test2)>=0.6)

PassengerId =test2$PassengerId

test2$Survived=ifelse(logreg2.predtest>=0.6,1,0)
test2$Survived[which(test2$Pclass==3 & test2$Fare>=25)]=0
Survived=test2$Survived

submit2=data.frame(PassengerId,Survived)
write.csv(submit2,file="submit2.csv",row.names=FALSE)
table(submit2$Survived)

#=====================================================================


















