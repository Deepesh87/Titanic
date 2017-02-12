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

#=====================================================================new analysis
#________________________________________________________________________________________________________________________-----
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

train_input=read.csv("train.csv")
test_input=read.csv("test.csv")
train=train_input#make copy of train
test=test_input#make copy of test
#delete Survived in train and merge train and test
train$Survived=NULL
all=rbind(train,test)
str(all)
summary(all)
#lets start filling the missing values
#row no. of Fare missing
which(is.na(all$Fare))
all$Fare[is.na(all$Fare)]=mean(all$Fare[all$Pclass==all$Pclass[is.na(all$Fare)] & all$Embarked==all$Embarked[is.na(all$Fare)]],na.rm=T)
#fill missing embarked
table(all$Embarked)
# fill the missing table with most embarked i.e S
all$Embarked[all$Embarked==""]='S'
#check
table(all$Embarked)
#relevel the factors
all$Embarked=factor(all$Embarked)
#__________________________________________
#fill the missing ages based on the Names
#split names into separate Title, First and last name
all$Name=as.character(all$Name)
all$last=sapply(all$Name,function(x){strsplit(x,"[,.]")[[1]][1]})
all$title=sapply(all$Name,function(x){strsplit(x,"[,.]")[[1]][2]})
all$title=sub(" ","",all$title)
all$last=as.factor(all$last)
all$title=as.factor(all$title)
table(all$title,is.na(all$Age))
#write a function which outputs the mean age
#if given a Title
fun=function(x){
  return(mean(all$Age[all$title==x],na.rm=T))
}
#fill the missing ages
all$Age[all$title=="Dr"& is.na(all$Age)]=fun("Dr")
all$Age[all$title=="Master"& is.na(all$Age)]=fun("Master")
all$Age[all$title=="Mrs"& is.na(all$Age)]=fun("Mrs")
all$Age[all$title=="Ms"& is.na(all$Age)]=fun("Ms")
all$Age[all$title=="Mr"& is.na(all$Age)]=fun("Mr")
all$Age[all$title=="Miss"& is.na(all$Age)]=fun("Miss")
#****************************************
#we change the Fare=0 to the mean Fare of each Pclass
all$Fare[all$Fare==0 & all$Pclass==1]=mean(all$Fare[all$Pclass==1])
all$Fare[all$Fare==0 & all$Pclass==2]=mean(all$Fare[all$Pclass==2])
all$Fare[all$Fare==0 & all$Pclass==3]=mean(all$Fare[all$Pclass==3])
#*****************************************
library(ggplot2)
train_input$Survived=as.factor(train_input$Survived)
ggplot(data=train_input,aes(x=SibSp+1+Parch,fill=Survived))+ geom_histogram()
#above shows more than those with more than 4 family members did not survive
#create a new variables wich gves the total family size
all$FamSize=sapply(all$SibSp+all$Parch+1,sum)
table(all$FamSize)
#**************************************************************************
#Ticket number
sum(all$Ticket=="")#count blanks
head(all$Ticket,10)
#extract first letter/number and the total # of letters/characters in the ticket
func1=function(x){nchar(x)}
all$Ticket_char_no=sapply((as.character(all$Ticket)),func1)
func2=function(x){
  strsplit(x,"")[[1]][1]
}
all$Ticket_first_no=sapply((as.character(all$Ticket)),func2)
all$Ticket_first_no=as.factor(all$Ticket_first_no)
all$Ticket_char_no=as.factor(all$Ticket_char_no)
#**************************************************************************
#CABIN number
#the first letter in the cabin number might be useful
#use func2 made above
all$Cabin_num=sapply(as.character(all$Cabin),func2)
all$Cabin_num=as.factor(all$Cabin_num)

all$Pclass=as.factor(all$Pclass)
#_________________________________________________________________
#lets do a clustering and see if that helps
set.seed(1) # for reproducibility
library(dplyr)
library(ISLR)
library(cluster)
library(Rtsne)
#calculate Gower's Distance
gower_dist <- daisy(all[, c(-1,-3,-8,-10)],
                    metric = "gower",
                    type = list(logratio = 3))
summary(gower_dist)
#print out the most similar and dissimilar pair 
gower_mat <- as.matrix(gower_dist)
#most similar
all[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
#most different
all[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
#************
#selecting number of clusters
#silhoutte width
sil_width <- c(NA)
for(i in 2:10){pam_fit <- pam(gower_dist,diss = TRUE,k = i)
                              sil_width[i] <- pam_fit$silinfo$avg.width}

# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
#2
pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
#names(pam_fit)
all$cluster=pam_fit$clustering
all$cluster=as.factor(all$cluster)
#______________________________________________________________________
#count the number of letters in name
func3=function(x){
  nchar(as.character(x))
}
all$name_count=sapply(all$Name,func3)
#*************************************
#split data back to train and test
train=head(all,nrow(train_input))
test=tail(all,nrow(test_input))
train$Survived=train_input$Survived
#************************************************************************
############################LOGISTIC REGRESSION
logreg=glm(Survived~Pclass+Sex+Age+Embarked+FamSize+Ticket_char_no+Ticket_first_no+name_count,data=train,family="binomial")
summary(logreg)
logreg.pred=predict(logreg,type="response")
table(logreg.pred>0.5,train$Survived)
#81.4% on training
#lets find the threshold from the ROC curve
library(ROCR)
ROCR=prediction(logreg.pred,train$Survived)
ROCCurve=performance(ROCR,"tpr","fpr")
plot(ROCCurve,colorize=T)
#looks like 0.60 is a good threshold
#predict on test
logreg.pred2=predict(logreg,newdata=test,type="response")
test$Survived=ifelse(logreg.pred2>0.6,1,0)
submit=test[,c("PassengerId","Survived")]
write.csv(submit,"sub_12thFeb_lm.csv",row.names = F)
#This scored poorly on leaderboard 
#only 77%
#*************************************************************************
#************************************************************************
#lets build a decision rf to identify the important variables
library(randomForest)
rf=randomForest(Survived~Pclass+Embarked+cluster+Ticket_char_no+Ticket_first_no+Sex+Age+Fare+FamSize+title+name_count,data=train,ntree=500,nodesize=8,mtry=9)
names(rf)
var=importance(rf)
var
#predict
rf.pred=predict(rf,newdata=test)
test$Survived=rf.pred
submit2=test[,c("PassengerId","Survived")]
write.csv(submit2,"sub_12thFeb_rf1.csv",row.names = F)
#*******************************************************
#cross validation
library(caret)
library(e1071)
numfolds=trainControl(method="cv",number=10) # cv= cross validation, 10 folds
cpgrid=expand.grid(.cp=seq(0.0001,0.2,0.001))
train(Survived~Pclass+Embarked+cluster+Ticket_char_no+Ticket_first_no+Sex+Age+Fare+FamSize+title+name_count,data=train,method="rpart",trControl=numfolds,tuneGrid=cpgrid)
# here we used method="rpart"since we are cross validating a cart model
rf2=randomForest(Survived~Pclass+Embarked+cluster+Ticket_char_no+Ticket_first_no+Sex+Age+Fare+FamSize+title,data=train,cp=0.0041)
predictCV=predict(rf2,newdata=test,type="class")
test$Survived=predictCV
submit3=test[,c("PassengerId","Survived")]
write.csv(submit3,"sub_12thFeb_rf_cv1.csv",row.names = F)
#*******************************************************
#random search for the mtry parameter
#http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(1)
tunegrid <- expand.grid(.mtry=c(3:9))
rf_gridsearch <- train(Survived~Pclass+Embarked+cluster+Ticket_char_no+Ticket_first_no+Sex+Age+Fare+FamSize+title,data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
#*******************

























