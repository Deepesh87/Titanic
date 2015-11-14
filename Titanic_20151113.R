# read training data
train=read.csv("train.csv",stringsAsFactors=FALSE)
table(train$Pclass,train$Survived) # this shows that most passengers who died were of 3rd class
table(train$Pclass,train$Survived,train$Sex) # this shows that almost all male passengersof 3rd class died
# read test data
test=read.csv("test.csv",stringsAsFactors=FALSE)

# There are missing values in Age, Embarked,Fare variables. Let us combine train and
# test data and try to fill the missing values with appropriate values based on other variables

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




































