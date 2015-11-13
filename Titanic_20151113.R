# read training data
train=read.csv("train.csv")

# read test data
test=read.csv("test.csv")

# There are missing values in Age and some other variables. Let us combine train and
# test data and try to fill the missing values with appropriate values based on other variables

Survived=train$Survived

train$Survived=NULL
colnames(train)

alldata=rbind(train,test)
str(alldata)
summary(alldata)





















































