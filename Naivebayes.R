#Loading the dataset
setwd("/Users/roma-kaul/Downloads/DataMining FinalProject Scripts/")
airbnbtrain <- read.csv("train_users_a.csv")

airbnbtrain$date_account_created<-as.Date(airbnbtrain$date_account_created, "%m/%d/%y")


#airbnbtrain$date_first_booking[airbnbtrain$date_first_booking==""] <- NA
airbnbtrain$date_first_booking<-as.Date(airbnbtrain$date_first_booking,na.rm=TRUE, "%m/%d/%y")

#using package lubridate
airbnbtrain$Accountcreatedmonth = month(airbnbtrain$date_account_created)
airbnbtrain$Accountcreatedmonth  = as.factor(airbnbtrain$Accountcreatedmonth )
airbnbtrain$signup_flow = as.factor(airbnbtrain$signup_flow)



# #Difference between date_account_created and date_first_booking
# airbnbtrain$Number_of_days <- train$date_account_created-train$date_first_booking
# test$Number_of_days <- test$date_account_created-test$date_first_booking


#filling missing age
meanage = mean(airbnbtrain$age,na.rm=TRUE)
meanage
airbnbtrain$age<-ifelse(is.na(airbnbtrain$age),meanage,airbnbtrain$age)

#Splitting the data
set.seed(12345)
inTrain <- sample(nrow(airbnbtrain), 0.7*nrow(airbnbtrain))
#training set
train <- data.frame(airbnbtrain[inTrain,])
#Validation set
test <- data.frame(airbnbtrain[-inTrain,])

train$country_destination = as.factor(train$country_destination)

table((train$country_destination))

# We require the library e1071
library(e1071)
df = data.frame(train$country_destination,train$age,train$gender,train$language,
                train$signup_method,train$affiliate_channel,train$affiliate_provider,train$first_affiliate_tracked,
                train$first_browser,
                train$signup_app,train$first_device_type)

# df = data.frame(airbnbtrain$country_destination,airbnbtrain$age,airbnbtrain$language,airbnbtrain$gender,
#  train$Accountcreatedmonth               airbnbtrain$signup_method,train$signup_flow,airbnbtrain$affiliate_channel,
#                 airbnbtrain$affiliate_provider,airbnbtrain$first_affiliate_tracked,
#                 airbnbtrain$signup_app,airbnbtrain$first_device_type,airbnbtrain$first_browser),train$first_browser
#train$first_affiliate_tracked
attach(df)

# naive bayes model
library(e1071)
model <- naiveBayes(df$train.country_destination~., data=df)
model

prediction <- predict(model, newdata = df[,-1])

unique(prediction)
#barplot()
write.csv(prediction,file="naiveBayesPred.csv")

matrix_Table = table(df$train.country_destination,prediction,dnn=list('actual','predicted'))
matrix_Table
model$apriori
barplot(model$apriori)
#accuracy calculation
accuracy = (matrix_Table[1,1]+matrix_Table[2,2]+matrix_Table[3,3]+matrix_Table[4,4]+matrix_Table[5,5]
          +matrix_Table[6,6]+matrix_Table[7,7]+matrix_Table[8,8]+matrix_Table[9,9]+matrix_Table[10,10]
          +matrix_Table[11,11])/ nrow(df)
accuracy
df1= data.frame(matrix_Table[1,1],matrix_Table[2,2],matrix_Table[3,3],matrix_Table[4,4],matrix_Table[5,5],
                 matrix_Table[6,6],matrix_Table[7,7],matrix_Table[8,8],matrix_Table[9,9],matrix_Table[10,10],
         matrix_Table[11,11])
#barplot(df1)
# calculating Recall and precision for top 3 elements

#Recall for AU
RecallAU= matrix_Table[1,1]/sum(matrix_Table[,1])
RecallAU

PrecisionAU = matrix_Table[1,1]/sum(matrix_Table[1,])
PrecisionAU

# Others
RecallOthers = matrix_Table[9,9]/sum(matrix_Table[,9])
RecallOthers

PrecisionOthers = matrix_Table[9,9]/sum(matrix_Table[9,])
PrecisionOthers

#Recall for US
RecallUS= matrix_Table[11,11]/sum(matrix_Table[,11])
RecallUS

PrecisionUS = matrix_Table[11,11]/sum(matrix_Table[11,])
PrecisionUS







