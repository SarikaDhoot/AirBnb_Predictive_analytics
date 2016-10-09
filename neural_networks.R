#Data Mining Project - Team D
#Neural Networks

#Cleaning the dataset

data1 <- read.csv("train_users_a.csv")

#removing NDF rows
data1$date_account_created<-as.Date(data1$date_account_created,"%m%d%y")
#data1<-sample(nrow(data1),0.1*nrow(data1))

#data1=subset(data1,data1$country_destination!="NDF")


mean(data1$age,na.rm=TRUE)
#47.87263 is the mean age value
data1$age<-ifelse(is.na(data1$age),47.87263,data1$age)

data1$date_first_booking[data1$date_first_booking==""] <- NA
data1$date_first_booking<-as.Date(data1$date_first_booking,na.rm=TRUE)

#Splitting the data
set.seed(12345)
inTrain <- sample(nrow(data1), 0.7*nrow(data1))
#training set
train_data <- data.frame(data1[inTrain,])
#Validation set
test_data <- data.frame(data1[-inTrain,])



#using nnet function
library(nnet)
nn <- nnet(country_destination ~ age+gender+signup_flow+language+signup_app+signup_flow+signup_method
             +affiliate_channel+affiliate_provider+first_affiliate_tracked+
             first_device_type+first_browser, size=5, maxit=1000, 
           data=train_data) 
table(predict(nn, train_data, type = "class"), train_data$country_destination)
unique(predict(nn, train_data, type = "class"))
#accuracy = (1+1+2+43591)/(88908) = 49.3%




 