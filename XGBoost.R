getwd()


#Reading the dataset
train <- read.csv("train_users_a.csv")
View(train)

#Cleaning
#Converting to date
train$date_account_created <- as.Date(train$date_account_created)
train$date_first_booking[train$date_first_booking==""]<- NA
train$date_first_booking <- as.Date(train$date_first_booking, na.rm=TRUE)

#Cleaning age
train$age<-ifelse(is.na(train$age),49.66834,train$age)

#Difference between date_account_created and date_first_booking
train$Number_of_days <- train$date_account_created-train$date_first_booking

#running the logistic model
logistic <- multinom(formula=country_destination~age+gender+language+signup_method+signup_flow+affiliate_channel+affiliate_provider+first_affiliate_tracked+signup_app+first_device_type+first_browser, data = train, maxit = 1000, MaxNWts = 4000)
remove(predicted_destination)

#predicting on logistic model
predicted_destination <- predict(logistic, data= test, arg="class")

#Confusion Matrix
table(train$country_destination, predicted_destination)

