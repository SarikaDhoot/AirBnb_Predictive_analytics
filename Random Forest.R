getwd()
setwd("/Users/Rishabh/Documents/Semester Two/Data Mining and Predictive Analytics/Project")

#Reading the dataset
airbnb_og <- read.csv("train_users_a.csv")
View(airbnb_og)

airbnb <-subset(airbnb_og, airbnb_og$country_destination!="NDF")
View(airbnb)


#Cleaning
#Converting to date
airbnb$date_account_created <- as.Date(airbnb$date_account_created, "%m/%d/%Y")
airbnb$date_first_booking[airbnb$date_first_booking==""]<- NA
airbnb$date_first_booking <- as.Date(airbnb$date_first_booking, na.rm=TRUE,"%m/%d/%Y")

View(airbnb)

#Cleaning age
airbnb$age<-ifelse(is.na(airbnb$age),mean(airbnb$age, na.rm = TRUE),airbnb$age)

#Difference between date_account_created and date_first_booking
airbnb$Number_of_days <- airbnb$date_first_booking-airbnb$date_account_created

#Sampling
set.seed(12345)
train=sample(nrow(airbnb),0.7*nrow(airbnb))
airbnb_train=airbnb[train,]
airbnb_validation=airbnb[-train,]
View(airbnb_train)
View(airbnb_validation)

#Random Forest
set.seed(12345)
rf.airbnb=randomForest(formula = country_destination~gender+age+language+signup_method+signup_flow+affiliate_channel+affiliate_provider+signup_app+first_device_type,data=airbnb_train,mtry=3, nodesize = 40, importance=TRUE)

#Predicting
yhat.rf = predict(rf.airbnb,newdata=airbnb_validation)

unique(yhat.rf)

#Confusion Matrix
table(airbnb_validation$country_destination,yhat.rf)

#Performing variable selection
varImpPlot(rf.airbnb)

#Accuracy
accuracy <- nrow(airbnb_validation[airbnb_validation$country_destination==yhat.rf,])/(nrow(airbnb_validation))
accuracy

#write.csv(x = yhat.rf,"rf.csv")
