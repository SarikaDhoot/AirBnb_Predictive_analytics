getwd()
setwd("/Users/roma-kaul/Downloads/Data Mining")

#Reading the dataset
airbnb_og <- read.csv("train_users_a.csv")
View(airbnb_og)



#Cleaning
#Converting to date
airbnb$date_account_created <- as.Date(airbnb$date_account_created, "%m/%d/%y")
airbnb$date_first_booking[airbnb$date_first_booking==""]<- NA
airbnb$date_first_booking <- as.Date(airbnb$date_first_booking, na.rm=TRUE,"%m/%d/%y")

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


#Random Forest
set.seed(12345)
rf.airbnb=randomForest(formula = country_destination~age+gender+signup_method+signup_flow+language
                       +affiliate_channel+affiliate_provider+first_affiliate_tracked+signup_app
                       +first_device_type,data=airbnb_train,mtry=2, nodesize = 45, importance=TRUE)
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

#Boosting Algorithm - rpart package
# boost.airbnb=gbm(country_destination~age+gender+signup_method+signup_flow+language+affiliate_channel+affiliate_provider+first_affiliate_tracked+signup_app+first_device_type, data=airbnb_train,
#                  distribution="gaussian",n.trees= 3000,interaction.depth=4)
install.packages("rpart")
library("rpart")
rpart.airbnb=rpart(country_destination~age+gender+signup_method+signup_flow+language
                   +affiliate_channel+affiliate_provider+first_affiliate_tracked+signup_app
                   +first_device_type, data=airbnb_train,
                 maxdepth =8)

#predicting -- rpart
yhat.boost= predict(rpart.airbnb,newdata=airbnb_validation,n.trees=3000, type='class')
write.csv(prediction,file="rpartPredict.csv")
#Confusion Matrix for rpart
table(airbnb_validation$country_destination,yhat.boost)

#Accuracy
accuracy <- nrow(airbnb_validation[airbnb_validation$country_destination==yhat.boost,])/(nrow(airbnb_validation))
accuracy



--#adaboost --  package adabag -- says " k  not found". Couldnt resolve.
airbnb.adaboost <- boosting(country_destination~age+gender+signup_method+signup_flow+language
                        +affiliate_channel, data= airbnb_train , mfinal=2, coeflearn="Breiman", 
                        boos= FALSE )
?boosting
#Using the pruning option
BC.adaboost.pred <- predict.boosting(airbnb.adaboost,newdata=airbnb_validation, newmfinal=10)
BC.adaboost.pred$confusion
BC.adaboost.pred$error


