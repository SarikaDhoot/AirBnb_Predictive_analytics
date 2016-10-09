library(nnet)
library(lubridate)

#Reading the dataset
airbnb <- read.csv("train_users_a.csv")
#View(airbnb)
head(airbnb)

#Cleaning
#Converting to date
airbnb$date_account_created <- as.Date(airbnb$date_account_created,"%m/%d/%Y")
airbnb$date_first_booking[airbnb$date_first_booking==""]<- NA
airbnb$date_first_booking <- as.Date(airbnb$date_first_booking, na.rm=TRUE, "%m/%d/%Y")

#Mean of age
meanage=subset(airbnb,age!="NA")
age=mean(meanage$age)
#View(meanage)
age

#Cleaning age
airbnb$age<-ifelse(is.na(airbnb$age),age,airbnb$age)

#Difference between date_account_created and date_first_booking
airbnb$Number_of_days <- airbnb$date_account_created-airbnb$date_first_booking

#Splitting data
set.seed(12345)
airbnb_sample <- sample(1:nrow(airbnb),0.7*nrow(airbnb))
airbnb_train <- airbnb[airbnb_sample,]
airbnb_valid <- airbnb[-airbnb_sample,]

#running the logistic model
logistic <- multinom(formula=country_destination~age+gender+language+signup_method+signup_flow+affiliate_channel+affiliate_provider+first_affiliate_tracked+signup_app+first_device_type+first_browser, data = airbnb_train, maxit = 100, MaxNWts = 4000)
#backlog <- step(logistic)
View(airbnb_train)
nrow(airbnb_valid)

#predicting on logistic model
predicted_destination <- predict(logistic, newdata= airbnb_valid, arg="class")
airbnb_valid$Predicted= predicted_destination
predicted_destination
unique(predicted_destination)
write.csv(predicted_destination,file="multinompredict.csv")

PredictedProb = predict(logistic, newdata= airbnb_valid, "probs")
airbnb_valid$PredictedProb= PredictedProb
PredictedProb

#Confusion Matrix
table(airbnb_valid$Predicted,airbnb_valid$country_destination)
unique(predicted_destination)

#Accuracy
accuracy=nrow(airbnb_valid[airbnb_valid$country_destination==airbnb_valid$Predicted,])/nrow(airbnb_valid)
accuracy

#US and everything else
airbnb_train$US=ifelse(airbnb_train$country_destination=="US",1,0)
airbnb_valid$US=ifelse(airbnb_valid$country_destination=="US",1,0)
logistic1 <- multinom(formula=US~age+gender+language+signup_method+signup_flow+affiliate_channel+affiliate_provider+first_affiliate_tracked+signup_app+first_device_type+first_browser, data = airbnb_train, maxit = 100, MaxNWts = 4000)
PredictedProb = predict(logistic1, newdata= airbnb_valid, "probs")
airbnb_valid$Predicted=PredictedProb
airbnb_valid$PredictedUS=ifelse(airbnb_valid$Predicted>0.5,1,0)
matrix_Table = table(airbnb_valid$US,airbnb_valid$PredictedUS,dnn=list('actual','predicted'))
matrix_Table

accuracy=nrow(airbnb_valid[airbnb_valid$US==airbnb_valid$PredictedUS,])/nrow(airbnb_valid)
accuracy

cutoff <- seq(0, 1, length = nrow(airbnb_valid))
fpr <- numeric(nrow(airbnb_valid))
tpr <- numeric(nrow(airbnb_valid))
## We'll collect it in a data frame.  (We could also just keep it in three vectors)
roc.table <- data.frame(Cutoff = cutoff, FPR = fpr,TPR = tpr)
## TPR is the Sensitivity; FPR is 1-Specificity
for (i in 1:nrow(airbnb_valid)) {
  roc.table$FPR[i] <- sum(airbnb_valid$Predicted > cutoff[i] & airbnb_valid$US == 0)/sum(airbnb_valid$US == 0)
  roc.table$TPR[i] <- sum(airbnb_valid$Predicted > cutoff[i] & airbnb_valid$US == 1)/sum(airbnb_valid$US == 1)
}
plot(TPR ~ FPR, data = roc.table, type = "o",xlab="1 - Specificity",ylab="Sensitivity",col="green",lty=1)
abline(a = 0, b = 1, lty = 2,col="black")
