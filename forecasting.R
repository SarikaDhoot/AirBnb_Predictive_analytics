airbnb <- read.csv("train_users_a.csv")
#View(airbnb)
colnames(airbnb)
head(airbnb)

install.packages("lubridate")
library(lubridate)
install.packages("forecast")
library(forecast)

#Cleaning
#Converting to date
airbnb$date_account_created <- as.Date(airbnb$date_account_created, "%m/%d/%Y")
airbnb$date_first_booking[airbnb$date_first_booking==""]<- NA
#airbnb=airbnb[!(airbnb$date_first_booking==""),]
airbnb$date_first_booking <- as.Date(airbnb$date_first_booking, na.rm=TRUE, "%m/%d/%Y")
nrow(airbnb)

#Mean of age
meanage=subset(airbnb,age!="NA")
age=mean(meanage$age)
#View(meanage)
age

#Cleaning age
airbnb$age<-ifelse(is.na(airbnb$age),age,airbnb$age)

#Difference between date_account_created and date_first_booking
airbnb$Number_of_days <- airbnb$date_account_created-airbnb$date_first_booking

airbnb[,"Month"] = months(airbnb$date_account_created)
airbnb[,"Year"] = as.numeric(format(airbnb$date_account_created,'%Y'))
airbnb[,"Time"] = ifelse(airbnb$Month=="January",1,0)
airbnb[,"Time"] = ifelse(airbnb$Month=="February",2,airbnb$Time)
airbnb[,"Time"] = ifelse(airbnb$Month=="March",3,airbnb$Time)
airbnb[,"Time"] = ifelse(airbnb$Month=="April",4,airbnb$Time)
airbnb[,"Time"] = ifelse(airbnb$Month=="May",5,airbnb$Time)
airbnb[,"Time"] = ifelse(airbnb$Month=="June",6,airbnb$Time)
airbnb[,"Time"] = ifelse(airbnb$Month=="July",7,airbnb$Time)
airbnb[,"Time"] = ifelse(airbnb$Month=="August",8,airbnb$Time)
airbnb[,"Time"] = ifelse(airbnb$Month=="September",9,airbnb$Time)
airbnb[,"Time"] = ifelse(airbnb$Month=="October",10,airbnb$Time)
airbnb[,"Time"] = ifelse(airbnb$Month=="November",11,airbnb$Time)
airbnb[,"Time"] = ifelse(airbnb$Month=="December",12,airbnb$Time)

myData= data.frame(Year=unique(airbnb$Year))
nrow(myData)
sort(myData$Year, decreasing = FALSE)
myData$Year=sort(myData$Year, decreasing = FALSE)
Data=data.frame(Year=numeric(nrow(myData)*12),Time=numeric(nrow(myData)*12),Total=numeric(nrow(myData)*12),TS=numeric(nrow(myData)*12))
names(Data)
m=1
for(j in 1:nrow(myData))
{  
 for(i in 1:12)
 {
  Data[m,]$Year=myData[j,]
  Data[m,]$Time=i
  Data[m,]$TS=m
  m=m+1
 }
}
View(Data)

for(i in 1:nrow(Data))
{
  Data[i,]$Total=nrow(airbnb[airbnb$Year==Data[i,]$Year & airbnb$Time==Data[i,]$Time,])
}

Data=Data[!(Data$Total==0),]
Data

?plot
plot(Data$TS, Data$Total, xlab="Time Period",
     ylab="Total bookings" )
axis(side=1, at=seq(0, 100, by=12))
lines(Data$TS,Data$Total)

plot.ts(Data$Total)

Trate=HoltWinters(Data$Total,
                  #alpha = 0.2,
                  #beta = 0.1,
                  gamma = FALSE)
TPred=predict(Trate,n.ahead=18,prediction.interval=TRUE)
TPred

plot.ts(Data$Total,xlim=c(1,72),ylim=c(0,25000))
lines(Trate$fitted[,1],col="green")
lines(TPred[,1],col="blue")
lines(TPred[,2],col="red")
lines(TPred[,3],col="red")

forecast(Trate,h=18)

