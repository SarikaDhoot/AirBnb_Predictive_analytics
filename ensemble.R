#Ensemble

data0 = read.csv("train_users_a.csv")
data1 <- read.csv("rf.csv")
data2 <- read.csv("multinompredict.csv")
data3 <- read.csv("rpartPredict.csv")


set.seed(12345)
inTrain <- sample(1:nrow(data0), 0.7*nrow(data0))
#training set
train_data <- data.frame(data0[inTrain,])
#Validation set
test_data <- data.frame(data0[-inTrain,])

df<-data.frame(data1$x,data2$x,data3$x)


# add library functional

require(functional)
predicted_ensemble<-apply(df, 1, Compose(table,
                    function(i) i==max(i),
                    which,
                    names,
                    function(i) paste0(i, collapse='/')
)
)
## Could not find compose
tableE = table(predicted_ensemble, test_data$country_destination)
Accuracy = tableE[1,11] / nrow(test_data)
Accuracy
