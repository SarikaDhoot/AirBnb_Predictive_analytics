#Data Mining Project - Team D
#Descriptive Analytics 

require(data.table)
require(ggplot2)
require(reshape2)
require(RColorBrewer)
require(labeling) ## changed
train_users_2 <- read.csv("train_users_2.csv")
train_users_2$date_account_created <- as.Date(train_users_2$date_account_created,"%m/%d/%Y")

attach(train_users_2)
train_users_2_clean <- train_users_2[age>=18 & age<=100,]

heatmapData <- as.matrix(table(train_users_2_clean$age,train_users_2_clean$date_account_created)^.2)
p <- qplot(x=Var2, y=Var1, data=melt(heatmapData), fill=value, geom="tile", 
           xlab = "Date Account Created", ylab = "Age", 
           main = "Number of Accounts Created Over Time Across Age")
p <- p + scale_x_discrete(breaks=levels(as.factor(train_users_2_clean$date_account_created))[c(TRUE, rep(FALSE, 90))], 
                          labels=levels(as.factor(train_users_2_clean$date_account_created))[c(TRUE, rep(FALSE, 90))])
p <- p + theme(axis.text.x  = element_text(angle=90, hjust=0.5, size=10))
p <- p + scale_fill_gradient(low="white", high="deeppink")
print(p)


heatmapData <- as.matrix(table(train_users_2_clean$signup_method,train_users_2_clean$date_account_created)^.2)
p <- qplot(x=Var2, y=Var1, data=melt(heatmapData), fill=value, geom="tile", 
           xlab = "Date Account Created", ylab = "Signup_method", 
           main = "Number of Accounts Created Over Time Across Signup Methods")
p <- p + scale_x_discrete(breaks=levels(as.factor(train_users_2_clean$date_account_created))[c(TRUE, rep(FALSE, 90))], 
                          labels=levels(as.factor(train_users_2_clean$date_account_created))[c(TRUE, rep(FALSE, 90))])
p <- p + theme(axis.text.x  = element_text(angle=90, hjust=0.5, size=10))
p <- p + scale_fill_gradient(low="white", high="orangered")
print(p)

