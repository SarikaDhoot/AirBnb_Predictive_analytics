
airbnb.original <- read.csv("trainUserAirbnb.csv") 
airbnb_data = airbnb.original


library(lubridate)
names(airbnb_data)
head(airbnb_data)

### Extracting First Booking Month
airbnb_data$date_first_booking<-as.Date(airbnb_data$date_first_booking, format = "%m/%d/%Y")

airbnb_data$FirstBookingMonth = as.factor(month(airbnb_data$date_first_booking))

#### Extracting Month of Account Created
airbnb_data$date_account_created<-as.Date(airbnb_data$date_account_created, format = "%m/%d/%Y")

airbnb_data$AccountCreatedMonth =as.factor(month(airbnb_data$date_first_booking))


airbnb_data$signup_flow = as.factor(airbnb_data$signup_flow)



names(airbnb_data)
str(airbnb_data)
head(airbnb_data)


### Cleaning


mean(airbnb_data$age,na.rm=TRUE)
#47.87263 is the mean age value
airbnb_data$age<-ifelse(is.na(airbnb_data$age),47.87263,airbnb_data$age)
airbnb_data$date_account_created = NULL
airbnb_data$date_first_booking = NULL
airbnb_data$timestamp_first_active = NULL
airbnb_data$id= NULL


######## Start ARules #######

### Discretizing 'Age' for Arules 
airbnb_data$age = as.factor(airbnb_data$age)

#Splitting the data
#set.seed(12345)
#inTrain <- sample(nrow(airbnb_data), 0.7*nrow(airbnb_data))
#training set
#train <- data.frame(airbnb_data[inTrain,])
#Validation set
#test <- data.frame(airbnb_data[-inTrain,])


#str(train)
#head(train)

install.packages(Matrix)
library(arules)

# find association rules with default settings
rules = apriori(airbnb_data)
inspect(airbnb_data)



#We now set rhs=c("country_destination=US", "country_destination=FR", "country_destination=CA", "country_destination=GB",
#                 "country_destination=ES","country_destination=IT", country_destination=PT", "country_destination=NL"
#                 "country_destination=DE","country_destination=AU","country_destination=other")
#to make sure that only rules with rhs containing "country_destination" only.





# Support = 0.5% and Confidence = 80%
rules <- apriori(airbnb_data, parameter = list(minlen=2, supp=0.005, conf=0.8), 
                 appearance = list(rhs=c( "country_destination=US","country_destination=FR", "country_destination=CA", "country_destination=GB", "country_destination=ES","country_destination=IT", "country_destination=PT", "country_destination=NL", "country_destination=DE","country_destination=AU","country_destination=other"), default="lhs"), 
                 control = list(verbose=F))
# Sorting by Lift
rules.sorted <- sort(rules, by="lift")
summary(rules.sorted)
inspect(rules.sorted)


# Finding Redundant Rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)


#Visualizing Association Rules

install.packages( arules , scatterplot3d, vcd, seriation, igraph,"grid","cluster","TSP","gclus", "colorspace")
#install.packages("arulesViz")
install.packages(registry)
library(arulesViz)

plot(rules.pruned)

#sel = plot(rules.pruned, measure=c("support","lift"), shading="confidence", interactive=TRUE)

## Visualizing 1st 5 rules with US 

subrules = head(sort(rules.pruned, by="lift"),5)


plot(subrules, method="paracoord")

plot(subrules, method="graph")


plot(subrules, method="graph",interactive = TRUE)


#Removing US & Other


# Support = 10% and Confidence = 80%
rules <- apriori(airbnb_data, parameter = list(minlen=2, supp=0.05, conf=0.05), 
                 appearance = list(rhs=c("country_destination=FR", "country_destination=CA", "country_destination=GB", "country_destination=ES","country_destination=IT", "country_destination=PT", "country_destination=NL", "country_destination=DE","country_destination=AU","country_destination=other"), default="lhs"), 
                 control = list(verbose=F))


# Sorting by Lift
rules.sorted <- sort(rules, by="lift")
summary(rules.sorted)
inspect(rules.sorted)


# Finding Redundant Rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)


plot(rules.pruned)

## Visualizing 1st 5 rules 

subrules = head(sort(rules.pruned, by="lift"),5)


plot(subrules, method="graph",interactive = TRUE)



############End ARules#####################
####################