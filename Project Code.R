# KDD Final Project
# Darshak
# Urvi
# Sunidhi
# Ravil
# Nikita


# Deliverable - Data Pre-processing
library(ggplot2)
train <- read.csv(file="C:/Users/darsh/Desktop/New folder/train.csv",stringsAsFactors = FALSE)
test <- read.csv(file="C:/Users/darsh/Desktop/New folder/test.csv",stringsAsFactors = FALSE)
store <- read.csv(file="C:/Users/darsh/Desktop/New folder/store.csv",stringsAsFactors = FALSE)
train
str(test)
str(train)
str(store)

summary(train)
summary(test)
summary(store)

#No sales for stores that are closed
#OpenStores Contains only those stores that are open
summary(train$Sales[!train$Open])

length(train$Sales[!train$Open]) / nrow(train)
OpenStores <- train[train$Open,]
summary(OpenStores)
train <- train[train$Open,]
summary(OpenStores)

#Test dataset has fewer values
#Checking for unique values reduces the data to be preprocessed by approx 24%
teststores <- as.numeric(as.character(unique(test$Store)))
teststores[1:10]
nrow(train)
nrow(test)
train <- train[train$Store %in% teststores,]
nrow(train)

#We see that all NA values pertain to a single store (622), they are for consecutive days except the Sundays in this period, 
#and contain all other days (Mondays to Saturdays). Furthermore, there are four days at the end of the period when there is an 
#active promotion running in this store. Since the 
#intervening Sundays are explicitly marked as 0, we can assume that these are all open days. Therefore I set the NA values to true.
test[is.na(test$Open),]
test[is.na(test)] <- 1
sapply(test, function(x) length(unique(x)))
sapply(train, function(x) length(unique(x)))
sapply(store, function(x) length(unique(x)))

#EDA
summary(train$Sales)

sd(train$Sales)

hist(train$Sales,xlab="Sales")
#To determine outliers
boxplot(train$Sales)

summary(train[train$Sales > 20000,])
#This indicates sales above 20000 are not outliers

#check if there is sufficient historic data
openDays <- aggregate(train$Store,list(train$Store),length)
openDays
summary(openDays)


t.test(train[train$Promo,]$Sales,train[!train$Promo,]$Sales)
t.test(train[train$StateHoliday != 0,]$Sales,train[train$StateHoliday == 0,]$Sales)
t.test(train[train$StateHoliday != 0,]$Sales,train[train$StateHoliday == 0,]$Sales)
t.test(train[train$SchoolHoliday,]$Sales,train[!train$SchoolHoliday,]$Sales)

a<- tapply(train$Sales,train$Date,mean)
plot(a)

plot(tapply(train$Sales,train$DayOfWeek,mean))
hist(train$Open)
hist(train$Customers)
hist(train$SchoolHoliday)
hist(train$Promo)


# date 
#train$Date
#splitdate <- strsplit(train$Date, "/")
#matrix(unlist(splitdate), ncol=3, byrow=TRUE)
train$Day
train$Month
train$Year


plot(tapply(train$Sales,train$Month,mean),xlab="Month",ylab="Mean of Sales")
unique_months <- unique(train$Month)
length(unique_months)

plot(tapply(train$Sales,train$Day,mean),xlab="Day",ylab="Mean of Sales")
unique_days <- unique(train$Day)
length(unique_days)
summary(train$Day)


plot(tapply(train$Sales,train$Year,mean),xlab="Year",ylab="Mean of Sales")
hist(train$Year)
unique_years <- unique(train$Year)
unique_years
length(unique_years)

plot(tapply(train$Sales,train$Dayofweek,mean),xlab="DayofWeek",ylab="Mean of Sales")

hist(train$Promo)
barplot(train$Promo) 

unique_promo <- unique(train$Promo)
unique_promo
plot(tapply(train$Sales,train$Promo,mean),xlab="Promo",ylab="Mean of Sales")

#merging
m=merge(train,store,by="Store")
m
unique_competition_distance <- unique(m$CompetitionDistance)
unique_competition_distance

summary(m$CompetitionDistance)

#Modelling techniques

install.packages(c("Rcpp", "readr"))
library(readr)
install.packages("randomForest")
library(randomForest)

train <- read.csv(file="C:/Users/darsh/Desktop/New folder/train.csv",stringsAsFactors = FALSE)
test <- read.csv(file="C:/Users/darsh/Desktop/New folder/test.csv",stringsAsFactors = FALSE)
store <- read.csv(file="C:/Users/darsh/Desktop/New folder/store.csv",stringsAsFactors = FALSE)

# Choosing only open stores
train <- train[ which(train$Open=='1'),]

# Removing columns Promo2SinceWeek, PromoInterval, Promo2SinceYear due to a high number of missing values
colDrop <- c( "Promo2SinceWeek", "PromoInterval", "Promo2SinceYear")
store <- store[ , !(names(store) %in% colDrop)]

# Replacing missing values of CompetitionOpenSinceMonth with its mean value
meanForNA <- na.omit(store$CompetitionOpenSinceMonth)
store$CompetitionOpenSinceMonth[is.na(store$CompetitionOpenSinceMonth)] <- as.integer(mean(meanForNA))

# Replacing missing values of CompetitionOpenSinceYear with its mean value
meanForNA <- na.omit(store$CompetitionOpenSinceYear)
store$CompetitionOpenSinceYear[is.na(store$CompetitionOpenSinceYear)] <- as.integer(mean(meanForNA))

# Replacing missing values of CompetitionDistance with its mean value
meanForNA <- na.omit(store$CompetitionDistance)
store$CompetitionDistance[is.na(store$CompetitionDistance)] <- as.integer(mean(meanForNA))

# Merging train & store and test & store data
train <- merge(train,store)
test <- merge(test,store)

# Spliting character variable date of train and test datasets into month, year, and day variables
train$Date <- as.Date(train$Date, format = "%m/%d/%y")
train$month <- as.integer(format(train$Date, "%m"))
train$year <- as.integer(format(train$Date, "%y"))
train$day <- as.integer(format(train$Date, "%d"))
names(train)
train <- train[,-c(3)]

test$Date <- as.Date(test$Date, format = "%m/%d/%y")
test$month <- as.integer(format(test$Date, "%m"))
test$year <- as.integer(format(test$Date, "%y"))
test$day <- as.integer(format(test$Date, "%d"))
names(test)
test <- test[,-c(4)]

# Creating dummy variables for categorical data
predictor.names <- names(train)[c(1:17)]
predictor.names

# Loop through the variable classes, and change all the factor to numeric.
# Doing so, will help us obtain the correlation matrix
for (i in predictor.names) {
  if (class(train[[i]])=="Factor") {
    cat(i)
    levels <- unique(c(train[[i]], test[[i]]))
    train[[i]] <- as.integer(factor(train[[i]],  levels=levels))
    test[[i]]  <- as.integer(factor(test[[i]],  levels=levels))
  }
}

# Splitting data into train and test sets

set.seed(12345)
n <- nrow(train)
shuffled <- train[sample(n),]
train_train <- shuffled[1:round(0.7 * n),]
train_validation <- shuffled[(round(0.7 * n) + 1):n,]

# create files to write the updated train, test and validation datasets

write.csv(train_train, "C:/Users/darsh/Desktop/New folder/train_trainData.csv")
write.csv(train_validation, "C:/Users/darsh/Desktop/New folder/train_validationData.csv")
write.csv(test, "C:/Users/darsh/Desktop/New folder/test_store_combined.csv")
train_train <- read.csv("C:/Users/darsh/Desktop/New folder/train_trainData.csv", header = TRUE)
train_validation <- read.csv("C:/Users/darsh/Desktop/New folder/train_validationData.csv", header = TRUE)
test <- read.csv("C:/Users/darsh/Desktop/New folder/test_store_combined.csv", header = TRUE)

names(train_train)
names(test)

# check for the class of the variables of train_train
sapply(train_train, class)

drops <- c("DayOfWeek","Sales","Customers","Open","Promo","SchoolHoliday")
corelationData <- train[, (names(train) %in% drops)]
corelationData
#correlation matrix
cor(corelationData)
# cor(train_train)

# train_train$CompetitionDistanceNorm <- rnorm(train_train$CompetitionDistance)


#Linear regression to predict the value with Open
linearRegession <- lm( Customers ~ DayOfWeek + Open + Promo + StateHoliday + SchoolHoliday + StoreType + Assortment + CompetitionDistance + CompetitionOpenSinceMonth + CompetitionOpenSinceYear + Promo2 + month + year + day, data=train_train)    
summary(linearRegession)

#Linear regression to predict the value without Open
linearRegessionImp <- lm( Customers ~ DayOfWeek  + Assortment + Promo + StateHoliday + SchoolHoliday +CompetitionDistance + CompetitionOpenSinceMonth + CompetitionOpenSinceYear + Promo2 + month + year + day, data=train_train)    
summary(linearRegessionImp)

names(train_train)
# Consider features "DayOfWeek" and "Promo"
feature.names <- names(train_train)[c(3,7,9:18)]

# check for factors
for (i in feature.names) {
  if (class(train[[i]])=="character") {
    levels <- unique(c(train_train[[i]], test[[i]]))
    train_train[[i]] <- as.integer(factor(train_train[[i]], levels=levels))
    train_validation[[i]] <- as.integer(factor(train_validation[[i]], levels=levels))
    test[[i]]  <- as.integer(factor(test[[i]],  levels=levels))
  }
}

names(test)

# Train data missing values
for ( i in colnames(train_train)) {
  temp1 <- sum(is.na(train_train[,i]))
  temp2 <- sum(train_train[,i] == "")
  print(paste0("missing values for ",i," is ",temp1,"-> NA Values and ",temp2,"-> missing values"))
}

# Validation data missing values

for ( i in colnames(train_validation)) {
  temp1 <- sum(is.na(train_validation[,i]))
  temp2 <- sum(train_validation[,i] == "")
  print(paste0("missing values for ",i," is ",temp1,"-> NA Values and ",temp2,"-> missing values"))
}

names(train_train)
# remove factor variables
train_train <- train_train[,-c(10,11)]
train_validation <- train_validation[,-c(10,11)]
names(train_train)

# consider feature variables
feature.names <- names(train_train)[c(3,7,9:16)]

names(train_validation)

#training model
train_model <- randomForest(train_train[,feature.names], 
                            log(train_train$Customers+1),
                            mtry = 6,
                            ntree = 20,
                            sampsize=150000, 
                            do.trace=TRUE,
                            nodesize = 15)

#prediction of customers
prediction <- exp(predict(train_model, train_validation)) -1
pred = (prediction)

submission <- data.frame(train_validation, predictedCustomers=pred)
write.csv(submission, "C:/Users/darsh/Desktop/New folder/train_store_combined_customers.csv")
names(test)

test <- test[,-c(9, 10)]

for ( i in colnames(test)) {
  temp1 <- sum(is.na(test[,i]))
  temp2 <- sum(test[,i] == "")
  print(paste0("missing values for ",i," is ",temp1,"-> NA Values and ",temp2,"-> missing values"))
}

names(test)
test <- test[ which(test$Open=='1'),]

# Predicting customers in test dataset
prediction <- exp(predict(train_model, test)) -1
pred = (prediction)
submission <- data.frame(test, Customers=pred)
write.csv(submission, "C:/Users/darsh/Desktop/New folder/test_store_combined_customers.csv")

# Sales Prediction
test <- read.csv("C:/Users/darsh/Desktop/New folder/test_store_combined_customers.csv", header = TRUE)
test$Customers <- as.integer(test$Customers)

# Feature selection
feature.names <- names(train_train)[c(3,5,7,9:16)]
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train_train[[f]], test[[f]]))
    train_train[[f]] <- as.integer(factor(train_train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

sales_model <- randomForest(train_train[,feature.names], 
                     log(train_train$Sales+1),
                     mtry = 6,
                     ntree = 20,
                     sampsize=150000, 
                     do.trace=TRUE,
                     nodesize = 15)

prediction <- exp(predict(sales_model, train_validation)) -1

pred = (prediction)
submission <- data.frame(train_validation, predictedSales=pred)
write.csv(submission, "C:/Users/darsh/Desktop/New folder/train_store_combined_salesPrediction.csv")

prediction <- exp(predict(sales_model, test)) -1
pred = (prediction)
submission <- data.frame(test, Sales=pred)
write.csv(submission, "C:/Users/darsh/Desktop/New folder/test_store_combined_salesPrediction.csv")

