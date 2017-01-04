library(data.table)
library(zoo)
library(forecast)
library(ggplot2)

test <- fread("C:/Users/ratho/Desktop/rossmann/input_data/test.csv")
train <- fread("C:/Users/ratho/Desktop/rossmann/input_data/train.csv")
store <- fread("C:/Users/ratho/Desktop/rossmann/input_data/stores.csv")

train[, Date := as.Date(Date,"%m/%d/%Y")]
test[, Date := as.Date(Date)]

train$Date  = as.Date(train$Date,"%m/%d/%Y")
test$Date  = as.Date(test$Date,"%m/%d/%Y")

train <- train[order(Date)]
test <- test[order(Date)]

# Convert the CompetitionOpenSince... variables to one Date variable
store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, 
                                               store$CompetitionOpenSinceMonth, sep = "-"))

# Convert the Promo2Since... variables to one Date variable
# Assume that the promo starts on the first day of the week
store$Promo2Since <- as.POSIXct(paste(store$Promo2SinceYear, 
                                      store$Promo2SinceWeek, 1, sep = "-"),
                                format = "%Y-%U-%u")
# Merge store and train 
train_store <- merge(train, store, by = "Store")


#converting categorical data in factors
train_store$Store= as.factor(train_store$Store)
train_store$DayOfWeek= as.factor(train_store$DayOfWeek)
train_store$Open = as.factor(train_store$Open)
train_store$Promo = as.factor(train_store$Promo)
train_store$StateHoliday = as.factor(train_store$StateHoliday)
train_store$SchoolHoliday = as.factor(train_store$SchoolHoliday)
train_store$StoreType = as.factor(train_store$StoreType)
train_store$Assortment = as.factor(train_store$Assortment)
train_store$Promo2 = as.factor(train_store$Promo2)
train_store$month = as.factor(format(train_store$Date, "%m")) 
train_store$year = as.factor(format(train_store$Date, "%Y"))
#train_store$week = as.factor(format(train_store$Date, "%W"))


#merging test and store to apply model
test_store <- merge(test, store, by = "Store")

test_store$Store= as.factor(test_store$Store)
test_store$DayOfWeek= as.factor(test_store$DayOfWeek)
test_store$Open = as.factor(test_store$Open)
test_store$Promo = as.factor(test_store$Promo)
test_store$StateHoliday = as.factor(test_store$StateHoliday)
test_store$SchoolHoliday = as.factor(test_store$SchoolHoliday)

test_store$StoreType = as.factor(test_store$StoreType)
test_store$Assortment = as.factor(test_store$Assortment)
test_store$Promo2 = as.factor(test_store$Promo2)

test_store$month = as.factor(format(test_store$Date, "%m")) 
test_store$year = as.factor(format(test_store$Date, "%Y")) 
#test_store$week = as.factor(format(test_store$Date, "%W")) 


#str(test_store)
#str(train_store)