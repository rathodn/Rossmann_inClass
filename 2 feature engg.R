
f_train_store = train_store
f_test_store = test_store
f_store = store

# engineering in f_train_store


f_train_store$CompetitionDistance[is.na(f_train_store$CompetitionDistance)] = 5382
f_train_store$CompetitionOpenSinceMonth[is.na(f_train_store$CompetitionOpenSinceMonth)] = 4
f_train_store$CompetitionOpenSinceYear[is.na(f_train_store$CompetitionOpenSinceYear)] = 2009

f_train_store$CompetitionOpenSince <- as.yearmon(paste(f_train_store$CompetitionOpenSinceYear, 
                                                       f_train_store$CompetitionOpenSinceMonth, sep = "-"))


#replicating above code in f_store

f_store$CompetitionDistance[is.na(f_store$CompetitionDistance)] = 5382
f_store$CompetitionOpenSinceMonth[is.na(f_store$CompetitionOpenSinceMonth)] = 4
f_store$CompetitionOpenSinceYear[is.na(f_store$CompetitionOpenSinceYear)] = 2009

f_store$CompetitionOpenSince <- as.yearmon(paste(f_store$CompetitionOpenSinceYear, 
                                                 f_store$CompetitionOpenSinceMonth, sep = "-"))

f_train_store$CompetitionOpenSinceYear = as.yearmon(paste(f_train_store$CompetitionOpenSince)) # converting iin year format


#replicating above code in f_test_store

f_test_store$CompetitionDistance[is.na(f_test_store$CompetitionDistance)] = 5382
f_test_store$CompetitionOpenSinceMonth[is.na(f_test_store$CompetitionOpenSinceMonth)] = 4
f_test_store$CompetitionOpenSinceYear[is.na(f_test_store$CompetitionOpenSinceYear)] = 2009

f_test_store$CompetitionOpenSince <- as.yearmon(paste(f_test_store$CompetitionOpenSinceYear, 
                                                      f_test_store$CompetitionOpenSinceMonth, sep = "-"))

f_test_store$CompetitionOpenSinceYear = as.yearmon(paste(f_test_store$CompetitionOpenSince)) # converting iin year format


#Added How Many days since promo started column: promo2Days
#convert promo2Since to date format
f_train_store$Promo2Since = as.Date(f_train_store$Promo2Since)
f_test_store$Promo2Since = as.Date(f_test_store$Promo2Since)

# adding 1500 days in case no promo2
f_train_store$Promo2Since[is.na(f_train_store$Promo2Since)] = f_train_store$Date[is.na(f_train_store$Promo2Since)] +1500
sum(is.na(f_train_store$Promo2Since))
#doing it in test
f_test_store$Promo2Since[is.na(f_test_store$Promo2Since)] = f_test_store$Date[is.na(f_test_store$Promo2Since)] +1500
sum(is.na(f_test_store$Promo2Since))

#Added promo2Days column = diff of days bet current store date - promo started date 
# Negative value means how many days after that day the promo will start
f_train_store$promo2Days = f_train_store$Date - f_train_store$Promo2Since

f_train_store$Promo2SinceYear = format(f_train_store$Promo2Since, "%Y") ## extracting year from date
f_train_store$Promo2SinceYear = as.yearmon(paste(f_train_store$Promo2Since)) # converting iin year format


f_train_store$Promo2SinceWeek = format(f_train_store$Promo2Since, "%W") ## extracting week from date
f_train_store$Promo2SinceWeek = as.factor(f_train_store$Promo2SinceWeek) # converting weeks in factors

#str(f_train_store)


#replicating in test
f_test_store$promo2Days = f_test_store$Date - f_test_store$Promo2Since

f_test_store$Promo2SinceYear = format(f_test_store$Promo2Since, "%Y") ## extracting year from date
f_test_store$Promo2SinceYear = as.yearmon(paste(f_test_store$Promo2Since)) # converting iin year format

f_test_store$Promo2SinceWeek = format(f_test_store$Promo2Since, "%W") ## extracting week from date
f_test_store$Promo2SinceWeek = as.factor(f_test_store$Promo2SinceWeek) # converting weeks in factors


#Promo Interval - Each round started month; so we can add column of promo2 round Number x started and 
#watch effect / drop in sales after how many days

#############################################################3

f_train_store$STACD = "b"
f_train_store$STACD[f_train_store$StoreType == "a" | f_train_store$StoreType == "c" | f_train_store$StoreType == "d"] = "ACD"

f_train_store$STACD = as.factor(f_train_store$STACD)


f_test_store$STACD = "b"
f_test_store$STACD[f_test_store$StoreType == "a" | f_test_store$StoreType == "c" | f_test_store$StoreType == "d"] = "ACD"

f_test_store$STACD = as.factor(f_test_store$STACD)

#####################################################################

##Days to zeroSales

#for train
#f_train_store$dayTozeroSales = NULL
f_train_store$dayTozeroSales  = ifelse(f_train_store$Open==0,0,1)
#f_train_store$dayTozeroSales  = as.factor(f_train_store$dayTozeroSales )
#summary(f_train_store$dayTozeroSales )
f_train_store$dayTozeroSales  = as.numeric(f_train_store$dayTozeroSales )

f_train_store <- f_train_store[order(Store)]

for (i in 1:1000) {
  
  f_train_store$dayTozeroSales  = ifelse(f_train_store$Open==0,0,
                                         1 + shift(f_train_store$dayTozeroSales , 1L, type="lag"))
}

f_train_store$dayTozeroSales  = as.factor(f_train_store$dayTozeroSales )
summary(f_train_store$dayTozeroSales )

##########################################################


#f_test_store$dayTotzeroSales = NULL
f_test_store$dayTozeroSales  = ifelse(f_test_store$Open==0,0,1)
#f_test_store$dayTozeroSales  = as.factor(f_test_store$dayTozeroSales )
#summary(f_test_store$dayTozeroSales )

f_test_store$dayTozeroSales  = as.numeric(f_test_store$dayTozeroSales )

f_test_store <- f_test_store[order(Store)]

for (i in 1:100) {
  
  f_test_store$dayTozeroSales  = ifelse(f_test_store$Open==0,0,
                                        1 + shift(f_test_store$dayTozeroSales , 1L, type="lag"))
}

f_test_store$dayTozeroSales  = as.factor(f_test_store$dayTozeroSales )
f_test_store$dayTozeroSales [is.na(f_test_store$dayTozeroSales )] = 6
#str(f_test_store$dayTozeroSales )

######################################################################
# comp distance factors

f_train_store <- f_train_store[order(Date)]

f_train_store$factCD[f_train_store$CompetitionDistance <= 100] = 0
f_train_store$factCD[f_train_store$CompetitionDistance > 100 & f_train_store$CompetitionDistance <= 300] = 1
f_train_store$factCD[f_train_store$CompetitionDistance > 300 & f_train_store$CompetitionDistance <= 500] = 2
f_train_store$factCD[f_train_store$CompetitionDistance > 500 & f_train_store$CompetitionDistance <= 1000] = 3
f_train_store$factCD[f_train_store$CompetitionDistance > 1000 & f_train_store$CompetitionDistance <= 2000] = 4
f_train_store$factCD[f_train_store$CompetitionDistance > 2000 & f_train_store$CompetitionDistance <= 4000] = 5
f_train_store$factCD[f_train_store$CompetitionDistance > 4000 & f_train_store$CompetitionDistance <= 10000] = 6
f_train_store$factCD[f_train_store$CompetitionDistance > 10000 & f_train_store$CompetitionDistance <= 20000] = 7
f_train_store$factCD[f_train_store$CompetitionDistance > 20000 & f_train_store$CompetitionDistance <= 40000] = 8
f_train_store$factCD[f_train_store$CompetitionDistance > 40000] = 9

f_train_store$factCD = as.factor(f_train_store$factCD)

#summary(f_train_store$factCD)
f_test_store <- f_test_store[order(Date)]

f_test_store$factCD[f_test_store$CompetitionDistance <= 100] = 0
f_test_store$factCD[f_test_store$CompetitionDistance > 100 & f_test_store$CompetitionDistance <= 300] = 1
f_test_store$factCD[f_test_store$CompetitionDistance > 300 & f_test_store$CompetitionDistance <= 500] = 2
f_test_store$factCD[f_test_store$CompetitionDistance > 500 & f_test_store$CompetitionDistance <= 1000] = 3
f_test_store$factCD[f_test_store$CompetitionDistance > 1000 & f_test_store$CompetitionDistance <= 2000] = 4
f_test_store$factCD[f_test_store$CompetitionDistance > 2000 & f_test_store$CompetitionDistance <= 4000] = 5
f_test_store$factCD[f_test_store$CompetitionDistance > 4000 & f_test_store$CompetitionDistance <= 10000] = 6
f_test_store$factCD[f_test_store$CompetitionDistance > 10000 & f_test_store$CompetitionDistance <= 20000] = 7
f_test_store$factCD[f_test_store$CompetitionDistance > 20000 & f_test_store$CompetitionDistance <= 40000] = 8
f_test_store$factCD[f_test_store$CompetitionDistance > 40000] = 9

f_test_store$factCD = as.factor(f_test_store$factCD)

summary(f_test_store$factCD)

############################################################################################################

f_train_store <- f_train_store[order(Store)]

f_train_store$cgZero[f_train_store$Open==0] = 0

ind  = which(f_train_store$Open == 0)

ind = ind[-1]

f_train_store$cgZero[ind-1] = 1
f_train_store$cgZero[ind+1] = 2
f_train_store$cgZero[ind-2] = 3
f_train_store$cgZero[ind+2] = 4
f_train_store$cgZero[is.na(f_train_store$cgZero)] = 5
f_train_store$cgZero[2] = 2
f_train_store$cgZero[3] = 4

f_train_store$cgZero = as.factor(f_train_store$cgZero)


###########
##cgzero for test

f_test_store <- f_test_store[order(Store)]

f_test_store$cgZero[f_test_store$Open==0] = 0

ind  = which(f_test_store$Open == 0)

f_test_store$cgZero[ind-1] = 1
f_test_store$cgZero[ind+1] = 2
f_test_store$cgZero[ind-2] = 3
f_test_store$cgZero[ind+2] = 4
f_test_store$cgZero[is.na(f_test_store$cgZero)] = 5

f_test_store$cgZero = as.factor(f_test_store$cgZero)

######################################################################################3
temp_f_train = f_train_store
temp_f_test = f_test_store

temptD = f_train_store

temptD = temptD[temptD$Sales > 0, ]

xql = sqldf("SELECT Store, avg(Sales) as mean_SS, min(Sales) as min_SS, max(Sales) as max_SS FROM temptD GROUP BY Store")

xql$ft_mean_SS = as.factor(round((abs(xql$mean_SS - mean(xql$mean_SS))) / sd(xql$mean_SS), 0))
xql$ft_min_SS = as.factor(round((abs(xql$min_SS - mean(xql$min_SS))) / sd(xql$min_SS), 0))
xql$ft_max_SS = as.factor(round((abs(xql$max_SS - mean(xql$max_SS))) / sd(xql$max_SS), 0))

f_train_store = merge(f_train_store, xql, by = "Store")
f_test_store = merge(f_test_store, xql, by = "Store")

######################################################################################

f_train_store <- f_train_store[order(Date)]
f_test_store <- f_test_store[order(Date)]

##################################################################################

## for train
store$comOpenSince = as.POSIXct(paste(store$CompetitionOpenSinceYear, 
                                               store$CompetitionOpenSinceMonth, 1, sep = "-"),
                                         format = "%Y-%U-%u")
substore = subset(store, select = c(Store,comOpenSince))

f_train_store = merge(f_train_store, substore, by = "Store")

#f_train_store$Date = as.Date(f_train_store$Date,"%m/%d/%Y")

f_train_store$comOpenSince = as.Date(f_train_store$comOpenSince)

f_train_store$comOpenSince[is.na(f_train_store$comOpenSince)] = f_train_store$Date[is.na(f_train_store$comOpenSince)] - 1500
f_train_store$compSinceDays = f_train_store$Date - f_train_store$comOpenSince

sum(is.na(f_train_store$comOpenSince))
#####33 for test

f_test_store = merge(f_test_store, substore, by = "Store")

#f_train_store$Date = as.Date(f_train_store$Date,"%m/%d/%Y")

f_test_store$comOpenSince = as.Date(f_test_store$comOpenSince)

f_test_store$comOpenSince[is.na(f_test_store$comOpenSince)] = f_test_store$Date[is.na(f_test_store$comOpenSince)] - 1500
f_test_store$compSinceDays = f_test_store$Date - f_test_store$comOpenSince

str(f_test_store)

sum(is.na(f_test_store$comOpenSince))
##################################################################################################################
ftrain_store = f_train_store
ftest_store = f_test_store

 # save(ftrain_store,file="Ftrain_store.RDATA")
 # save(ftest_store,file="Ftest_store.RDATA")
 # 
 # load()
