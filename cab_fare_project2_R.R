
rm(list = ls())
setwd("C:/Users/NANINE/Desktop/EdWisor/PROJECT2")
# #loading Libraries
x = c("ggplot2", "corrgram", "DMwR", "usdm", "caret", "randomForest", "e1071",
      "DataCombine", "doSNOW", "inTrees", "rpart.plot", "rpart",'MASS','xgboost','stats')
#load Packages
lapply(x, require, character.only = TRUE)
rm(x)


# loading datasets
cab_train = read.csv("train_cab.csv", header = T, na.strings = c(" ", "", "NA"))
cab_test = read.csv("test.csv")
cab_test_pickup_datetime = cab_test["pickup_datetime"]
# Structure of data
str(cab_train)
str(cab_test)
summary(cab_train)
summary(cab_test)
head(cab_train,5)
head(cab_test,5)

# Exploratory Data Analysis                    
# Changing the data types of variables
cab_train$fare_amount = as.numeric(as.character(cab_train$fare_amount))
cab_train$passenger_count=round(cab_train$passenger_count)

#OUTLIER ANALYSIS
# Removing values which are not within desired range(outlier) depending upon basic understanding of dataset.

# 1.Fare amount has a negative value, which doesn't make sense. A price amount cannot be -ve and also cannot be 0. So we will remove these fields.
cab_train[which(cab_train$fare_amount < 1 ),]
nrow(cab_train[which(cab_train$fare_amount < 1 ),])
cab_train = cab_train[-which(cab_train$fare_amount < 1 ),]

#2.Passenger_count variable
for (i in seq(4,11,by=1))
  {
  print(paste('passenger_count above ' ,i,nrow(cab_train[which(cab_train$passenger_count > i ),])))
  }
# so 20 observations of passenger_count is consistenly above from 6,7,8,9,10 passenger_counts, let's check them.
cab_train[which(cab_train$passenger_count > 6 ),]
# Also we need to see if there are any passenger_count==0
cab_train[which(cab_train$passenger_count <1 ),]
nrow(cab_train[which(cab_train$passenger_count <1 ),])
nrow(cab_train[which(cab_train$passenger_count >6 ),])
# We will remove these 58 observations and 20 observation which are above 6 value because a cab cannot hold these number of passengers.
cab_train = cab_train[-which(cab_train$passenger_count < 1 ),]
cab_train = cab_train[-which(cab_train$passenger_count > 6),]
# 3.Latitudes range from -90 to 90.Longitudes range from -180 to 180.Removing which does not satisfy these ranges
print(paste('pickup_longitude above 180=',nrow(cab_train[which(cab_train$pickup_longitude >180 ),])))
print(paste('pickup_longitude above -180=',nrow(cab_train[which(cab_train$pickup_longitude < -180 ),])))
print(paste('pickup_latitude above 90=',nrow(cab_train[which(cab_train$pickup_latitude > 90 ),])))
print(paste('pickup_latitude above -90=',nrow(cab_train[which(cab_train$pickup_latitude < -90 ),])))
print(paste('dropoff_longitude above 180=',nrow(cab_train[which(cab_train$dropoff_longitude > 180 ),])))
print(paste('dropoff_longitude above -180=',nrow(cab_train[which(cab_train$dropoff_longitude < -180 ),])))
print(paste('dropoff_latitude above -90=',nrow(cab_train[which(cab_train$dropoff_latitude < -90 ),])))
print(paste('dropoff_latitude above 90=',nrow(cab_train[which(cab_train$dropoff_latitude > 90 ),])))
# There's only one outlier which is in variable pickup_latitude.So we will remove it with nan.
# Also we will see if there are any values equal to 0.
nrow(cab_train[which(cab_train$pickup_longitude == 0 ),])
nrow(cab_train[which(cab_train$pickup_latitude == 0 ),])
nrow(cab_train[which(cab_train$dropoff_longitude == 0 ),])
nrow(cab_train[which(cab_train$pickup_latitude == 0 ),])
# there are values which are equal to 0. we will remove them.
cab_train = cab_train[-which(cab_train$pickup_latitude > 90),]
cab_train = cab_train[-which(cab_train$pickup_longitude == 0),]
cab_train = cab_train[-which(cab_train$dropoff_longitude == 0),]


#MISSING VALUE ANALYSIS                  
missing_val = data.frame(apply(cab_train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(cab_train)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val

unique(cab_train$passenger_count)
unique(cab_test$passenger_count)
cab_train[,'passenger_count'] = factor(cab_train[,'passenger_count'], labels=(1:6))
cab_test[,'passenger_count'] = factor(cab_test[,'passenger_count'], labels=(1:6))


# 1.For Passenger_count:

cab_train$passenger_count[1000]
cab_train$passenger_count[1000] = NA
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Mode Method
getmode(cab_train$passenger_count)
# We can't use mode method because data will be more biased towards passenger_count=1

# 2.For fare_amount:
cab_train$fare_amount[1000]
cab_train$fare_amount[1000]= NA

# Mean Method
mean(cab_train$fare_amount, na.rm = T)

#Median Method
median(cab_train$fare_amount, na.rm = T)

sum(is.na(cab_train))
str(cab_train)
summary(cab_train)
summary(cab_test)


#Outlier Analysis

# We Will do Outlier Analysis only on Fare_amount just for now and we will do outlier analysis after feature engineering laitudes and longitudes.
# Boxplot for fare_amount
pl1 = ggplot(cab_train,aes(x = factor(passenger_count),y = fare_amount))
pl1 + geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100)


# Replace all outliers with NA and impute
vals = cab_train[,"fare_amount"] %in% boxplot.stats(cab_train[,"fare_amount"])$out
cab_train[which(vals),"fare_amount"] = NA

#lets check the NA's
sum(is.na(cab_train$fare_amount))

cab_train$fare_amount[is.na(cab_train$fare_amount)] = median(cab_train$fare_amount, na.rm = T)
sum(is.na(cab_train$fare_amount))
sum(is.na(cab_train$passenger_count))
cab_train$passenger_count[is.na(cab_train$passenger_count)] = 2
sum(is.na(cab_train$passenger_count))
sum(is.na(cab_train))
sum(is.na(cab_test))
#feature engineering
# 2.Calculate the distance travelled using longitude and latitude
deg_to_rad = function(deg){
  (deg * pi) / 180
}
haversine = function(long1,lat1,long2,lat2){
  #long1rad = deg_to_rad(long1)
  phi1 = deg_to_rad(lat1)
  #long2rad = deg_to_rad(long2)
  phi2 = deg_to_rad(lat2)
  delphi = deg_to_rad(lat2 - lat1)
  dellamda = deg_to_rad(long2 - long1)
  
  a = sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * 
    sin(dellamda/2) * sin(dellamda/2)
  
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  R = 6371e3
  R * c / 1000 #1000 is used to convert to meters
}
# Using haversine formula to calculate distance fr both train and test
cab_train$dist = haversine(cab_train$pickup_longitude,cab_train$pickup_latitude,cab_train$dropoff_longitude,cab_train$dropoff_latitude)
cab_test$dist = haversine(cab_test$pickup_longitude,cab_test$pickup_latitude,cab_test$dropoff_longitude,cab_test$dropoff_latitude)

View(cab_train)
# We will remove the variables which were used to feature engineer new variables
cab_train = subset(cab_train,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
cab_test = subset(cab_test,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
View(cab_train)
View(cab_test)
sum(is.na(cab_train$pickup_datetime))

# 1.Feature Engineering for timestamp variable
# we will derive new features from pickup_datetime variable
# new features will be year,month,day_of_week,hour
#Convert pickup_datetime from factor to date time
cab_train$pickup_date = as.Date(as.character(cab_train$pickup_datetime))
cab_train$pickup_weekday = as.factor(format(cab_train$pickup_date,"%u"))# Monday = 1
cab_train$pickup_mnth = as.factor(format(cab_train$pickup_date,"%m"))
cab_train$pickup_yr = as.factor(format(cab_train$pickup_date,"%Y"))

pickup_time = strptime(cab_train$pickup_datetime,"%Y-%m-%d %H:%M:%S")
cab_train$pickup_hour = as.factor(format(pickup_time,"%H"))
cab_train = subset(cab_train,select = -c(pickup_datetime,pickup_date,pickup_yr))
View(cab_train)

#Add same features to test set
cab_test$pickup_date = as.Date(as.character(cab_test$pickup_datetime))
cab_test$pickup_weekday = as.factor(format(cab_test$pickup_date,"%u"))# Monday = 1
cab_test$pickup_mnth = as.factor(format(cab_test$pickup_date,"%m"))
cab_test$pickup_yr = as.factor(format(cab_test$pickup_date,"%Y"))

pickup_time = strptime(cab_test$pickup_datetime,"%Y-%m-%d %H:%M:%S")
cab_test$pickup_hour = as.factor(format(pickup_time,"%H"))
cab_test = subset(cab_test,select = -c(pickup_datetime,pickup_date,pickup_yr))
View(cab_test)
str(cab_test)
str(cab_train) 
#Feature selection                
numeric_index = sapply(cab_train,is.numeric) #selecting only numeric
numeric_data = cab_train[,numeric_index]
cnames = colnames(numeric_data)
#Correlation analysis for numeric variables
corrgram(cab_train[,numeric_index],upper.panel=panel.pie, main = "Correlation Plot")

# Feature Scaling   
 #Normalisation
 cab_train[,'dist'] = (cab_train[,'dist'] - min(cab_train[,'dist']))/
    (max(cab_train[,'dist'] - min(cab_train[,'dist'])))
 print('dist')
View(cab_train) 

cab_test[,'dist'] = (cab_test[,'dist'] - min(cab_test[,'dist']))/
   (max(cab_test[,'dist'] - min(cab_test[,'dist'])))
View(cab_test) 

sum(is.na(cab_train))
cab_train = na.omit(cab_train)
sum(is.na(cab_train))


sum(is.na(cab_test))

#converting multilevel categorical variable into binary dummy variable
cnames= c("pickup_mnth","pickup_weekday","passenger_count","pickup_hour")
cab_train_new=cab_train[,cnames]
fare_amount=data.frame(cab_train$fare_amount)
names(fare_amount)[1]="fare_amount"
cab_train_new <- fastDummies::dummy_cols(cab_train_new)
cab_train_new= subset(cab_train_new,select = -c(pickup_mnth,pickup_weekday,passenger_count,pickup_hour))
d3 = cbind(cab_train_new,cab_train)
d3= subset(d3,select = -c(pickup_mnth,pickup_weekday,passenger_count,pickup_hour,fare_amount))
cab_train_new=cbind(d3,fare_amount)
View(cab_train_new)
str(cab_train_new)

cnames= c("pickup_mnth","pickup_weekday","passenger_count","pickup_hour")
cab_test_new=cab_test[,cnames]
cab_test_new <- fastDummies::dummy_cols(cab_test_new)
cab_test_new= subset(cab_test_new,select = -c(pickup_mnth,pickup_weekday,passenger_count,pickup_hour))
cab_test_new = cbind(cab_test_new,cab_test)
cab_test_new= subset(cab_test_new,select = -c(pickup_mnth,pickup_weekday,passenger_count,pickup_hour))
View(cab_test_new)
str(cab_test_new)

#divide data into test and train
train_index = sample(1:nrow(cab_train_new), 0.7 * nrow(cab_train_new))
train = cab_train_new[train_index,]
test = cab_train_new[-train_index,]
str(train)

#Linear regression model making
lm_model = lm(fare_amount ~., data = train)
pred_LR = predict(lm_model,test[,-51])

###########Decision tree regression  #################
fit = rpart(fare_amount ~ ., data = train, method = "anova")
pred_DT = predict(fit, test[,-51])

#############Random Forest Model##########################
RF_model = randomForest(fare_amount ~ ., train, importance = TRUE, ntree = 200)
pred_RF = predict(RF_model, test[,-51])
plot(RF_model)
#evaluating MApe value
MAPE = function(y, yhat){
   mean(abs((y - yhat)/y))*100
}
MAPE(test[,51],  pred_LR)
MAPE(test[,51],  pred_DT)
MAPE(test[,51],  pred_RF)

str(cab_test_new)
str(cab_train_new)


#final model
#Random Forest Model
RF_model_out = randomForest(fare_amount ~ ., cab_train_new, importance = TRUE, ntree = 200)

# Saving the trained model
saveRDS(RF_model_out, "./final_RF_model_using_R.rds")

# loading the saved model
final_model <- readRDS("./final_RF_model_using_R.rds")
print(final_model)

# Lets now predict on test dataset
pred_RF_out= predict(RF_model_out,cab_test_new)
RF_pred = data.frame(cab_test_pickup_datetime,"predictions" = pred_RF_out)

# Now lets write(save) the predicted fare_amount in disk as .csv format 
write.csv(RF_pred,"RF_predictions_R.csv",row.names = FALSE)






