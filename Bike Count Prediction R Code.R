#Clean the environment
rm(list = ls())

#Set working directory
setwd("C:/Users/sai/Documents/New folder R")
getwd()

#Load the librarires
libraries = c("plyr","dplyr", "ggplot2","rpart","DMwR","randomForest","usdm","corrgram","DataCombine")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)


#Read the csv file
df_day = read.csv(file = "day.csv", header = T, sep = ",", na.strings = c(" ", "", "NA"))


########################################EXPLORE THE DATA########################################
#First few rows
head(df_day)

#Dimensions of data
dim(df_day)

#Column names
names(df_day)

#Structure of variables
str(df_day)

##Droping variables which are not requried.

#instant - index number
#dteday- all the requried like month week day all ready present
#droping casual and registered because there sum is equal to target variable ie. 'cnt'

df_day <- subset(df_day, select = -c(instant,dteday,casual,registered))

##################################################### Univariate Analysis############################################################
#Create columns
df_day$actual_temp <- df_day$temp*39
df_day$actual_feel_temp <- df_day$atemp*50
df_day$actual_windspeed <- df_day$windspeed*67
df_day$actual_hum = df_day$hum * 100

df_day$actual_season = factor(x = df_day$season, levels = c(1,2,3,4), labels = c("Spring","Summer","Fall","Winter"))
df_day$actual_yr = factor(x = df_day$yr, levels = c(0,1), labels = c("2011","2012"))
df_day$actual_holiday = factor(x = df_day$holiday, levels = c(0,1), labels = c("Working day","Holiday"))
df_day$actual_weathersit = factor(x = df_day$weathersit, levels = c(1,2,3,4), 
                                  labels = c("Clear","Cloudy/Mist","Rain/Snow/Fog","Heavy Rain/Snow/Fog"))

df_day$weathersit = as.factor(df_day$weathersit)
df_day$season = as.factor(df_day$season)
df_day$mnth = as.factor(df_day$mnth)
df_day$weekday = as.factor(as.character(df_day$weekday))
df_day$workingday = as.factor(as.character(df_day$workingday))
df_day$yr = as.factor(df_day$yr)
df_day$holiday = as.factor(df_day$holiday)


# function to create univariate distribution of numeric  variables
univariate_numeric <- function(num_x) {
  
  
  ggplot(df_day)+
    geom_histogram(aes(x=num_x,y=..density..),
                   fill= "grey")+
    geom_density(aes(x=num_x,y=..density..))
  
}


# analyze the distribution of  target variable 'cnt'and other numeric independent variables
univariate_numeric(df_day$cnt)
univariate_numeric(df_day$temp)
univariate_numeric(df_day$atemp)
univariate_numeric(df_day$hum)
univariate_numeric(df_day$windspeed)

#Check the distribution of categorical Data using bar graph
bar1 = ggplot(data = df_day, aes(x = actual_season)) + geom_bar() + ggtitle("Count of Season")
bar2 = ggplot(data = df_day, aes(x = actual_weathersit)) + geom_bar() + ggtitle("Count of Weather")
bar3 = ggplot(data = df_day, aes(x = actual_holiday)) + geom_bar() + ggtitle("Count of Holiday")
bar4 = ggplot(data = df_day, aes(x = workingday)) + geom_bar() + ggtitle("Count of Working day")
# ## Plotting plots together
gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)

######################################### Bivariate  Relationship #####################################################

#relation between Numerical Variables and target variable 'cnt' using scatter plot
scat1 = ggplot(data = df_day, aes(x =actual_temp, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike COunt")
scat2 = ggplot(data = df_day, aes(x =actual_hum, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point(color="red") + xlab("Humidity") + ylab("Bike COunt")
scat3 = ggplot(data = df_day, aes(x =actual_feel_temp, y = cnt)) + ggtitle("Distribution of Feel Temperature") + geom_point() + xlab("Feel Temperature") + ylab("Bike COunt")
scat4 = ggplot(data = df_day, aes(x =actual_windspeed, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point(color="red") + xlab("Windspeed") + ylab("Bike COunt")
gridExtra::grid.arrange(scat1,scat2,scat3,scat4,ncol=2)

#Box plot for Categorical Variables with 'CNT' to know the releation

cnames = colnames(df_day[,c("actual_season","mnth","actual_yr","actual_holiday","actual_weathersit","weekday","workingday")])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(x = cnames[i],y = 'cnt'), data = df_day)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(x=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}

gridExtra::grid.arrange(gn1,ncol=1)
gridExtra::grid.arrange(gn1,gn3,gn2,gn4,gn5,gn6,gn7,ncol=4)


########################################MISSING VALUE Analysis########################################
missing_values = sapply(df_day, function(x){sum(is.na(x))})

#no Missing Values found in the data


######################################### Outlier Analysis ####################################
#Check for outliers in data using boxplot
cnames = colnames(df_day[,c("temp","atemp","windspeed","hum")])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = df_day)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn3,gn2,gn4,ncol=2)

cor(df_day$windspeed,df_day$cnt)
cor(df_day$hum,df_day$cnt)
#Remove outliers in Windspeed
val = df_day$windspeed[df_day$windspeed %in% boxplot.stats(df_day$windspeed)$out]
df_day = df_day[which(!df_day$windspeed %in% val),]

#Remove outliers in hum
val = df_day$hum[df_day$hum %in% boxplot.stats(df_day$hum)$out]
df_day = df_day[which(!df_day$hum %in% val),]

# Boxplot after removing  outliers
cnames = colnames(df_day[,c("windspeed","hum")])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = df_day)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn2,ncol=2)

cor(df_day$windspeed,df_day$cnt)
cor(df_day$hum,df_day$cnt)

#######################################  Feature  Scaling ######################################

# In dataset  numeric  variables like 'temp' , 'atem' ,'hum' and ' windspeed' are in normalization form 
#no Need of Feature scaling

############################################# feature  selection   #######################################

#Check for multicollinearity using VIF

df_VIF = df_day[,c("temp","atemp","hum","windspeed")]
vifcor(df_VIF)

#Check for collinearity using corelation graph
corrgram(day, order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Remove the unwanted variables

df_day <- subset(df_day, select = -c(holiday,atemp,weekday,workingday,actual_temp,actual_feel_temp,actual_windspeed,
                                     actual_hum,actual_season,actual_yr,actual_holiday,actual_weathersit))

rmExcept(keepers = "df_day")

########################################DECISION TREE########################################
#MAPE: 19.42%
#MAE: 782
#RMSE: 953.8
#Accuracy: 81.58%

#Divide the data into train and test
set.seed(12)
train_index = sample(1:nrow(df_day), 0.8 * nrow(df_day))
train = df_day[train_index,]
test = df_day[-train_index,]

#rpart for regression
dt_model = rpart(cnt ~ ., data = train, method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model, test[,-8])

#Create dataframe for actual and predicted values
df = data.frame("actual"=test[,8], "pred"=dt_predictions)
head(df)
print(dt_model)
#  plotting decision tree
par(cex= 0.8)
plot(dt_model)
text(dt_model)
#calculate MAPE
regr.eval(trues = test[,8], preds = dt_predictions, stats = c("mae","mse","rmse","mape"))

#calculate MAPE
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}

MAPE(test[,8], dt_predictions)


########################################RANDOM FOREST########################################
#MAPE: 15.68%
#MAE: 437
#RMSE: 724
#Accuracy: 84.32%

#Train the data using random forest
rf_model = randomForest(cnt~., data = train, ntree = 500 ,nodesize =8,importance=TRUE)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-8])

#Create dataframe for actual and predicted values
df = cbind(df,rf_predictions)
head(df)

#Calculate MAPE
regr.eval(trues = test[,8], preds = rf_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,8], rf_predictions)

#variable importance  plot

varimp <- importance(rf_model)
# sort variable  
sort_var <- names(sort(varimp[,1],decreasing =T))
sort_var
varImpPlot(rf_model,type = 2)

########################################LINEAR REGRESSION########################################
#MAPE: 17.12%
#RMSE: 844
#Accuracy: 82.88%
#MAE: 647
#Adjusted R squared: 0.8483
#F-statistic: 183.2

#Train the data using linear regression
lr_model = lm(formula = cnt~., data = train)

#Check the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model, test[,-8])

#Create dataframe for actual and predicted values
df = cbind(df,lr_predictions)
head(df)

#Calculate MAPE
regr.eval(trues = test[,8], preds = lr_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,8], lr_predictions)

# Random forest is performing better than linear regression.

# Model input and output 
write.csv(test_set, file = "InputtestR.csv")
write.csv(rf_predictions, file="outputR.csv")