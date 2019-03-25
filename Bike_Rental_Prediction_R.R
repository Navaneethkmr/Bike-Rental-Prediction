#Clean the environment
rm(list = ls())

#set the working directory
setwd("C:/Users/Navaneeth/Desktop/Edwisor/Project/Bike rental")


#load the required libraries
x = c("ggplot2","gridExtra","DMwR","caret","corrplot")
lapply(x, require, character.only = TRUE)
rm(x)


#read the data csv file by replacing space,comma & NA values with NA.
data=read.csv("day.csv",header=T,na.strings = c(""," ","NA"))

#*********************  Exploratory data Analysis  ***************************

#To have a look at the structure of the dataset
str(data)

#Convert the reqired variables to factor
data$season= as.factor(data$season)
data$yr= as.factor(data$yr)
data$mnth= as.factor(data$mnth)
data$holiday= as.factor(data$holiday)
data$weekday= as.factor(data$weekday)
data$workingday= as.factor(data$workingday)
data$weathersit= as.factor(data$weathersit)

#dropping the instant variable as it's just the row numbers
data$instant=NULL
#dropping the dteday variable as we have separate variables for workingdays and holidays
data$dteday=NULL


# get indexes of numerical variables
numeric_index = sapply(data,is.numeric) #selecting only numeric
# store the numeric data
numeric_data = data[,numeric_index]
# store the column names of numerical variables
cnames = colnames(numeric_data)

#plotting density plot for numeric variables
p1= ggplot(numeric_data, aes(x= temp)) + geom_density()
p2= ggplot(numeric_data, aes(x= atemp)) + geom_density()
p3= ggplot(numeric_data, aes(x= hum)) + geom_density()
p4= ggplot(numeric_data, aes(x= windspeed)) + geom_density()
p5= ggplot(numeric_data, aes(x= casual)) + geom_density()
p6= ggplot(numeric_data, aes(x= registered)) + geom_density()
p7= ggplot(numeric_data, aes(x= cnt)) + geom_density()

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6,p7, ncol = 2)


#***************************   Visualizations    ***********************

#Bike rental count with respect to season
ggplot(data, aes(x = season,y=cnt)) + geom_bar(stat = 'identity',fill='steelblue') + ggtitle("Bike rental count wrt season")


#Bike rental count with respect to workingday
ggplot(data, aes(x = workingday,y=cnt,fill=workingday)) + geom_boxplot() + ggtitle("Bike rental count wrt workingday")


#Bike rental count with respect to weekdays(Monday is taken as zero)
ggplot(data, aes(x = weekday,y=cnt)) + geom_bar(stat = 'identity',fill='steelblue') + ggtitle("Bike rental count wrt weekdays")


#Bike rental count with respect to holiday
ggplot(data, aes(x = holiday,y=cnt,fill=holiday)) + geom_boxplot() + ggtitle("Bike rental count wrt holidays")

#Bike rental count with respect to months
g1=ggplot(data, aes(x = mnth,y=cnt)) + geom_bar(stat = 'identity',fill='steelblue') + ggtitle("Bike rental count wrt months")

#Bike rental count with respect to year
g2=ggplot(data, aes(x = yr,y=cnt)) + geom_bar(stat = 'identity',fill='steelblue') + ggtitle("Bike rental count wrt year")
gridExtra::grid.arrange(g1,g2, ncol = 2)

#Bike rental count with respect to year & months
ggplot(data,aes(x=yr,y=cnt,fill=mnth))+geom_boxplot() + ggtitle("Bike rental count wrt year & months")


#Bike rental count with respect to weather situation
ggplot(data,aes(x=weathersit,y=cnt,fill=weathersit))+geom_boxplot() + ggtitle("Bike rental count wrt weather situation")

#Bike rental count with respect to temperature
g3=ggplot(data,aes(x=temp,y=cnt))+geom_point()+geom_smooth(method='lm',se=F)+ggtitle("Bike rental count wrt temp")
#Bike rental count with respect to temperature & weather situation
g4=ggplot(data,aes(x=temp,y=cnt,col=weathersit))+geom_point()+geom_smooth(method='lm',se=F)+ggtitle("Bike rental count wrt temp & weather condition")
gridExtra::grid.arrange(g3,g4, ncol = 2)

#Bike rental count with respect to adjusted temperature
ggplot(data,aes(x=atemp,y=cnt))+geom_point()+geom_smooth(method='lm',se=F)+ggtitle("Bike rental count wrt atemp")

#Bike rental count with respect to humidity
g5=ggplot(data,aes(x=hum,y=cnt))+geom_point()+geom_smooth(method='lm',se=F)+ggtitle("Bike rental count wrt humidity")
#Bike rental count with respect to humidity & weather situation
g6=ggplot(data,aes(x=hum,y=cnt,col=weathersit))+geom_point()+geom_smooth(method='lm',se=F)+ggtitle("Bike rental count wrt humidity & weather condition")
gridExtra::grid.arrange(g5,g6, ncol = 2)

#Bike rental count with respect to windspeed
g7=ggplot(data,aes(x=windspeed,y=cnt))+geom_point()+geom_smooth(method='lm',se=F)+ggtitle("Bike rental count wrt windspeed")
#Bike rental count with respect to windspeed & weather situation
g8=ggplot(data,aes(x=windspeed,y=cnt,col=weathersit))+geom_point()+geom_smooth(method='lm',se=F)+ggtitle("Bike rental count wrt windspeed & weather condition")
gridExtra::grid.arrange(g7,g8, ncol = 2)



#***********************   Missing Value Analysis   ********************************

#calcualte the sum of NAs in each colum and store it as a dataframe in missing_val
missing_val = data.frame(apply(data,2,function(x) {sum(is.na(x))}))
#Storing the row names
missing_val$Columns = row.names(missing_val)
# rename the 1st column name
names(missing_val)[1] =  "Missing_percentage"
# calculate the percentage of NAs
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(data)) * 100
# get percentage of NAs in decreasing order
missing_val = missing_val[order(-missing_val$Missing_percentage),] 
#drop the row names
row.names(missing_val) = NULL
#re-arrange the columns
missing_val = missing_val[,c(2,1)]


#************************     Outlier Analysis   ********************************


# get indexes of numerical variables
numeric_index = sapply(data,is.numeric) #selecting only numeric
# store the numeric data
numeric_data = data[,numeric_index]
#dropping the target variable
numeric_data$cnt=NULL
# store the column names of numerical variables
cnames = colnames(numeric_data)

# loop for plotting the box plot for all the numerical variables
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of bike rental count for",cnames[i])))
}

# Plotting the boxplots together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,ncol=2)

#*********************    Outlier Treatment    ******************

#We are treating outliers on hum and windspeed
num=c('hum','windspeed')

# Replace Outlier values with NAs
for (i in num)
{
  outlier_values=data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  data[,i][data[,i] %in% outlier_values]= NA
}


# Impute NAs with KNN Imputation
data= knnImputation(data,k=3)

#***********************    Feature Selection   ***************************

# Correlation Plot 
numeric_data = data[,numeric_index]
cnames = colnames(numeric_data)
corr= cor(data[,cnames])
corrplot(corr, method = "color",outline = TRUE,addCoef.col = "black")

#From the above corrplot we will remove atemp as it is positively correlated with temp.
# Also, the variables 'casual' & 'registered' will be dropped as we have sum of both these variables as 'cnt'


#Chi square test to check the dependency/correlation between the categorical variables
factor_index=sapply(data,is.factor) 
factor_data = data[,factor_index]
cat_names = colnames(factor_data)

for (i in 1:length(cat_names))
{
  for (j in 2:length(cat_names))
  {
    print(names(factor_data)[i])
    print(names(factor_data)[j])
    print(chisq.test(table(factor_data[,i], factor_data[,j])))
  }
}

#Finding relationship between categorical variable and target variable(Continuous) using ANOVA
#For loop for ANOVA
for(i in cat_names)
{
  assign(paste0("anova_", i), aov(data$cnt ~ factor_data[,i]))
}

summary(anova_season)# season is statistically significant to explain the target variable
summary(anova_yr) #yr is statistically significant to explain the target variable
summary(anova_mnth) # mnth is statistically significant to explain the target variable
summary(anova_holiday) #holiday is statistically significant to explain the target variable
summary(anova_weekday) #weekday is not statistically significant to explain the target variable
summary(anova_workingday) #workignday is statistically significant to explain the target variable
summary(anova_weathersit) #weathersit is statistically significant to explain the target variable


# Dimension Reduction 

data= subset(data,select = -c(atemp,casual,registered,weekday))

#**********************   Train-Test Split  *****************

#Split 70% of the data as train and 30% as test
set.seed(1234)
train.index = createDataPartition(data$cnt, p = .70, list = FALSE)
train = data[ train.index,]
test  = data[-train.index,]

#Function for mape 
mape = function(act,pre)
{
  mean(abs((act-pre)/act))*100
}

#Function for RMSE calculation
rmse = function(pred,act) 
{
  difference = (pred - act)
  root_mean_square_error = sqrt(mean(difference^2))
  return(root_mean_square_error)
}

#*********************   Decision Tree model using 10 fold cross validation   *************

#set the traincontrol parameters 
control = trainControl(method="cv",number = 10,search = "random")
#Train the decision tree model
set.seed(12)
DT_model= train(cnt ~., data=train,method="rpart",trControl=control,tuneLength=10)
#Print the CV results
print(DT_model)
#Predict on the test data
predictions= predict(DT_model,test[,-10])

#Calculate MAPE
mape(test[,10],predictions)
#Calculate RMSE
rmse(predictions,test[,10])

#Save the DT Model 
saveRDS(DT_model,"DecisionTree_Rmodel.rds") 


# MAPE = 23.06735
# RMSE = 885.047


#****************    Random Forest model using 10 fold cross validation   **********

#set the traincontrol parameters 
control = trainControl(method="cv",number = 10,search = "random")
#Train the random forest model
set.seed(111)
rf_model= train(cnt ~., data=train,method="rf",trControl=control,tuneLength=10)
#Print the CV results
print(rf_model)
#Predict on the test data
rf_predictions= predict(rf_model,test[,-10])

#Calculate MAPE
mape(test[,10],rf_predictions)  #18.23251
#Calculate RMSE
rmse(rf_predictions,test[,10])  

#Save the RF Model 
saveRDS(rf_model,"rf_Rmodel.rds")


# MAPE = 18.23251
# RMSE = 684.0097

#***************    Linear regression using 10 fold cross validation   ****************

#set the traincontrol parameters
control = trainControl(method="cv",number = 10,search = "random")
#Train the LM model
set.seed(112)
lm_model= train(cnt ~., data=train,method="lm",trControl=control,tuneLength=5)
#Print the CV results
print(lm_model)
#Predict on the test data
lm_predictions= predict(lm_model,test[,-10])

#Calculate MAPE
mape(test[,10],lm_predictions)
##Calculate RMSE
rmse(lm_predictions,test[,10])

#Save the RF Model 
saveRDS(lm_model,"lm_Rmodel.rds")


# MAPE = 16.19918
# RMSE = 727.8427


#*********************    END OF THE FILE        *********************#
