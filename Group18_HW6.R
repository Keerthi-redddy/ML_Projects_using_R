library(tidyverse)
library(ggplot2)
library(forcats)
library(dplyr) 
library(Metrics)
library(caret)
library(MASS)


#reading data-------------
TrainData = read.csv("C:/Users/udayb/OneDrive/Desktop/Academics/Fall 23/IDA/Homework/HW6/Train.csv")
View(TrainData)
TestData = read.csv("C:/Users/udayb/OneDrive/Desktop/Academics/Fall 23/IDA/Homework/HW6/Test.csv")

#a-1)-----------------------------------------------------------------------------
TrainNumeric = TrainData %>% 
  select(where(is.numeric))


TrainFactor =  TrainData %>% 
  transmute(across(where(is.character), as.factor))

glimpse(TrainNumeric)
glimpse(TrainFactor)



#For Numeric Data of TrainData Dataframe-----------------------

#function to calculate 1st and 3rd Quartiles
Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}
Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

#function to calculate length n, unique, missing, mean, min, Q1, median, Q3, max and standardDeviation.
myNumericSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}


numericSummary = TrainNumeric %>%
  summarise_all(myNumericSummary)

#addidng Variable names row in dataframe
numericSummary <-cbind(
  stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"),
  numericSummary)

glimpse(numericSummary)

numericSummaryFinal <- numericSummary %>%
  pivot_longer("sessionId":"revenue", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*missing/n,
         unique_pct = 100*unique/n) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())

library(knitr)
options(digits=3)
options(scipen=99)
numericSummaryFinal %>% kable()



#For Non Numeric Data of TrainData Dataframe-----------------------

#code for identifying the first, second, or least common modes
getmodes <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}




#code for identifying the frequencies of the first, second, or least common modes
getmodesCnt <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl)) #1st mode freq
  }
  else if (type==2) {
    return (max(tbl[-m1])) #2nd mode freq
  }
  else if (type==-1) {
    return (min(tbl)) #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}


#code for  number of observations, the number of missing observations, the percent of missing, 
#the number of unique values, the percent of unique values, the frequency ratio
myNonNumericSummary <- function(x) {
  c(  length(x),
      sum(is.na(x)),
      (sum(is.na(x)) / length(x)) * 100,
      n_distinct(x),
      (n_distinct(x) / length(x)) * 100,
      
      round((sort(table(x),decreasing = TRUE))[1]/(sort(table(x),decreasing = TRUE))[2],2))}

#Calculating the number of observations, the number of missing observations, the percent of missing, 
#the number of unique values, the percent of unique values, the frequency ratio and storing it in data frame nonNumericSummary_1
nonNumericSummary_1 =  TrainFactor %>%
  summarise_all(myNonNumericSummary)

#Calculating the first mode, first mode frequency, second mode, second, mode frequency, 
#least common value, and least common frequency and storing it in data frame nonNumericSummary_2
nonNumericSummary_2 = TrainFactor %>%
  summarise_all( ~c(
    getmodes(.),
    getmodesCnt(.),
    getmodes(., 2),
    getmodesCnt(., 2),
    getmodes(., -1),
    getmodesCnt(., -1)))



#Concating both data frames nonNumericSummary_1, nonNumericSummary_2 into the final data frame nonNumericSummary
nonNumericSummary = rbind(nonNumericSummary_1, nonNumericSummary_2)

#adding an extra row which has the variable names
nonNumericSummary <-cbind(
  stat=c("n","missing","missing_pct","unique","unique_pct","freqRatio","1st mode","1st mode freq","2nd mode","2nd mode freq","least common","least common freq"),
  nonNumericSummary)



glimpse(nonNumericSummary)


nonNumericSummaryFinal <- nonNumericSummary %>%
  pivot_longer("date":"adwordsClickInfo.adNetworkType", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  select(variable, everything())

glimpse(nonNumericSummaryFinal)

#Plots-------------------------------------------------------------------------
#Plot for Missing value counts
Train_sum <- TrainData  %>%summarise_all(list(~is.na(.)))
Train_sum%>%
  pivot_longer(everything(),names_to = "Features", values_to="NA_values") %>%
  count(Features, NA_values) %>%ggplot(aes(y=Features,x=n,fill=NA_values))+
  geom_col()

#Plot for outliers of revenue, pageviews and visitNumber
outlierdetection <- TrainData[,c("revenue","pageviews","visitNumber")]
boxplot(outlierdetection)


#Plot for Continental Analysis of revwnue
ggplot(TrainData, aes(x = continent, y = revenue)) +
  geom_line() +
  labs(title = "Continent Analysis of Revenue")




# Calculating missing percentage of data from each variable for Train and Test data
p = function(x) {sum(is.na(x))/length(x)*100}
apply(TrainData, 2, p)

p = function(x) {sum(is.na(x))/length(x)*100}
apply(TestData, 2, p)
# removing some variables from the Test and Train data
TrainData=subset(TrainData,select=-c(adContent,adwordsClickInfo.page,
                             adwordsClickInfo.slot,adwordsClickInfo.gclId,
                             adwordsClickInfo.adNetworkType,
                             adwordsClickInfo.isVideoAd,region,referralPath,
                             deviceCategory,sessionId,networkDomain,metro,medium,date,
                             keyword,isTrueDirect,isMobile,topLevelDomain,continent,
                             city,campaign,bounces))
TestData=subset(TestData,select=-c(adContent,adwordsClickInfo.page,
                             adwordsClickInfo.slot,adwordsClickInfo.gclId,
                             adwordsClickInfo.adNetworkType,
                             adwordsClickInfo.isVideoAd,region,referralPath,
                             deviceCategory,sessionId,networkDomain,metro,medium,date,
                             keyword,isTrueDirect,isMobile,topLevelDomain,continent,
                             city,campaign,bounces))

columns_to_assign_na <- c("pageviews", "newVisits", "subContinent",
                          "operatingSystem","source","country","browser") 
# Specify the value you want to assign
assign_zero <- 0  # Replace with the Zero

# Loop through the selected columns and assign the value
for (col in columns_to_assign_na) {
  if (any(is.na(TrainData[[col]]))) {
    TrainData[[col]][is.na(TrainData[[col]])] <- assign_zero
  }
  if (any(is.na(TestData[[col]]))) {
    TestData[[col]][is.na(TestData[[col]])] <- assign_zero
  }
}
sum(is.na(TrainData))
sum(is.na(TestData))
# Define the columns and the number of levels to keep for each column
columns_to_lump <- c("channelGrouping", "country", "browser","subContinent",
                     "operatingSystem","source")
n_levels_to_keep <- c(5, 11, 9,10,6,7)

# Loop through the columns and apply fct_lump to both train and test data
for (i in 1:length(columns_to_lump)) {
  # View(fct_count(train$browser, sort = TRUE))
  column <- columns_to_lump[i]
  n <- n_levels_to_keep[i]
  
  TrainData[[column]] <- fct_lump(TrainData[[column]], n = n)
  TestData[[column]] <- fct_lump(TestData[[column]], n = n)
  
  # View(fct_count(train$browser, sort = TRUE))
}

#timeSinceLastVisit
TrainData$timeSinceLastVisit <- log(TrainData$timeSinceLastVisit+1)
TestData$timeSinceLastVisit <- log(TestData$timeSinceLastVisit+1)
View(fct_count(TrainData$browser, sort = TRUE))
#visitStartTime
TrainData$visitStartTime <- log(TrainData$visitStartTime+1)
TestData$visitStartTime <- log(TestData$visitStartTime+1)
sum(is.na(TrainData))
sum(is.na(TestData))

modef <- function(x){
  dd <- unique(x)
  dd[which.max(tabulate(match(x,dd)))]
}

Features <- function(dataset, includeRevenue = TRUE) {
  tuple_d <- dataset %>% group_by(custId) %>% dplyr::summarize(
    channelGrouping = modef(channelGrouping),
    
    mean_timeSinceLastVisit = mean(timeSinceLastVisit),
    range_timeSinceLastVisit = max(timeSinceLastVisit) - min(timeSinceLastVisit),
    sd_timeSinceLastVisit = sd(timeSinceLastVisit, na.rm = TRUE),
    median_timeSinceLastVisit = median(timeSinceLastVisit, na.rm = TRUE),
    
    browser = modef(browser),
    operatingSystem = modef(operatingSystem),
    subContinent = modef(subContinent),
    source = modef(source),
    
    pageviews_sum = sum(pageviews, na.rm = TRUE),
    pageviews_mean = mean(pageviews, na.rm = TRUE),
    pageviews_median = median(pageviews, na.rm = TRUE),
    pageviews_sd = sd(pageviews, na.rm = TRUE)
  )
  
  if (includeRevenue) {
    tuple_d <- dataset %>% group_by(custId) %>% dplyr::summarize(
      channelGrouping = modef(channelGrouping),
      
      mean_timeSinceLastVisit = mean(timeSinceLastVisit),
      range_timeSinceLastVisit = max(timeSinceLastVisit) - min(timeSinceLastVisit),
      sd_timeSinceLastVisit = sd(timeSinceLastVisit, na.rm = TRUE),
      median_timeSinceLastVisit = median(timeSinceLastVisit, na.rm = TRUE),
      
      browser = modef(browser),
      operatingSystem = modef(operatingSystem),
      subContinent = modef(subContinent),
      source = modef(source),
      
      pageviews_sum = sum(pageviews, na.rm = TRUE),
      pageviews_mean = mean(pageviews, na.rm = TRUE),
      pageviews_median = median(pageviews, na.rm = TRUE),
      pageviews_sd = sd(pageviews, na.rm = TRUE),
      totalRevenue = log(sum(revenue) + 1)
    )
  }
  
  return(tuple_d)
}

TrainData_new <- Features(TrainData,TRUE)
TestData_new <- Features(TestData,FALSE)
glimpse(TrainData_new)
w<-data.frame(custId=TestData_new$custId)

TrainData_new$sd_timeSinceLastVisit[is.na(TrainData_new$sd_timeSinceLastVisit)] <- assign_zero
TestData_new$sd_timeSinceLastVisit[is.na(TestData_new$sd_timeSinceLastVisit)] <- assign_zero

TrainData_new$pageviews_sd[is.na(TrainData_new$pageviews_sd)] <- assign_zero
TestData_new$pageviews_sd[is.na(TestData_new$pageviews_sd)] <- assign_zero

sum(is.na(TrainData_new))
sum(is.na(TestData_new))



##iii)


train_sum <- TrainData_new  %>%summarise_all(list(~is.na(.)))
train_sum%>%
  pivot_longer(everything(),names_to = "Features", values_to="NA_values") %>%
  count(Features, NA_values) %>%ggplot(aes(y=Features,x=n,fill=NA_values))+
  geom_col()

test_sum <- TestData_new  %>%summarise_all(list(~is.na(.)))
test_sum%>%
  pivot_longer(everything(),names_to = "Features", values_to="NA_values") %>%
  count(Features, NA_values) %>%ggplot(aes(y=Features,x=n,fill=NA_values))+
  geom_col()

test_new=subset(test_new,select=-c(custId))
train_new=subset(train_new,select=-c(custId))

lmfit <- lm(totalRevenue ~ ., data=train_new)
summary(lmfit)
AIC(lmfit)
lm_predict<- predict(lmfit,test_new)

rmse(train_new$totalRevenue,lm_predict)

train_new=subset(train_new,select=-c(channelGrouping,mean_timeSinceLastVisit,median_timeSinceLastVisit))
test_new=subset(test_new,select=-c(channelGrouping,mean_timeSinceLastVisit,median_timeSinceLastVisit))

train_new$browser[train_new$browser%in%c("Safari (in-app)","Chrome","Edge","Firefox",
                                         "Internet Explorer","Opera Mini","Other")]<-"Other"
test_new$browser[test_new$browser%in%c("Safari (in-app)","Chrome","Edge","Firefox",
                                       "Internet Explorer","Opera Mini","Other")]<-"Other"

train_new$browser[train_new$browser%in%c("Firefox")]<-"Other"
test_new$browser[test_new$browser%in%c("Firefox")]<-"Other"

train_new$operatingSystem[train_new$operatingSystem%in%c("Linux")]<-"Other"
test_new$operatingSystem[test_new$operatingSystem%in%c("Linux")]<-"Other"

sum(is.na(lm_predict))

cooksD <- cooks.distance(lmfit) 
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
names(influential)
ifl<- names(influential)
outliers <- train_new[ifl,]

#a dataframe without outliers
train_modify <- train_new %>% anti_join(outliers)

train_modify <- as.data.frame(train_modify)
View(train_modify)

lmfit1 <- lm(totalRevenue ~ ., data=train_modify)
summary(lmfit1)

predRevenue<-predict(lmfit1,test_new)
sum(is.na(predRevenue))

#-------------------------------------------------------------------------------------------------------
#PLS
set.seed(200)
train_modify_x=subset(train_modify,select=-c(totalRevenue))
train_new_x <- train_new %>% select(!totalRevenue)
control <- trainControl(method = "cv",number=10,p=0.9)
# set hyper-parameter tuning range
PLS_fit <- train(train_modify_x, train_modify$totalRevenue, method = "pls",
                 tuneGrid = expand.grid(.ncomp = 1:10),
                 trControl = control)
PLS_fit


#-------------------------------------------------------------------------------------------------------
#Lasso

Lasso_Data <- train_modify
View(Lasso_Data)
set.seed(200)
control <- trainControl(method = "cv",number=10,p=0.9)
tunegrid <- expand.grid(.fraction = seq(0, 1, by = 0.1))
lasso_fit <- train(totalRevenue ~ ., data=Lasso_Data,method="lasso",
                   preProcess=c("center","scale"),
                   trControl=control,
                   tuneGrid=tunegrid)
summary(lasso_fit)
LASSO_pred<-predict(lasso_fit,test_new)

#-------------------------------------------------------------------------------------------------------

#MARS
MARS <- train(data=train_modify ,totalRevenue~.,
              method="earth",
              trControl=trainControl(method = "cv",number = 5),
              tuneGrid=expand.grid(degree = 1:3, nprune=c(10,20,30)))

train_pred <- predict(MARS,test_new, nprune = MARS$bestTune$nprune ,
                      degree = MARS$bestTune$degree)

rmse(train_modify$totalRevenue,train_pred)


MARS <- train(data=train_new ,totalRevenue~.,
              method="earth",
              trControl=trainControl(method = "cv",number = 5),
              tuneGrid=expand.grid(degree = 1:3, nprune=c(10,20,30)))

MARStrainpred=subset(train_modify,select=-c(totalRevenue))

train_pred <- predict(MARS,test_new, nprune = MARS$bestTune$nprune ,
                      degree = MARS$bestTune$degree)

rmse(train_new$totalRevenue,train_pred)


#SVM
library(e1071)
# Define the parameter grid for tuning
param_grid <- expand.grid(
  cost = c(0.1, 1, 3),
  gamma = c(0.1, 1, 3)
  #  epsilon = c(0.1, 0.01, 0.002)
)

# Perform grid search with cross-validation
svm_tune <- tune(
  svm,
  totalRevenue ~ .,
  data = train_new,
  ranges = param_grid,
  kernel = "linear",
  scale = FALSE,  # Scaling is often recommended for SVM, but you can experiment
  tunecontrol = tune.control(sampling = "cross", cross = 5)
)

# Extract the best model
best_model <- svm_tune$best.model

# Make predictions on the test set
predictions <- predict(best_model, newdata = MARStrainpred)

rmse(train_new$totalRevenue,predictions)

write.csv(train_pred, "C:/Users/ASUS/Downloads/2023-isedsa-5103-ida-hw-6/train_pred_mars.csv", row.names = FALSE)
