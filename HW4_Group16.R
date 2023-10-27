install.packages("tidyverse")
install.packages("car")
install.packages("caret")
install.packages("EnvStats")

install.packages('MASS')
install.packages('mice')
install.packages('forcats')


library('e1071')
library(tidyverse)
library(knitr)
library(MASS)
library(car)
library(EnvStats)
library('forcats')


# Unload packages that depend on 'MASS'
detach("package:jomo", unload = TRUE)
detach("package:lme4", unload = TRUE)
detach("package:ipred", unload = TRUE)

# Now, you can try unloading 'MASS'
detach("package:MASS", unload = TRUE)



# problem 1. a
housingData = read.csv("./keerthi/housingData.csv")
library(tidyverse)
housingData <- housingData %>%
  dplyr::mutate(age = YrSold - YearBuilt,
                ageSinceRemodel = YrSold - YearRemodAdd,
                ageofGarage = YrSold - GarageYrBlt)
# problem 1.b
housingNumeric <- housingData %>%
  dplyr::select(where(is.numeric))
# problem 1.c
housingFactor<-housingData %>%
  dplyr::select(where(is.character)) %>% transmute_all(as.factor)
#problem 1.d
glimpse(housingNumeric)
glimpse(housingFactor)
#problem 1.e
Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}

Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

#problem 1.f

myNumericSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}

#problem 1.g
numericSummary<-housingNumeric %>% dplyr::summarize_all(myNumericSummary)

lifecycle::last_lifecycle_warnings()

#problem 1.h

numericSummary <-cbind(
  stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"),
  numericSummary)
#glimpse(numericSummary)

#problem 1.i

numericSummaryFinal <- numericSummary %>%
  pivot_longer("Id":"ageofGarage", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*missing/n,
         unique_pct = 100*unique/n) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())

options(digits=3)
options(scipen=99)
numericSummaryFinal %>% kable()


#problem 1.j
getmodes <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1))
  }
  else if (type==2) {
    #1st mode
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl)))
  }
  else {
    stop("Invalid type selected")
  }
}
#least common mode
getmodesCnt <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl))
  }
  else if (type==2) {
    return (max(tbl[-m1]))
  }
  else if (type==-1) {
    return (min(tbl))
  }
  else {
    #1st mode freq
    #2nd mode freq
    #least common freq
    stop("Invalid type selected")
  }
}
freq_ratio<-function(x,a,b){
  freq=round(getmodesCnt(x,type = a)/getmodesCnt(x,type = b),2)
  return(freq)
}
myFactorSummary <- function(x){
  c(length(x), sum(is.na(x)),n_distinct(x),freq_ratio(x,1,2), getmodes(x,type = 1),
    getmodesCnt(x,type = 1), getmodes(x,type = 2), getmodesCnt(x,type = 2), getmodes(x,type = -1),
    getmodesCnt(x,type = -1) )
}
FactorSummary<-housingFactor %>% dplyr::summarize_all(myFactorSummary)
FactorSummary <-cbind(
  stat=c("n","missing","unique","freqRatio","1st mode","1st mode freq","2nd mode","2nd mode freq","least common","least common freq"),
  FactorSummary)
FactorSummaryFinal <- FactorSummary %>%
  pivot_longer("MSZoning":"SaleType", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*as.numeric(missing)/as.numeric(n),
         unique_pct = 100*as.numeric(unique)/as.numeric(n)) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())
options(digits=3)
options(scipen=99)
FactorSummaryFinal %>% kable()
  
  
summary(housingData)  

#Problem 2a
hist(housingData$GrLivArea, main='Histogram for Above ground living area', xlab='Above ground living area')
hist(housingData$SalePrice, main='Histogram for Sale Price', xlab='Sale Price')

boxcox(housingData$SalePrice, optimize = TRUE, lambda = c(-3,3))
lambda <- -0.09565572
transformed_SalePrice <- (housingData$SalePrice^lambda - 1) / lambda
hist(transformed_SalePrice, main = "Histogram of SalePrice (Box-Cox)", xlab = "Transformed Sale Price")


boxcox(housingData$GrLivArea, optimize = TRUE, lambda = c(-3,3))
lambda <- 0.1233492
transformed_GrLivArea <- (housingData$GrLivArea^lambda - 1) / lambda
hist(transformed_GrLivArea, main = "Histogram of Above ground living area(Box-Cox)", xlab = "Above ground living area")

#problem 2b
#i
mean_lotfrontage <- mean(housingData$LotFrontage, na.rm = TRUE)
housingData$LotFrontage[is.na(housingData$LotFrontage)] <- mean_lotfrontage
hist(housingData$LotFrontage, main='Histogram of Lot Frontage before mean value imputation', xlab='Lot Frontage')

#ii NOT SURE. 
 # Load necessary libraries
library(caret)	``
model <- train(LotFrontage ~ OverallQual + OverallCond + YearBuilt + GrLivArea + GarageCars + GarageArea, 
               data = housingData[!is.na(housingData$LotFrontage), ], 
               method = "lm")
predicted_values <- predict(model, newdata = housingData[is.na(housingData$LotFrontage), ])
housingData$LotFrontage[is.na(housingData$LotFrontage)] <- predicted_values

hist(housingData$LotFrontage, main='Histogram of Lot Frontage(regression with error)', xlab='Lot Frontage')

#iii NOT SURE
# Load necessary libraries
library(mice)

# Create a PMM imputation model with explicit predictor variables
# Specify the predictor variables to use for imputation
predictor_vars <- c("OverallQual", "OverallCond", "YearBuilt", "GrLivArea", "GarageCars", "GarageArea")
pmm_model <- mice(housingData, method = "pmm", formula = LotFrontage ~ . - 1 + predictor_vars, m = 1, maxit = 5)
# Note: You can adjust the 'm' and 'maxit' parameters as needed.

# Perform imputation
imputed_data <- complete(pmm_model)

# Replace the original dataset with the imputed dataset
housingData <- imputed_data

    
#2c

exterior1st_freq <- table(housingData$Exterior1st)
top_levels <- names(sort(exterior1st_freq, decreasing = TRUE)[1:4])
housingData$Exterior1st <- forcats::fct_other(housingData$Exterior1st, keep = top_levels, other_level = "Other")

#2di
library(tidyverse)
average_prices <- housingData %>%
  group_by(Neighborhood) %>%
  summarize(Avg_SalePrice = mean(SalePrice, na.rm = TRUE))
  
#ii
ggplot(data = housingData, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Neighborhood", y = "Sale Price") +
  ggtitle("Boxplot of Sale Prices by Neighborhood")
  
#iii
median_prices <- housingData %>%
  group_by(Neighborhood) %>%
  summarize(Median_Price = median(SalePrice, na.rm = TRUE)) %>%
  arrange(desc(Median_Price))
  
housingData$Neighborhood <- factor(housingData$Neighborhood, levels = median_prices$Neighborhood)

ggplot(data = housingData, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Neighborhood", y = "Sale Price") +
  ggtitle("Boxplot of Sale Prices by Neighborhood (Ordered by Median Price)")
    
