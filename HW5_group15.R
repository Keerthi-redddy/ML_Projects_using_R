library(tidyverse)
library(mice)
library(caret)
library(corrplot)
library(MASS)
library(car)
library(glmnet)
library(ggplot2)
library(kernlab)
install.pacakges("svmPloy")
library(svmPoly)

#Load data

data<- read.csv("/Users/mahithamallapu/Desktop/R_Programs/housingData.csv", na.strings = c("", NA))

#splitting the data into train and test

test.data <- data[1:100, ]
train.data <- data[101:nrow(data),]


#checking the variables with missing values

na.perc <- colMeans(is.na(train.data))*100
#Variables having more than 80% missing values
na.perc[na.perc>80] 

train.data <- train.data[, !names(train.data) %in% names(na.perc[na.perc > 80])]

train.data <- train.data %>%
  dplyr::mutate(dplyr::across(!where(is.numeric), as.factor)) %>%
  dplyr::select(-Id)

na.perc <- colMeans(is.na(train.data))*100
na.perc[na.perc > 0]

missing.LotFrontage <- is.na(train.data$LotFrontage)
missing.FireplaceQu <- is.na(train.data$FireplaceQu)
# Long running line
imputer <- mice(train.data, method = "pmm", m=5, maxit=50, seed=500)
train.data <- complete(imputer)

na.perc <- colMeans(is.na(train.data))*100
na.perc[na.perc > 0] # Shows that the number of missing values in all columns is Zero

par(mfrow=c(1, 2))
plot(train.data$SalePrice, train.data$LotFrontage, col=as.factor(missing.LotFrontage),
     main="Complete Lot Frontage", xlab="Sale Price", ylab="Lot Frontage")
plot(train.data$SalePrice, train.data$FireplaceQu, col=as.factor(missing.FireplaceQu),
     main="Complete Fireplace Qu", xlab="Sale Price", ylab="FireplaceQu")

##Scaling and Centering numeric values in the test dataset

pre.processor <- preProcess(train.data, method = c("center","scale"))
train.data <- predict(pre.processor, train.data)
head(train.data)

## i) OLS

fit.ols <- lm(data=train.data, SalePrice~.)

fit.ols.rmse <- RMSE(fit.ols$fitted.values, train.data$SalePrice)

p.values <- summary(fit.ols)$coeff[,"Pr(>|t|)"]
p.values[p.values > 0.8]


##Finding the correlations between numeric variables of the dataset.
M <- train.data %>%
  dplyr::select(where(is.numeric)) %>%
  cor()

corrplot(M)

fit.ols2 <- update(fit.ols, SalePrice~.-GarageArea-GarageYrBlt-GrLivArea-LotFrontage-YrSold)

ols <- c(summary(fit.ols)$adj.r.squared,
         AIC(fit.ols), BIC(fit.ols))
ols2 <- c(summary(fit.ols2)$adj.r.squared,
          AIC(fit.ols2), BIC(fit.ols2))
stats <- data.frame(ols, ols2, row.names = c("Adj R-Square", "AIC", "BIC"))
stats

fit.ols2.step <- stepAIC(fit.ols2, direction = "both", trace = FALSE, 
                         k = log(nrow(train.data)))
fit.ols2.step$anova


fit.ols.final <- lm(data=train.data, SalePrice ~ MSSubClass + MSZoning + LotArea + Neighborhood +
                      OverallQual + OverallCond + YearBuilt + MasVnrType + MasVnrArea +
                      BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF +
                      X2ndFlrSF + BedroomAbvGr + GarageFinish + GarageCars + WoodDeckSF +
                      OpenPorchSF + EncPorchSF)
ols3 = c(summary(fit.ols.final)$adj.r.squared, AIC(fit.ols.final), BIC(fit.ols.final))
cbind(stats, ols3)

# The Variables, Coefficient estimates, respective p-values, adjusted R-square values
summary(fit.ols.final)

# Saving to variable so that it can be used in Final Summary
fit.ols.final.rmse <- RMSE(fit.ols.final$fitted.values, train.data$SalePrice)

fit.ols.final.rmse
AIC(fit.ols.final)
BIC(fit.ols.final)
vif(fit.ols.final)

##ii)Residual analysis
residuals.vs.fitted <- data.frame(residuals = fit.ols.final$residuals,
                                  fitted = fit.ols.final$fitted.values)
ggplot(residuals.vs.fitted, aes(x=fitted, y=residuals)) +
  geom_point() +
  geom_smooth()

##preprocessing the data
#Function to pre-process the dataset

pre.process <- function(data) {
  na.perc <- colMeans(is.na(data))*100
  cleaned.data <- 
    data[, !names(data) %in% names(na.perc[na.perc > 80])] %>%
    dplyr::mutate(dplyr::across(!where(is.numeric), as.factor)) %>%
    dplyr::select(-Id) %>%
    mice(method = "pmm", m=5, maxit=50, seed=500) %>%
    complete()
  
  pre.processor <- preProcess(cleaned.data, method = c("center","scale"))
  cleaned.data <- predict(pre.processor, cleaned.data)
  return(cleaned.data)
}


#Reading the dataset and doing pre processing for all 1000 rows using above function

data <- read.csv("/Users/mahithamallapu/Desktop/R_Programs/housingData.csv")
data$SalePrice <- log(data$SalePrice) # Converting the SalePrice into log values

clean.data <- pre.process(data)

# Creating the train controller to use Cross Validation method
fitControl <- trainControl(method = "cv")

# Grid for various values of "Number of Components"
plsGrid <- expand.grid(ncomp=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

# Fitting the PLS model using above fitControl and ncompGrid values
fit.pls <- train(SalePrice ~ .,
                 data = clean.data, 
                 method = "pls",
                 trControl = fitControl,
                 tuneGrid = plsGrid)

# Plot to show the RMSE values for each number of components
plot(fit.pls)

#Below code shows the Number of Components for the Final Model selected based on the RMSE value.

fit.pls$bestTune$ncomp

#Lastly finding the CV RMSE and R-squared values

fit.pls$results %>%
  filter(ncomp == fit.pls$bestTune$ncomp) %>%
  dplyr::select(ncomp, RMSE, Rsquared)


## 1c. LASSO
#Using hyper-parameter tuning to determine the hyper parameter values with RMSE as the 
lassoGrid <- expand.grid(fraction=c(seq(0.1,0.9,0.1)))
fit.lasso = train(SalePrice ~ .,
                  data = clean.data, 
                  method = "lasso",
                  trControl = fitControl,
                  tuneGrid = lassoGrid)

# Ploting to visualize the RMSE values for each number of components
plot(fit.lasso)
#value for fraction based on value of RMSE
fit.lasso$bestTune$fraction
#the CV RMSE estimate for the final model
fit.lasso$results[fit.lasso$results$fraction == fit.lasso$bestTune$fraction,c("fraction" , "RMSE" , "Rsquared")]
#####1d
#Principal component regression
pcrGrid <- expand.grid(ncomp = c(seq(1, 10)))
fit.pcr = train(SalePrice ~ .,
                data = clean.data, 
                method = "pcr",
                trControl = fitControl,
                tuneGrid = pcrGrid)

plot(fit.pcr)
#Ridge regression
ridgeGrid <- expand.grid(lambda = seq(1, 4, 0.1) * 0.01)
fit.ridge = train(SalePrice ~ .,
                  data = clean.data, 
                  method = "ridge",
                  trControl = fitControl,
                  tuneGrid = ridgeGrid)

plot(fit.ridge)
#Elasticnet Regression
elasticNetGrid <- expand.grid(
  fraction = c(seq(0.1, 0.9, 0.1)),  # Fraction values
  lambda = c(0.01, 0.1, 1, 10, 100)  # Lambda values
)
fit.enet = train(SalePrice ~ .,
                 data = clean.data, 
                 method = "enet",
                 trControl = fitControl,
                 tuneGrid = elasticNetGrid)

plot(fit.enet)
#SVR
library(caret)
svmGrid <- expand.grid(
  degree = c(2, 3, 4),     # Degrees of the polynomial kernel
  scale = c(1.0, 1.2, 1.5),  # Scaling factor
  C = c(0.001, 0.01, 0.1)  # Cost parameter
)
fit.svm = train(SalePrice ~ ., 
                data = clean.data, 
                method = "svmPoly", 
                trControl = fitControl,
                tuneGrid = svmGrid)

plot(fit.svm)

# Defining the data frame with model information and results
 data.frame(
  Model = c("OLS", "OLS", "PLS", "LASSO", "PCR", "Ridge", "ENET", "SVR"),
  Notes = c("lm", "lm + filtered variables", "pls", "caret", "caret", "caret ", "caret and elasticnet", "caret and kernlab using svmPoly"),
  Hyperparameters = c("N/A", "N/A",
                      sprintf("ncomp = %s", fit.pls$bestTune$ncomp),
                      sprintf("fraction = %s", fit.lasso$bestTune$fraction),
                      sprintf("ncomp = %s", fit.pcr$bestTune$ncomp),
                      sprintf("lambda = %s", fit.ridge$bestTune$lambda),
                      sprintf("fraction = %s, lambda = %s", 
                              fit.enet$bestTune$fraction, fit.enet$bestTune$lambda),
                      sprintf("degree = %s, scale = %s, C = %s",
                              fit.svr$bestTune$degree, fit.svr$bestTune$scale, fit.svr$bestTune$C)),
  `CV RMSE` = c(fit.ols.rmse, 
                fit.ols.final.rmse, 
                fit.pls$results %>%
                  filter(ncomp == fit.pls$bestTune$ncomp) %>%
                  dplyr::select(RMSE) %>% as.numeric(), 
                fit.lasso$results %>%
                  filter(fraction == fit.lasso$bestTune$fraction) %>%
                  dplyr::select(RMSE) %>% as.numeric(), 
                fit.pcr$results %>%
                  filter(ncomp == fit.pcr$bestTune$ncomp) %>%
                  dplyr::select(RMSE) %>% as.numeric(), 
                fit.ridge$results %>%
                  filter(lambda == fit.ridge$bestTune$lambda) %>%
                  dplyr::select(RMSE) %>% as.numeric(), 
                fit.enet$results %>%
                  filter(fraction == fit.enet$bestTune$fraction &
                           lambda == fit.enet$bestTune$lambda) %>%
                  dplyr::select(RMSE) %>% as.numeric(),
                fit.svr$results %>%
                  filter(degree == fit.svr$bestTune$degree &
                           scale == fit.svr$bestTune$scale &
                           C == fit.svr$bestTune$C) %>%
                  dplyr::select(RMSE) %>% as.numeric()),

  `CV R-Square` = c(summary(fit.ols)$adj.r.squared, 
                    summary(fit.ols.final)$adj.r.squared, 
                    fit.pls$results %>%
                      filter(ncomp == fit.pls$bestTune$ncomp) %>%
                      dplyr::select(Rsquared) %>% as.numeric(), 
                    fit.lasso$results %>%
                      filter(fraction == fit.lasso$bestTune$fraction) %>%
                      dplyr::select(Rsquared) %>% as.numeric(), 
                    fit.pcr$results %>%
                      filter(ncomp == fit.pcr$bestTune$ncomp) %>%
                      dplyr::select(Rsquared) %>% as.numeric(), 
                    fit.ridge$results %>%
                      filter(lambda == fit.ridge$bestTune$lambda) %>%
                      dplyr::select(Rsquared) %>% as.numeric(), 
                    fit.enet$results %>%
                      filter(fraction == fit.enet$bestTune$fraction &
                               lambda == fit.enet$bestTune$lambda) %>%
                      dplyr::select(Rsquared) %>% as.numeric(),
                    fit.svr$results %>%
                      filter(degree == fit.svr$bestTune$degree &
                               scale == fit.svr$bestTune$scale &
                               C == fit.svr$bestTune$C) %>%
                             dplyr::select(Rsquared) %>% as.numeric())
                    
                    
  )
  
                             






