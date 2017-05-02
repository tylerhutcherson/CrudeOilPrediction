# Test Project ~ PREDICTION
# Tyler Hutcherson
# tch6zf@virginia.edu
####################################
setwd("~/Desktop/Data_Science/Side_Projects/Metis")
library(dplyr)
library(caret)
library(purrr)
library(readr)
library(neuralnet)
library(boot)
set.seed(123456)
source("TestProjectScripts.R")
####################################

df <- read_csv("oil_cleaned.csv", col_names = TRUE) %>% 
  select(-c(1)) %>% 
  createLags(c(1:13))
df <- df[complete.cases(df),]

# Exp 1:  normalized data
exp1 <- df %>% 
          normalize()

# Exp 2: normalized + 3 day moving average data
exp2 <- df %>% 
          movingA(.,n = 3) %>% 
          .[complete.cases(.),] %>% 
          normalize()

# Exp 3: normlized + 3 day moving average + log-differenced data
exp3 <- df %>% 
          movingA(3) %>% 
          .[complete.cases(.),] %>% 
          map(function(s){
            if(is.date(s)){
              return(s)
            }
            else{
              diff(log(s))
           }
}) 
exp3$Date <- exp3$Date[-1814]
exp3 <- exp3 %>% 
          data.frame() %>%
          normalize()

##################################################

# PERFORM MODELING: Linear Model - baseline method

# Experiment 1 Data
set.seed(123456)
train <- exp1[exp1$Date < "2016-09-01",]
test <- setdiff(exp1,train)
exp1.lm.fit1 <- glm(Price~PriceLag1+PriceLag2+PriceLag3+PriceLag4+fCon1Lag1+fCon2Lag1+gold_silverLag1+globalLag1, data=train)
summary(exp1.lm.fit1)
pr.lm1 <- ((predict(exp1.lm.fit1,test)+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)
MSE.lm1 <- sum((pr.lm - (((test$Price+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)))^2)/nrow(test)
# RMSE = 0.7586
cv.glm(train,exp1.lm.fit1,K=5)$delta[1] #has the lowest CV MSE


exp1.lm.fit2 <- glm(Price~PriceLag1+fCon1Lag1+fCon2Lag1+gold_silverLag1+globalLag1, data=train)
summary(exp1.lm.fit2)
pr.lm2 <- ((predict(exp1.lm.fit2,test)+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)
MSE.lm2 <- sum((pr.lm2 - (((test$Price+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)))^2)/nrow(test)
# RMSE = 0.75736
cv.glm(train,exp1.lm.fit2,K=5)$delta[1]

exp1.lm.fit3 <- glm(Price~PriceLag1+PriceLag2+PriceLag3+PriceLag4+PriceLag5+PriceLag6+PriceLag7+PriceLag8+PriceLag9+PriceLag10+PriceLag11, data=train)
summary(exp1.lm.fit3)
pr.lm3 <- ((predict(exp1.lm.fit3,test)+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)
MSE.lm3 <- sum((pr.lm3 - (((test$Price+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)))^2)/nrow(test)
# RMSE = 0.749036
cv.glm(train,exp1.lm.fit3,K=5)$delta[1]

exp1.lm.fit4 <- glm(Price~fCon1Lag1+fCon2Lag1+fCon3Lag1+fCon4Lag1+gold_silverLag1+globalLag1, data=train)
summary(exp1.lm.fit4)
pr.lm4 <- ((predict(exp1.lm.fit4,test)+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)
MSE.lm4 <- sum((pr.lm4 - (((test$Price+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)))^2)/nrow(test)
# RMSE = 0.78797
cv.glm(train,exp1.lm.fit4,K=5)$delta[1]


# Experiment 2 Data
set.seed(123456)
train <- exp2[exp2$Date < "2016-09-01",]
test <- setdiff(exp2,train)
exp2.lm.fit1 <- glm(Price~PriceLag1+PriceLag2+PriceLag3+PriceLag4+fCon1Lag1+fCon2Lag1+gold_silverLag1+globalLag1, data=train)
summary(exp2.lm.fit1)
pr.lm1 <- ((predict(exp2.lm.fit1,test)+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)
MSE.lm1 <- sum((pr.lm1 - (((test$Price+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)))^2)/nrow(test)
# RMSE = 0.322409
cv.glm(train,exp2.lm.fit1,K=5)$delta[1] 

exp2.lm.fit2 <- glm(Price~PriceLag1+fCon1Lag1+fCon2Lag1+gold_silverLag1+globalLag1, data=train)
summary(exp2.lm.fit2)
pr.lm2 <- ((predict(exp2.lm.fit2,test)+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)
MSE.lm2 <- sum((pr.lm2 - (((test$Price+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)))^2)/nrow(test)
# RMSE = 0.4529021
cv.glm(train,exp2.lm.fit2,K=5)$delta[1]

exp2.lm.fit3 <- glm(Price~PriceLag1+PriceLag2+PriceLag3+PriceLag4+PriceLag5+PriceLag6+PriceLag7+PriceLag8+PriceLag9+PriceLag10+PriceLag11, data=train)
summary(exp2.lm.fit3)
pr.lm3 <- ((predict(exp2.lm.fit3,test)+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)
MSE.lm3 <- sum((pr.lm3 - (((test$Price+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)))^2)/nrow(test)
# RMSE = 0.269185
cv.glm(train,exp2.lm.fit3,K=5)$delta[1]  # WINNER

exp2.lm.fit4 <- glm(Price~fCon1Lag1+fCon2Lag1+fCon3Lag1+fCon4Lag1+gold_silverLag1+globalLag1, data=train)
summary(exp2.lm.fit4)
pr.lm4 <- ((predict(exp2.lm.fit4,test)+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)
MSE.lm4 <- sum((pr.lm4 - (((test$Price+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)))^2)/nrow(test)
# RMSE = 0.4813772
cv.glm(train,exp2.lm.fit4,K=5)$delta[1]

# Experiment 3 Data
set.seed(123456)
train <- exp3[exp3$Date < "2016-09-01",]
test <- setdiff(exp3,train)
exp3.lm.fit1 <- glm(Price~PriceLag1+PriceLag2+PriceLag3+PriceLag4+fCon1Lag1+fCon2Lag1+gold_silverLag1+globalLag1, data=train)
summary(exp3.lm.fit1)
pr.lm1 <- ((predict(exp3.lm.fit1,test)+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)
MSE.lm1 <- sum((pr.lm1 - (((test$Price+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)))^2)/nrow(test)
# RMSE = 0.6529223
cv.glm(train,exp3.lm.fit1,K=5)$delta[1] 

exp3.lm.fit2 <- glm(Price~PriceLag1+fCon1Lag1+fCon2Lag1+gold_silverLag1+globalLag1, data=train)
summary(exp3.lm.fit2)
pr.lm2 <- ((predict(exp3.lm.fit2,test)+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)
MSE.lm2 <- sum((pr.lm2 - (((test$Price+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)))^2)/nrow(test)
# RMSE = 0.4529021
cv.glm(train,exp3.lm.fit2,K=5)$delta[1]

exp3.lm.fit3 <- glm(Price~PriceLag1+PriceLag2+PriceLag3+PriceLag4+PriceLag5+PriceLag6+PriceLag7+PriceLag8+PriceLag9+PriceLag10+PriceLag11, data=train)
summary(exp3.lm.fit3)
pr.lm3 <- ((predict(exp3.lm.fit3,test)+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)
MSE.lm3 <- sum((pr.lm3 - (((test$Price+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)))^2)/nrow(test)
# RMSE = 0.269185
cv.glm(train,exp3.lm.fit3,K=5)$delta[1]  # WINNER

exp3.lm.fit4 <- glm(Price~fCon1Lag1+fCon2Lag1+fCon3Lag1+fCon4Lag1+gold_silverLag1+globalLag1, data=train)
summary(exp3.lm.fit4)
pr.lm4 <- ((predict(exp3.lm.fit4,test)+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)
MSE.lm4 <- sum((pr.lm4 - (((test$Price+1)*(max(df$Price)-min(df$Price))/2)+min(df$Price)))^2)/nrow(test)
# RMSE = 0.4813772
cv.glm(train,exp3.lm.fit4,K=5)$delta[1]









train <- df[2:nrow(df),]


fitControl_time <- trainControl(method = "timeslice",
                                initialWindow = 180,
                                fixedWindow = TRUE,
                                horizon = 20)

lmFit1 <- train(Price ~ Price_lag1+Price_lag2+Price_lag3+Price_lag4+Price_lag5+Price_lag6+global_lag1+gold_silver_lag1+fCon1_lag1+fCon2_lag1+fCon3_lag1+fCon4_lag1, data = train[,2:50], 
                 method = "lm", 
                 trControl = fitControl_time,
                 verbose = FALSE)
lmFit1


pcrFit1 <- train(Price ~ ., data = train[,c(2,9:50)], 
                 method = "pcr", 
                 trControl = fitControl_time,
                 verbose = FALSE)


svmFit1 <- train(Price ~ ., data = train[,c(2,9:50)], 
                 method = "svmPoly", 
                 trControl = fitControl_time,
                 verbose = FALSE)

nnFit1 <- train(Price ~ ., data = train[,c(2,9:50)], 
                 method = "nnet",
                preProcess = "pca")
