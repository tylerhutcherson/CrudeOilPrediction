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
library(nnet)
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
          .[complete.cases(.),] 
exp2Max <- max(exp2$Price)
exp2Min <- min(exp2$Price)
exp2 <- normalize(exp2)
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
          data.frame() 
exp3Max <- max(exp3$Price)
exp3Min <- min(exp3$Price)
exp3 <- normalize(exp3)
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
pr.lm1 <- ((predict(exp2.lm.fit1,test)+1)*(exp2Max-exp2Min)/2)+exp2Min
MSE.lm1 <- sum((pr.lm1 - (((test$Price+1)*(exp2Max-exp2Min)/2)+exp2Min))^2)/nrow(test)
# RMSE = 0.3168753
cv.glm(train,exp2.lm.fit1,K=5)$delta[1] 

exp2.lm.fit2 <- glm(Price~PriceLag1+fCon1Lag1+fCon2Lag1+gold_silverLag1+globalLag1, data=train)
summary(exp2.lm.fit2)
pr.lm2 <- ((predict(exp2.lm.fit2,test)+1)*(exp2Max-exp2Min)/2)+exp2Min
MSE.lm2 <- sum((pr.lm2 - (((test$Price+1)*(exp2Max-exp2Min)/2)+exp2Min))^2)/nrow(test)
# RMSE = 0.4451287
cv.glm(train,exp2.lm.fit2,K=5)$delta[1]

exp2.lm.fit3 <- glm(Price~PriceLag1+PriceLag2+PriceLag3+PriceLag4+PriceLag5+PriceLag6+PriceLag7+PriceLag8+PriceLag9+PriceLag10+PriceLag11, data=train)
summary(exp2.lm.fit3)
pr.lm3 <- ((predict(exp2.lm.fit3,test)+1)*(exp2Max-exp2Min)/2)+exp2Min
MSE.lm3 <- sum((pr.lm3 - (((test$Price+1)*(exp2Max-exp2Min)/2)+exp2Min))^2)/nrow(test)
# RMSE = 0.2645648
cv.glm(train,exp2.lm.fit3,K=5)$delta[1]  # WINNER

exp2.lm.fit4 <- glm(Price~fCon1Lag1+fCon2Lag1+fCon3Lag1+fCon4Lag1+gold_silverLag1+globalLag1, data=train)
summary(exp2.lm.fit4)
pr.lm4 <- ((predict(exp2.lm.fit4,test)+1)*(exp2Max-exp2Min)/2)+exp2Min
MSE.lm4 <- sum((pr.lm4 - (((test$Price+1)*(exp2Max-exp2Min)/2)+exp2Min))^2)/nrow(test)
# RMSE = 0.4731151
cv.glm(train,exp2.lm.fit4,K=5)$delta[1]

# Experiment 3 Data - ran into problems no time to worry about it

#########################################

# PERFORM MODELING: Neural Network (with Experiment 2 Data)

# multilayer
set.seed(123456)
cv.error <- NULL
k <- 5
trainIndex <- createDataPartition(exp2$Price, times=k, p=.8, list=F)
n <- names(exp2)
f <- as.formula(paste("Price ~", paste(n[!n %in% c("Price","Date")], collapse = " + ")))

for(i in 1:k){
  train.cv <- exp2[trainIndex[,i],]
  test.cv <- exp2[-trainIndex[,i],]
  nn <- neuralnet(f,data=train.cv,hidden=c(9,5),linear.output=T)
  pr.nn <- compute(nn,test.cv[,3:27])
  pr.nn <- (pr.nn$net.result+1)*(exp2Max-exp2Min)/2+exp2Min
  test.cv.r <- (test.cv$Price+1)*(exp2Max-exp2Min)/2+exp2Min
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
}

mean(sqrt(cv.error)) # RMSE =
# not bad - too many parameters to tune

# Singler Layer NNet
my.grid <- expand.grid(.decay = c(0, 0.05, 0.1), .size = c(5, 7, 9, 12))
trnCtrl=trainControl(
  method = "repeatedcv",
  number=5,
  repeats=3,
  search='grid',
)
train.cv <- exp2[exp2$Date < "2016-09-01",]
test.cv <- setdiff(exp2,train.cv)
caret.fit <- train(f, data = train.cv, trControl = trnCtrl, method = "nnet", maxit = 10000, tuneGrid = my.grid, trace = F, linout = 1) 

pr.nn <- predict(caret.fit$finalModel,test.cv[,3:27])
pr.nn <- (pr.nn+1)*(exp2Max-exp2Min)/2+exp2Min
test.cv.r <- (test.cv$Price+1)*(exp2Max-exp2Min)/2+exp2Min
sqrt(sum((test.cv.r - pr.nn)^2)/nrow(test.cv)) # RMSE = 0.094339

##########################################################

# PERFORM MODELING: Random Forest
my.grid.rf <- expand.grid(mtry=c(7,9,12,15))
caret.fit.rf <- train(f, data=train.cv, trControl = trnCtrl, method = 'rf', tuneGrid= my.grid.rf, trace=F, importance=TRUE)
pr.rf <- predict(caret.fit.rf$finalModel,test.cv[,3:27])
pr.rf <- (pr.rf+1)*(exp2Max-exp2Min)/2+exp2Min
test.cv.rf <- (test.cv$Price+1)*(exp2Max-exp2Min)/2+exp2Min
sqrt(sum((test.cv.rf - pr.rf)^2)/nrow(test.cv)) # RMSE = 0.2444824567


# tune tree size (how many trees to grow)
range <- seq(from = 100, to = 3000, by = 150)
range %>% map(function(s){
  randomForest(f, data=train.cv, mtry=15, importance=TRUE, 
               na.action=na.exclude, ntree = s) %>% 
    get("mse",.) %>% 
    mean()
}) -> tuneTreeSize

plot(range,tuneTreeSize, main="Tree Size and MSE")
# best # of trees = 2350


########################################################
########################################################

# Best Model = Single Layer Neural Network with 9 hidden units

# plot network
# build final model on full data except predication date: 4/27/17
# use tuning parameters from the grid search results

final.model <- nnet(f, data=exp2[1:(nrow(exp2)-1),], maxit = 10000, size = 9, decay = 0)
prediction <- predict(caret.fit$finalModel,exp2[nrow(exp2),3:27])
prediction <- (pr.nn+1)*(exp2Max-exp2Min)/2+exp2Min
actual <- (exp2$Price[nrow(exp2)]+1)*(exp2Max-exp2Min)/2+exp2Min
difference <- abs(actual-prediction)
percentDiff <- (difference/actual)*100




