# Test Project ~ PREDICTION
# Tyler Hutcherson
# tch6zf@virginia.edu
####################################
library(dplyr)
library(caret)
library(purrr)
set.seed(123456)
####################################


df <- read.csv("oil_cleaned.csv") %>% 
  select(-c(1))

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
