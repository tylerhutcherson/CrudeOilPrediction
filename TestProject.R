# Test Project
# Tyler Hutcherson
# tch6zf@virginia.edu
####################################
install.packages("Quandl")
library(Quandl)
Quandl.api_key("QTwA_u6oyP3DyrNzeVDS")
library(dplyr)
library(mice)
library(purrr)
####################################

## set working directory to proper location 
setwd("~/Desktop/Data_Science/Side_Projects/Metis")

## read in daily oil spot prices
daily_oil <- read.csv("Cushing_OK_WTI_Spot_Price_FOB_Daily.csv") %>% 
  mutate(Trade_Date=as.Date(as.character(Trade.Date), "%m/%d/%y")) %>% 
  select(-c(1))

## read in Global Index, Gold/Silver, Crude Oil futures contracts 1-4 from Quandl
oil_predictors <- Quandl(c("NASDAQOMX/NQGI","NASDAQOMX/XAU","EIA/PET_RCLC1_D","EIA/PET_RCLC2_D","EIA/PET_RCLC3_D","EIA/PET_RCLC4_D")) %>% 
  select(-c(3:6,8:11)) %>% 
  subset(`Trade Date`>"2004-12-31")
names(oil_predictors)[1] <- "Trade_Date"

## merge data together
df <- merge(x = daily_oil,y = oil_predictors, by = "Trade_Date", all.y = TRUE, all.x = FALSE) 
df <- df[complete.cases(df[5:8]),] #pattern noticed in the 5-8 columns for missing values..
temp <- mice(df[,2:8], method="norm") #perform multiple imputation on the rest using bayesian linear imputation
df <- cbind(df$Trade_Date,temp$data)  #rejoin
names(df) <- c("Date", "Price", "global", "gold_silver", "fCon1", "fCon2", "fCon3", "fCon4") #rename columns

## rescale all variables except the date 
is.date <- function(x) inherits(x, 'Date')

df %>% map(function(s){
  if(is.date(s)){
    return(s)
  }
  else{
    return(scale(s,center=TRUE,scale=TRUE))
  }
}) %>% 
  as.data.frame() -> df






