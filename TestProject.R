# Test Project
# Tyler Hutcherson
# tch6zf@virginia.edu
####################################
install.packages("Quandl")
library(Quandl)
Quandl.api_key("QTwA_u6oyP3DyrNzeVDS")
library(dplyr)
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




# some functions to handle the data wrangling
pMiss <- function(x){sum(is.na(x))/length(x)*100}
lag_matrix <- function(df, k){
  #handle missing values
  #clean
  #create lags
}






