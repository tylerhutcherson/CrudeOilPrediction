# Test Project ~ CLEANING
# Tyler Hutcherson
# tch6zf@virginia.edu
####################################
install.packages("Quandl")
library(Quandl)
Quandl.api_key("QTwA_u6oyP3DyrNzeVDS")
library(dplyr)
library(mice)
library(purrr)
library(tseries)
library(forecast)
####################################

## set working directory to proper location 
setwd("~/Desktop/Data_Science/Side_Projects/Metis")

## read in daily oil spot prices
daily_oil <- read.csv("Cushing_OK_WTI_Spot_Price_FOB_Daily.csv") %>% 
  mutate(Trade_Date=as.Date(as.character(Trade.Date), "%m/%d/%y")) %>% 
  select(-c(1))

## read in Global Index, Gold/Silver, Crude Oil futures contracts 1-4 from Quandl
oil_predictors <- Quandl(c("NASDAQOMX/NQGI","NASDAQOMX/XAU","EIA/PET_RCLC1_D","EIA/PET_RCLC2_D","EIA/PET_RCLC3_D","EIA/PET_RCLC4_D")) %>% 
  select(-c(3:6,8:11)) 
names(oil_predictors)[1] <- "Trade_Date"

## merge data together
df <- merge(x = daily_oil,y = oil_predictors, by = "Trade_Date", all.y = FALSE, all.x = FALSE) 
df <- df[complete.cases(df[5:8]),] #pattern noticed in the 5-8 columns for missing values..
df <- df[df$Trade_Date > "2015-05-01",] #grab last two years of data
names(df) <- c("Date","Price", "global", "gold_silver", "fCon1", "fCon2", "fCon3", "fCon4") #rename columns

###########################################################
## EXPLORATORY ANALYSIS ##
plot.ts(df$Price, main="2015-Present Crude Oil Price in $$", ylab = "Price in $$")
plot.ts(df$gold_silver, main="2015-Present Gold/Silver Price in $$", ylab = "Price in $$")
plot.ts(df$global, main="2015-Present Global Index Price in $$", ylab = "Price in $$")
plot.ts(df$fCon1, main="2015-Present Futures Contract 1 Price in $$", ylab = "Price in $$")
plot.ts(df$fCon2, main="2015-Present Futures Contract 2 Price in $$", ylab = "Price in $$")
plot.ts(df$fCon3, main="2015-Present Futures Contract 3 Price in $$", ylab = "Price in $$")
plot.ts(df$fCon4, main="2015-Present Futures Contract 4 Price in $$", ylab = "Price in $$")

# calculate the total error in the futures contracts
sum(abs(df$Price-df$fCon1), na.rm = TRUE) #84.21
sum(abs(df$Price-df$fCon2), na.rm = TRUE) #466.05
sum(abs(df$Price-df$fCon3), na.rm = TRUE) #810.77
sum(abs(df$Price-df$fCon4), na.rm = TRUE) #1108.96

# perform 3-day moving average filter on all ts data
df[,2:8] %>% map(function(s){
  ma(s, order = 3, centre=FALSE)
}) %>% 
  as.data.frame() %>% 
  cbind(df$Date,.) -> df

df <- df[complete.cases(df),]

# non-stationarity is problematic.. difference the data
#adf.test(df$Price)
#adf.test(df$global)
#adf.test(df$gold_silver)
#adf.test(df$fCon1)
#adf.test(df$fCon2)
#adf.test(df$fCon3)
#adf.test(df$fCon4)

sizes <- c(1:13)
sizes %>%
  map(function(size){
    values <- lag(df$Price, n=size)
    name <- paste0("PriceLag",size)
    list(name, values)
  }) %>%
  reduce(cbind) %>%
  cbind(df) %>%
  glimpse



















## rescale all variables except the date column
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

## create lagged variables
df <- mutate_all(df,funs(lag1 = lag(.,1))) %>% 
  mutate_all(funs(lag2 = lag(.,2))) %>% 
  mutate_all(funs(lag3 = lag(.,3))) %>% 
  select(-c(9,17,25:33,41,49,57)) %>% 
  arrange(desc(Date))

names(df)[30:50] <- c("Price_lag4", "global_lag4", "gold_silver_lag4", "fCon1_lag4","fCon2_lag4","fCon3_lag4","fCon4_lag4",
                      "Price_lag5", "global_lag5", "gold_silver_lag5", "fCon1_lag5","fCon2_lag5","fCon3_lag5","fCon4_lag5",
                      "Price_lag6", "global_lag6", "gold_silver_lag6", "fCon1_lag6","fCon2_lag6","fCon3_lag6","fCon4_lag6")


df <- df[complete.cases(df),]

write.csv(df,"oil_cleaned.csv")




