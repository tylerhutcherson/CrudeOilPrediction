# Crude Oil Prediction ~ Data Gathering and Merging
# Tyler Hutcherson
# tch6zf@virginia.edu
####################################
suppressWarnings(suppressMessages(install.packages("Quandl")))
suppressWarnings(suppressMessages(library(Quandl)))
## INSTALL YOUR API KEY HERE
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(purrr)))
suppressWarnings(suppressMessages(library(tseries)))
suppressWarnings(suppressMessages(library(forecast)))
####################################

## read in daily oil spot prices - make sure .csv file is in the project directory
daily_oil <- read.csv("Cushing_OK_WTI_Spot_Price_FOB_Daily.csv") %>% 
  mutate(Trade_Date=as.Date(as.character(Trade.Date), "%m/%d/%y")) %>% 
  select(-c(1))

## read in Global Index, Gold/Silver Index, Crude Oil futures contracts 1-4 from Quandl
oil_predictors <- Quandl(c("NASDAQOMX/NQGI","NASDAQOMX/XAU","EIA/PET_RCLC1_D","EIA/PET_RCLC2_D","EIA/PET_RCLC3_D","EIA/PET_RCLC4_D")) %>% 
  select(-c(3:6,8:11)) 
names(oil_predictors)[1] <- "Trade_Date"

## merge data together
df <- merge(x = daily_oil,y = oil_predictors, by = "Trade_Date", all.y = FALSE, all.x = FALSE) 
df <- df[complete.cases(df[5:8]),] #pattern noticed in the 5-8 columns for missing values..
df <- df[df$Trade_Date>="2010-01-01",]
names(df) <- c("Date","Price", "global", "gold_silver", "fCon1", "fCon2", "fCon3", "fCon4") #rename columns

###########################################################
## EXPLORATORY ANALYSIS ##
plot.ts(df$Price, main="2010-Present Crude Oil Price in $$", ylab = "Price in $$")
plot.ts(df$gold_silver, main="2010-Present Gold/Silver Price in $$", ylab = "Price in $$")
plot.ts(df$global, main="2010-Present Global Index Price in $$", ylab = "Price in $$")
plot.ts(df$fCon1, main="2010-Present Futures Contract 1 Price in $$", ylab = "Price in $$")
plot.ts(df$fCon2, main="2010-Present Futures Contract 2 Price in $$", ylab = "Price in $$")
plot.ts(df$fCon3, main="2010-Present Futures Contract 3 Price in $$", ylab = "Price in $$")
plot.ts(df$fCon4, main="2010-Present Futures Contract 4 Price in $$", ylab = "Price in $$")

# calculate the total error in the futures contracts
sum(abs(df$Price-df$fCon1), na.rm = TRUE) #357.6
sum(abs(df$Price-df$fCon2), na.rm = TRUE) #1523.19
sum(abs(df$Price-df$fCon3), na.rm = TRUE) #2665.97
sum(abs(df$Price-df$fCon4), na.rm = TRUE) #3670.78

# non-stationarity is problematic.. difference the data? 
#adf.test(df$Price)
#adf.test(df$global)
#adf.test(df$gold_silver)
#adf.test(df$fCon1)
#adf.test(df$fCon2)
#adf.test(df$fCon3)
#adf.test(df$fCon4)
##########################################################

# write out the cleaned data, consisting of spot prices from 2010-present
write.csv(df,"oil_cleaned.csv")







