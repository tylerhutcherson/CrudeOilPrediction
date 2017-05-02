# Test Project ~ Scripts
# Tyler Hutcherson
# tch6zf@virginia.edu
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(purrr)))

# This is a collection of functions I wrote to do data cleaning and manipulation for the prediction task

# perform 3-day moving average filter on all ts data except the trade date
is.date <- function(x) inherits(x, 'Date')

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

movingA <- function(df,n){
  df %>% 
    map(function(s){
      if(is.date(s)){
        return(s)
      }
      else{
        return(ma(s, order = n, centre=FALSE))
      }
    }) %>% 
    as.data.frame() -> df
  return(df)
}

## create lags in price variables and others
createLags <- function(df,sizes){
  sizes %>%
    map(function(size){
      values <- lag(df$Price, n=size)
      name <- paste0("PriceLag",size)
      list(name, values)
    }) %>%
    reduce(cbind) %>% 
    as.data.frame() %>% 
    header.true() %>% 
    map(function(s){
      unlist(s)
    }) %>% 
    as.data.frame() %>% 
    cbind(df,.) %>% 
    mutate(globalLag1 = lag(global, n=1), 
           gold_silverLag1 = lag(gold_silver, n=1),
           fCon1Lag1 = lag(fCon1, n=1), 
           fCon2Lag1 = lag(fCon2, n=1), 
           fCon3Lag1 = lag(fCon3, n=1), 
           fCon4Lag1 = lag(fCon4, n=1)) -> df
  return(df)
}

## rescale all variables except the date column to [-1,1]
norm <- function(col){
  return((2*(col - min(col))/(max(col)-min(col)))-1)
}

normalize <- function(df){
  df %>% map(function(s){
    if(is.date(s)){
      return(s)
    }
    else{
      return(norm(s))
    }
  }) %>% 
    as.data.frame() -> df
  return(df)
}
