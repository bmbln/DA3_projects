#################################################
# Assignment 2                                  #
# Data Analysis 3                               #
# Predicting AirBnB apartment prices in Paris   #
# Peter ENDES-NAGY                              #
#################################################

##SET UP
rm( list = ls() )
#libraries
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(cowplot)
library(modelsummary)
library(fixest)
library(data.table)

################################################
##LOAD AND CLEAN DATA PRE-FEATURE-ENGINEERING###
################################################

rm(listings_b)










########################
#2) Feature engineering
#Remove irrelevant variables 
#Missing values
data <- as.data.frame( listings )
to_filter <- sapply( data , function(x) sum( is.na( x ) ) )
to_filter[ to_filter > 0 ]

#Target variable: price. No missing value, otherwise we should drop it. 
#Imputating 

data2 <- data %>%
  mutate(
    #assume N beds is N accomodates. Because of double beds, a flag shall be used. N=648
    flag_beds = ifelse( is.na( beds ) , 1 , 0 ) ,
    n_beds = ifelse( is.na( beds ) , accommodates , beds) ,
    
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds),
  )
#Drop if too many missing (see bathrooms, all missing) or not that important for us. 
to_drop <- c( "bathrooms" , )
