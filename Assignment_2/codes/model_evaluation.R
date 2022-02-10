#################################################
# Assignment 2                                  #
# Data Analysis 3                               #
# Predicting AirBnB apartment prices in Paris   #
# Peter ENDES-NAGY                              #
#                                               #
#################################################

rm( list = ls() )
#libraries
library(tidyverse)
library(modelsummary)
library(caret)

##LOAD CLEAN DATA 
data_holdout <- read_csv( "https://raw.githubusercontent.com/bmbln/DA3_projects/main/Assignment_2/data/cleaned/data_holdout.csv") %>% 
  mutate_if( is.character, factor )
data_train <- read_csv( "https://raw.githubusercontent.com/bmbln/DA3_projects/main/Assignment_2/data/cleaned/data_train.csv") %>% 
  mutate_if( is.character, factor )

##LOAD model results
load( url("https://github.com/bmbln/DA3_projects/blob/main/Assignment_2/data/results/lasso_model.RData?raw=true") )
load( url("https://github.com/bmbln/DA3_projects/blob/main/Assignment_2/data/results/cart_model.RData?raw=true") )
load( url("https://github.com/bmbln/DA3_projects/blob/main/Assignment_2/data/results/rf_model.RData?raw=true") )
load( url("https://github.com/bmbln/DA3_projects/blob/main/Assignment_2/data/results/rf_auto_model.RData?raw=true") )


###################################
#### PARTIAL DEPENDENCE PLOTS #####
###################################
# 1) Numerical
#Number of accommodates
pdp_n_acc <- pdp::partial( rf_model_auto , pred.var = "n_accommodates" , 
                          pred.grid = distinct_( data_holdout , "n_accommodates" ) , 
                          train = data_train , 
                          inv.link = exp ) #our price variable is in log!!!

#Number of bedrooms
pdp_n_bedrooms <- pdp::partial( rf_model_auto , pred.var = "n_bedrooms" , 
                               pred.grid = distinct_( data_holdout , "n_bedrooms" ) , 
                               train = data_train , 
                               inv.link = exp ) #our price variable is in log!!!



# 2) Dummy variables in the TOP 10
#more than one bathrooms 
pdp_d_more_bathrooms <- pdp::partial( rf_model_auto , pred.var = "d_more_bathrooms" , 
                                pred.grid = distinct_( data_holdout , "d_more_bathrooms" ) , 
                                train = data_train , 
                                inv.link = exp ) #our price variable is in log!!!

#no available days in the next month month and the month after
pdp_d_unavailable_31_90 <- pdp::partial( rf_model_auto , pred.var = "d_unavailable_31_90" , 
                                      pred.grid = distinct_( data_holdout , "d_unavailable_31_90" ) , 
                                      train = data_train , 
                                      inv.link = exp ) #our price variable is in log!!!

#Amenity: dishwasher
pdp_am_dishwasher <- pdp::partial( rf_model_auto , pred.var = "am_dishwasher" , 
                                         pred.grid = distinct_( data_holdout , "am_dishwasher" ) , 
                                         train = data_train , 
                                         inv.link = exp ) #our price variable is in log!!!


#3) Factor type of variables
#Room type: private
pdp_fac_room_type <- pdp::partial( rf_model_auto , pred.var = "fac_room_type" , 
                                   pred.grid = distinct_( data_holdout , "fac_room_type" ) , 
                                   train = data_train , 
                                   inv.link = exp ) #our price variable is in log!!!


#Host has more than 5 listings
pdp_fac_host_listings_count <- pdp::partial( rf_model_auto , pred.var = "fac_host_listings_count" , 
                                  pred.grid = distinct_( data_holdout , "fac_host_listings_count" ) , 
                                  train = data_train , 
                                  inv.link = exp ) #our price variable is in log!!!


#save them locally so they can be called later directly
save( pdp_fac_host_listings_count , pdp_fac_room_type , 
        pdp_am_dishwasher , 
        pdp_d_unavailable_31_90 , pdp_d_more_bathrooms , 
        pdp_n_acc , pdp_n_bedrooms , pdp_n_review_scores_rating  , file = '~/Documents/CEU/DA3/DA3_projects/Assignment_2/data/results/dpd.RData')


