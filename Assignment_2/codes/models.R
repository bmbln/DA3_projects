#################################################
# Assignment 2                                  #
# Data Analysis 3                               #
# Predicting AirBnB apartment prices in Paris   #
# Peter ENDES-NAGY                              #
#                                               #
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
library(rattle)

##LOAD CLEAN DATA 
data <- read_csv( "https://raw.githubusercontent.com/bmbln/DA3_projects/main/Assignment_2/data/cleaned/paris_listings_clean.csv") %>% 
  mutate_if( is.character, factor )

  
# create train and holdout samples -------------------------------------------
set.seed(666)
train_indices <- as.integer( createDataPartition( data$ln_price , p = 0.7, list = F ) )
data_train <- data[ train_indices , ]
data_holdout <- data[ -train_indices , ]

#save locally
#write_csv( data_train , "~/Documents/CEU/DA3/DA3_projects/Assignment_2/data/cleaned/data_train.csv")
#write_csv( data_holdout , "~/Documents/CEU/DA3/DA3_projects/Assignment_2/data/cleaned/data_holdout.csv")

dim(data_train)
dim(data_holdout)

#VARIABLES for models
#group variables for the models 
poly_vars <- c( "n_days_since2"  , "n_days_since3" , 
                     "n_accommodates2" , "n_accommodates3" ,
                     "n_bedrooms2" , "n_bedrooms3" )  
amenities_vars <- grep( "^am_.*", names( data ) , value = TRUE )
price_vars <- c( "n_price" , "ln_price" )
simple_vars <- setdiff( colnames(data) , c( poly_vars , amenities_vars , price_vars ) )
#interaction terms for LASSO 
interactions <- c( "n_accommodates*fac_property_type" , "fac_room_type*fac_property_type" , 
                   "fac_property_type*fac_neighbourhood_cleansed" , "fac_room_type*fac_neighbourhood_cleansed" , "n_accommodates*fac_neighbourhood_cleansed" )

#save them into predictors vars
predictors_1 <- c( simple_vars , amenities_vars )
predictors_2 <- c( simple_vars , amenities_vars , poly_vars , interactions )

######MODEL BUILDING######
# do 5-fold CV
train_control <- trainControl( method = "cv" ,
                              number = 5 ,
                              verboseIter = FALSE )

# set tuning
tune_grid <- expand.grid(
  .mtry = c(10) ,
  .splitrule = "variance" ,
  .min.node.size = c(5)
)

#LASSO with predictors_2 (interactions and polinomials incl) 
set.seed(666)
system.time({
  lasso_model <- train(
    formula( paste0( "ln_price ~" , paste0( predictors_2 , collapse = " + " ) ) ) ,
    data = data_train ,
    method = "glmnet" ,
    preProcess = c( "center" , "scale" ) ,
    tuneGrid =  expand.grid( "alpha" = 1 , "lambda" = seq( 0.01 , 0.25 , by = 0.01 ) ) ,
    trControl = train_control
  )
})

#CART with predictors_1
set.seed(666)
system.time({
  cart_model <- train(
    formula( paste0( "ln_price ~" , paste0( predictors_1 , collapse = " + " ) ) ) ,
    data = data_train ,
    method = "rpart" ,
    tuneLength = 10 ,
    trControl = train_control
  )
})

#Random Forest with predictors_1
set.seed(666)
system.time({
  rf_model <- train(
    formula( paste0( "ln_price ~" , paste0( predictors_1 , collapse = " + " ) ) ) ,
    data = data_train ,
    method = "ranger" ,
    trControl = train_control ,
    tuneGrid = tune_grid ,
    importance = "impurity"
  )
})

#Random Forest with predictors_1, autotuning
set.seed(666)
system.time({
   rf_model_auto <- train(
     formula(paste0( "ln_price ~" , paste0( predictors_1 , collapse = " + " ) ) ) ,
     data = data_train ,
     method = "ranger" ,
     trControl = train_control ,
     importance = "impurity"
   )
})

#Save model results locally 
#save( lasso_model , file = '~/Documents/CEU/DA3/DA3_projects/Assignment_2/data/results/lasso_model.RData' )
#save( cart_model , file = '~/Documents/CEU/DA3/DA3_projects/Assignment_2/data/results/cart_model.RData' )
#save( rf_model , file = '~/Documents/CEU/DA3/DA3_projects/Assignment_2/data/results/rf_model.RData' )
#save( rf_model_auto , file = '~/Documents/CEU/DA3/DA3_projects/Assignment_2/data/results/rf_auto_model.RData' )
