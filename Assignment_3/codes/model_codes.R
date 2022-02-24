#################################################
# Assignment 3                                  #
# Data Analysis 3                               #
# Predicting firm exit                          #
# Peter ENDES-NAGY                              #
#################################################

##SET UP
rm( list = ls() )
#libraries
library(tidyverse)
library(caret)
library(modelsummary)
library(data.table)
library(Hmisc)
library(pROC)

#Summary function, RMSE criteria. AUC could be done as well, but we are going to stick to RMSE
#used in 5-fold cv
#courtesy of Agoston Reguly
twoClassSummaryExtended <- function (data, lev = NULL, model = NULL)
{
  lvls <- levels(data$obs)
  rmse <- sqrt(mean((data[, lvls[1]] - ifelse(data$obs == lev[2], 0, 1))^2))
  c(defaultSummary(data, lev, model), "RMSE" = rmse)
}
#create calibration plot, courtesy of Agoston Reguly. 
create_calibration_plot <- function(data, prob_var, actual_var, y_lab = "Actual event probability" , n_bins = 10, breaks = NULL) {
  
  if (is.null(breaks)) {
    breaks <- seq(0,1,length.out = n_bins + 1)
  }
  
  binned_data <- data %>%
    mutate(
      prob_bin = cut(!!as.name(prob_var), 
                     breaks = breaks,
                     include.lowest = TRUE)
    ) %>%
    group_by(prob_bin, .drop=FALSE) %>%
    summarise(mean_prob = mean(!!as.name(prob_var)), mean_actual = mean(!!as.name(actual_var)), n = n())
  
  p <- ggplot(data = binned_data) +
    geom_line(aes(mean_prob, mean_actual), color='red', size=0.6, show.legend = TRUE) +
    geom_point(aes(mean_prob,mean_actual), color = 'red', size = 1, shape = 16, alpha = 0.7, show.legend=F, na.rm = TRUE) +
    geom_segment(x=min(breaks), xend=max(breaks), y=min(breaks), yend=max(breaks), color='blue', size=0.3) +
    theme_bw() +
    labs(x= "Predicted event probability",
         y= y_lab) +
    coord_cartesian(xlim=c(0,1), ylim=c(0,1))+
    expand_limits(x = 0.01, y = 0.01) +
    scale_y_continuous(expand=c(0.01,0.01),breaks=c(seq(0,1,0.1))) +
    scale_x_continuous(expand=c(0.01,0.01),breaks=c(seq(0,1,0.1))) 
  
  p
}

###########################################################
# Feature engineering
###########################################################

#Load clean and feature engineered data
data <- readRDS( url("https://github.com/bmbln/DA3_projects/blob/main/Assignment_3/data/data_engineered.RData?raw=true") )


#check the HGC
ggplot( data = data , aes( x = high_growth_f ) ) +
  geom_bar( aes( y = ( ..count.. ) / sum( ..count.. ) ) , fill = 'navyblue') +
  labs( y = 'Probability' , 
        x = NULL , 
        title = "HGC rate in the 2012 sample") +
  ylim( 0 , 1 ) +
  theme_bw()

# Main firm variables
rawvars <-  c("curr_assets", "curr_liab", "extra_exp", "extra_inc", "extra_profit_loss", "fixed_assets",
              "inc_bef_tax", "intang_assets", "inventories", "liq_assets", "material_exp", "personnel_exp",
              "profit_loss_year", "share_eq", "subscribed_cap")

# Data quality of financial variables
qualityvars <- c("balsheet_flag", "balsheet_length", "balsheet_notfullyear")
#problem with data and length\years that the balance sheet covers. 

engvar <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs","intang_assets_bs",
            "share_eq_bs", "subscribed_cap_bs", 
            "extra_exp_pl", "extra_inc_pl","extra_profit_loss_pl",
            "inc_bef_tax_pl", "inventories_pl", "profit_loss_year_pl", 
            "material_exp_pl",  "personnel_exp_pl")
# bs: divided by assets, pl by sales 
# total/fixed/liquid/current/intangible assets
# equity shares, subscribed capital
# extra expenditure, extra income, extra profit or loss
# Income before tax, inventories, profit or loss
# material and personnel expenditures

#quadratic forms of some above
engvar2 <- c("extra_profit_loss_pl_quad", "inc_bef_tax_pl_quad",
             "profit_loss_year_pl_quad", "share_eq_bs_quad")
#extra profit or loss, income before tax
#profit or loss, equity shares

# Flag variables
engvar3 <- c(grep("*flag_low$", names(data), value = TRUE) ,
             grep("*flag_high$", names(data), value = TRUE) ,
             grep("*flag_error$", names(data), value = TRUE) ,
             grep("*flag_zero$", names(data), value = TRUE) )

# Growth variables
d1 <-  c("d1_sales_mil_log_mod", "d1_sales_mil_log_mod_2", "d1_sales_mil_log_mod_3" , 
         "flag_low_d1_sales_mil_log", "flag_high_d1_sales_mil_log")
#historical sales growth (from 2011 to 2012), winsorized, flags incl. 
#quadratic and cubic forms incl. 


# Human capital related variables
hr <- c("female", "ceo_count", "foreign_management" ,
        "ceo_age", "flag_high_ceo_age", "flag_low_ceo_age", "flag_miss_ceo_age", 
        "labor_avg_mod", "flag_miss_labor_avg" )
#female CEO dummy, number of CEO's, foreign management
# winsorized CEO's age and flags (high/low/missing)  
# labor cost and missing value flag

# Firms history related variables
firm <- c("age", "age2", "new", 
          "ind2_cat", "m_region_loc", "urban_m")
# age, squared age, newly established (dummy) , 
# industry categories, headquarter's location, located in big city (dummy)

# interactions for logit, LASSO
interactions1 <- c("ind2_cat*age", "ind2_cat*age2",
                   "ind2_cat*d1_sales_mil_log", "ind2_cat*sales_mil_log",
                   "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                   "ind2_cat*female",   "ind2_cat*urban_m", "ind2_cat*labor_avg_mod")
#industry interactions

interactions2 <- c("sales_mil_log*age", "sales_mil_log*female",
                   "sales_mil_log*profit_loss_year_pl", "sales_mil_log*foreign_management")
#sales interactions


########
# Model setups
##Simple logit models 
X1 <- c("sales_mil_log" , 
        engvar ,
        firm , 
        d1 )
X2 <- c("sales_mil_log" , 
        engvar , engvar2 , engvar3 ,
        firm , hr , 
        qualityvars , 
        d1 )

## Logit with LASSO
logitvars <- c("sales_mil_log" , 
               engvar , engvar2 , engvar3 , 
               firm , hr ,
               qualityvars , 
               d1 , 
               interactions1 , interactions2 )

## RF (no interactions, no modified features)
rfvars  <-  c("sales_mil", "d1_sales_mil_log", rawvars, hr, firm, qualityvars)

#let's save the number of variables for later tables: flags, quadratic forms, interactions excl.
nvars <- list()
nvars[["X1"]] <- length( X1 ) -1-4 #correction for firm and d1
nvars[["X2"]] <- length( setdiff( X2 , c( engvar2 , engvar3 ) ) ) -1-4-4-1 #correction for firm,d1,hr,quality
nvars[["LASSO"]] <- length( setdiff( logitvars , c( engvar2 , engvar3 , interactions1 , interactions2 ) ) ) -1-4-4-1 #correction for firm,d1,hr,quality
nvars[["RF"]] <- length( rfvars ) -1-4-1 #correction for firm,hr,quality


######
#Separate the datasets 
set.seed(666)
# Create train and holdout samples
train_indices <- as.integer( createDataPartition( data$high_growth , p = 0.75 , list = FALSE ) )
data_train    <- data[ train_indices, ]
data_holdout  <- data[ -train_indices, ]

dim(data_train)
dim(data_holdout)


# 5 fold cross-validation
train_control <- trainControl(
  method = "cv", 
  number = 5 , #5-fold
  classProbs = TRUE , #it allows multiple classes if not binary
  summaryFunction = twoClassSummaryExtended , #called from aux, courtesy of Agoston Reguly. 
  savePredictions = TRUE
)

#####################3
## 2 logit models: 
logit_model_vars <- list( "X1" = X1 , "X2" = X2 )

CV_RMSE_folds <- list()
models <- list()

for (model_name in names( logit_model_vars ) ) {
  
  # setting the variables for each model
  features <- logit_model_vars[[model_name]]
  
  # Estimate logit model with 5-fold CV
  set.seed(666)
  glm_model <- train(
    formula( paste0( "high_growth_f ~", paste0( features , collapse = " + ") ) ) ,
    method    = "glm" , #linear model
    data      = data_train , #on the train set
    family    = binomial , #logit
    trControl = train_control #set up earlier
  )
  
  # Save the results to list
  models[[model_name]] <- glm_model
  # Save RMSE on test for each fold
  CV_RMSE_folds[[model_name]] <- glm_model$resample[ , c( "Resample" , "RMSE" ) ]
  }


#Logit with LASSO 

# Estimate logit with LASSO with 5-fold CV to find lambda
set.seed(666)
system.time({
  logit_lasso_model <- train(
    formula( paste0( "high_growth_f ~", paste0( logitvars , collapse = " + " ) ) ) ,
    data = data_train ,
    method = "glmnet" ,
    preProcess = c( "center" , "scale" ) ,
    family = "binomial" ,
    trControl = train_control ,
    tuneGrid = expand.grid( "alpha" = 1 , lambda = seq( .01 , 1 ,  by = .01 ) ) ,
    na.action = na.exclude
  )
})

# Save the results
tuned_logit_lasso_model <- logit_lasso_model$finalModel
best_lambda <- logit_lasso_model$bestTune$lambda
lasso_coeffs <- as.matrix( coef( tuned_logit_lasso_model , best_lambda ) )


models[["LASSO"]] <- logit_lasso_model
CV_RMSE_folds[["LASSO"]] <- logit_lasso_model$resample[ ,c( "Resample" , "RMSE" ) ]


#RF
# 5 fold cross-validation, same as for the logit models, with small addition
train_control$verboseIter <- TRUE

# Tuning parameters, it was tried earlier [5:8] for mtyr and [5,10,15,20,25,30] for min.node.size
#mtyr = 8 and min.node.size = 5 is chosen as the best by the machine
# We use 'gini index' to decide split rule
tune_grid <- expand.grid(
  .mtry = 8,
  .splitrule = "gini",
  .min.node.size = 25
)

set.seed(666)
rf_model <- train(
  formula( paste0( "high_growth_f ~ " , paste0( rfvars , collapse = " + ") ) ) ,
  method = "ranger" ,
  data = data_train ,
  tuneGrid = tune_grid ,
  trControl = train_control
)

models[["RF"]] <- rf_model

CV_RMSE_folds[["RF"]] <- rf_model$resample[,c("Resample", "RMSE")]

## Claculate AUC for each fold for each models 
CV_AUC_folds <- list()
for (model_name in names(models)) {
  
  auc <- list()
  model <- models[[model_name]]
  for (fold in c( "Fold1" , "Fold2" , "Fold3" , "Fold4" , "Fold5" ) ) {
    # get the prediction from each fold
    cv_fold <-
      model$pred %>%
      filter( Resample == fold )
    # calculate the roc curve
    roc_obj <- roc( cv_fold$obs , cv_fold$HGC , quiet = TRUE )
    # save the AUC value
    auc[[fold]] <- as.numeric( roc_obj$auc )
  }
  
  CV_AUC_folds[[model_name]] <- data.frame( "Resample" = names(auc) ,
                                           "AUC" = unlist(auc) )
}


#average RMSE and average AUC for each models
CV_RMSE <- list()
CV_AUC <- list()

for (model_name in names(models)) {
  CV_RMSE[[model_name]] <- mean(CV_RMSE_folds[[model_name]]$RMSE)
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}

ncoeffs <- lapply(models, FUN = function(x) length(x$coefnames))
# quick adjustment for LASSO
ncoeffs[["LASSO"]] <- sum(lasso_coeffs != 0) - 1 #Intercept excluded

model_summary <- data.frame( "N vars" = unlist( nvars ),
                        "N coeffs" = unlist( ncoeffs ),
                        "CV RMSE" = unlist( CV_RMSE ),
                        "CV AUC" = unlist( CV_AUC ) )


#loss functions 
#we have a bank that wants to offer premium services to HGC's and expects extra revenue from these companies, let's say 5.000 EUR
#the premium service comes with increased cost from the bank's part as it includes consulting services, let's say 1.000 EUR
#if a company isn't a HGC, then consultations costs lost, so FP is the consultation cost lost: 1.000 EUR
#if a company turns into HGC, it generates extra revenue for the bank, so a FN is loss of this extra revenue minus the consultation cost: 4.000 EUR
#relative costs: 4 VS 1
FP=1
FN=4
cost = FN/FP
# the prevalence, or the proportion of cases in the population (n.cases/(n.controls+n.cases))
prevelance = sum( data_train$high_growth )/length( data_train$high_growth )

#ROC curves and best thresholds 
best_tresholds <- list()
expected_loss <- list()

for (model_name in names(models) ) {
  
  model <- models[[model_name]]
  colname <- paste0(model_name,"_prediction")
  
  best_tresholds_cv <- list()
  expected_loss_cv <- list()
  
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc( cv_fold$obs , cv_fold$HGC , quiet = TRUE)
    # Add the weights (costs) here!
    best_treshold <- coords( roc_obj , "best" , ret = "all" , transpose = FALSE ,
                            best.method = "youden" , best.weights = c( cost , prevelance ))
    # save best treshold for each fold and save the expected loss value
    best_tresholds_cv[[fold]] <- best_treshold$threshold
    expected_loss_cv[[fold]] <- ( best_treshold$fp*FP + best_treshold$fn*FN )/length( cv_fold$HGC )
  }

  # average
  best_tresholds[[model_name]] <- mean( unlist( best_tresholds_cv ) )
  expected_loss[[model_name]]  <- mean( unlist( expected_loss_cv ) )
  
}

cv_summary_results <- data.frame("N vars" = unlist( nvars ),
                             "N coeffs" = unlist( ncoeffs ),
                             "CV RMSE" = unlist( CV_RMSE ),
                             "CV AUC" = unlist( CV_AUC ) ,
                             "CV tresholds" = unlist(best_tresholds),
                             "CV expected loss" = unlist(expected_loss) )

cv_summary_results

#claculate on hold out: RMSE, AUC, best threshold, expected loss
#+ creae confusion table for each models 
HO_RMSE <- list()
HO_AUC <- list()
HO_expected_loss <- list()
HO_best_tresholds <- list()
HO_prevelance = sum( data_holdout$high_growth )/length( data_holdout$high_growth )
confusion_tables <- list()
for (model_name in names(models) ) {
  #RMSE
  predicted_probabilities_holdout <- predict( models[[model_name]] , newdata = data_holdout , type = "prob" )
  data_holdout$prediction <- predicted_probabilities_holdout[ , "HGC"]
  HO_RMSE[[model_name]] <- RMSE( data_holdout$prediction , data_holdout$high_growth )
  
  # ROC curve on holdout
  roc_obj_holdout <- roc(data_holdout$high_growth, data_holdout[, "prediction", drop=TRUE ] , quiet=TRUE )
  # AUC
  HO_AUC[[model_name]] <- as.numeric(roc_obj_holdout$auc)
  
  #get best threshold and save for HO
  HO_best_treshold <- coords( roc_obj_holdout , "best" , ret = "all" , transpose = FALSE ,
                           best.method = "youden" , best.weights = c( cost , HO_prevelance ))
  # save best treshold for each fold and save the expected loss value
  HO_best_tresholds[[model_name]] <- HO_best_treshold$threshold
  # Get expected loss on holdout with optimal threshold
  holdout_treshold <- coords( roc_obj_holdout , x = HO_best_tresholds[[model_name]] , input= "threshold" ,
                             ret = "all" , transpose = FALSE )
  HO_expected_loss[[model_name]] <- ( holdout_treshold$fp*FP + holdout_treshold$fn*FN )/length( data_holdout$high_growth )
  
  #confusion matrices with optimal threshold 
  holdout_prediction <-
    ifelse( data_holdout$prediction < best_tresholds[[model_name]], "not_HGC", "HGC" ) %>%
    factor( levels = c("not_HGC", "HGC") )
  cm_object <- confusionMatrix( holdout_prediction , data_holdout$high_growth_f )
  confusion_tables[[model_name]] <- cm_object$table
}

HO_summary_results <- data.frame("N vars" = unlist( nvars ),
                                 "N coeffs" = unlist( ncoeffs ),
                                 "HO RMSE" = unlist( HO_RMSE ),
                                 "HO AUC" = unlist( HO_AUC ) ,
                                 "HO tresholds" = unlist(HO_best_tresholds),
                                 "HO expected loss" = unlist(HO_expected_loss) )

HO_summary_results
cv_summary_results

#RF dominates both the hold-out, both the CV set 
#let's see the confusion table on the hold-out sample
confusion_tables[["RF"]]
#in pct
round( confusion_tables[["RF"]] / sum( confusion_tables[["RF"]] ) * 100 , 1 )

#create calibration plots for each model
calibration_plots <- list()
for (model_name in names(models) ) {

  predicted_probabilities_holdout <- predict( models[[model_name]] , newdata = data_holdout , type = "prob" )
  data_holdout$prediction <- predicted_probabilities_holdout[ , "HGC"]
  
  calibration_plots[[model_name]] <-  create_calibration_plot( data_holdout, 
                          prob_var = "prediction", 
                          actual_var = "high_growth",
                          n_bins = 10)
}

#save( models , HO_summary_results , cv_summary_results , confusion_tables , calibration_plots , file = "~/Documents/CEU/DA3/DA3_projects/Assignment_3/data/models_and_results.RData" )



calibration_plots[["X2"]] + 
  ggtitle( "Calibration plot for Random Forest Model") +
  theme_bw()


