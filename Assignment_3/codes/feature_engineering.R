#################################################
# Assignment 3                                  #
# Data Analysis 3                               #
# Predicting firm exit                          #
# Feature engineering                           #
# Peter ENDES-NAGY                              #
#################################################
rm( list = ls() )
#libraries
library(tidyverse)
library(data.table)
library(modelsummary)

P95 <- function(x){quantile(x,0.95,na.rm=T)}
P99 <- function(x){quantile(x,0.99,na.rm=T)}
P999 <- function(x){quantile(x,0.999,na.rm=T)}
P05 <- function(x){quantile(x,0.05,na.rm=T)}
P01 <- function(x){quantile(x,0.01,na.rm=T)}
P001 <- function(x){quantile(x,0.001,na.rm=T)}


data <- readRDS( url("https://github.com/bmbln/DA3_projects/blob/main/Assignment_3/data/data_prep.RData?raw=true") )

# change some industry category codes
data <- data %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
  )

table(data$ind2_cat)

# Firm characteristics
##Age: 
ggplot( data = data, aes( x = age , y = as.numeric( high_growth ) ) ) +
  geom_smooth( method = "lm" , formula = y ~ poly( x , 2 ) , color = 'black' , se = F , size = 1  ) +
  geom_smooth( method = "loess" , se = F , colour = 'red' , size = 1 , span = 0.9 ) +
  ylim( 0 , 1 ) +
  labs( x = "Age in years" , 
        y = "HGC (probability)" ,
        title = "Modelling firm's age" , 
        subtitle = "Black: quadratic function \nRed: actual age variable") +
  theme_bw()
#should use quadratic form, although it overestimates the old end

data <- data %>%
  mutate( age2 = age^2,
          foreign_management = as.numeric( foreign >= 0.5 ),
          gender_m = factor( gender, levels = c( "female" , "male" , "mix" ) ) ,
          m_region_loc = factor( region_m , levels = c( "Central" , "East" , "West" ) ) )

# assets can't be negative. Change them to 0 and add a flag.
data <-data  %>%
  mutate( flag_asset_problem = ifelse( intang_assets < 0 | curr_assets < 0 | fixed_assets < 0 , 1 , 0   ) )

table( data$flag_asset_problem )
#not so severe, 13 problems

data <- data %>%
  mutate( intang_assets = ifelse( intang_assets < 0 , 0 , intang_assets ) ,
          curr_assets = ifelse( curr_assets < 0 , 0 , curr_assets ) ,
          fixed_assets = ifelse( fixed_assets < 0 , 0 , fixed_assets ) )

# generate total assets
data <- data %>%
  mutate( total_assets_bs = intang_assets + curr_assets + fixed_assets )
summary(data$total_assets_bs) # a lognormal distribution though, but not a problem as we take ratios

###ratios 
pl_names <- c( "extra_exp" , "extra_inc" , "extra_profit_loss" , "inc_bef_tax" , "inventories" , 
               "material_exp" , "profit_loss_year" , "personnel_exp" )
bs_names <- c( "intang_assets" , "curr_liab" , "fixed_assets" , "liq_assets" , "curr_assets" , 
               "share_eq" , "subscribed_cap" , "tang_assets" )

# divide all pl_names elements by sales and create new column for it
data <- data %>%
  mutate_at( vars( pl_names ), funs( "pl" = . / sales ) )

# divide all bs_names elements by total_assets_bs and create new column for it
data <- data %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))

########################################################################
# creating flags, and winsorizing tails
########################################################################

# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <- c( "extra_exp_pl" , "extra_inc_pl" , "inventories_pl" , "material_exp_pl" , "personnel_exp_pl" ,
           "curr_liab_bs" , "fixed_assets_bs" , "liq_assets_bs" , "curr_assets_bs" , "subscribed_cap_bs" ,
           "intang_assets_bs" )

data <- data %>%
  mutate_at( vars( zero ) , funs( "flag_high" = as.numeric( . > 1 ) ) ) %>%
  mutate_at( vars( zero ) , funs( ifelse( . > 1 , 1 , . ) ) ) %>%
  mutate_at( vars( zero ) , funs( "flag_error" = as.numeric( . < 0 ) ) ) %>%
  mutate_at( vars( zero ) , funs( ifelse( . < 0 , 0 , . ) ) )


# for vars that could be any, but are mostly between -1 and 1
any <-  c( "extra_profit_loss_pl" , "inc_bef_tax_pl" , "profit_loss_year_pl" , "share_eq_bs" )

data <- data %>%
  mutate_at( vars( any ) , funs( "flag_low" = as.numeric( . < -1 ) ) ) %>%
  mutate_at( vars( any ) , funs( ifelse( . < -1 , -1 , . ) ) ) %>%
  mutate_at( vars( any ) , funs( "flag_high" = as.numeric( . > 1 ) ) ) %>%
  mutate_at( vars( any ) , funs( ifelse( .> 1 , 1 , . ) ) ) %>%
  mutate_at( vars( any ) , funs( "flag_zero" = as.numeric( . == 0 ) ) ) %>%
  mutate_at( vars( any ) , funs( "quad" = .^2 ) )


########################################################################
# additional
# including some imputation
########################################################################

# CEO age
data <- data %>%
  mutate( ceo_age = year - birth_year ,
          flag_low_ceo_age = as.numeric( ceo_age < 25 & !is.na( ceo_age ) ) ,
          flag_high_ceo_age = as.numeric( ceo_age > 75 & !is.na( ceo_age ) ) ,
          flag_miss_ceo_age = as.numeric( is.na( ceo_age ) ) ,
          ceo_age = ifelse( ceo_age < 25 , 25, ceo_age ) %>% #winsorizing the tails
            ifelse( . > 75 , 75 , . ) %>%
            ifelse( is.na( . ) , mean( . , na.rm = TRUE ) , . ) ,
          ceo_young = as.numeric( ceo_age < 40 ) )

# number emp
summary(data$labor_avg)

# impute with mean values
data <- data %>%
  mutate( labor_avg_mod = ifelse( is.na( labor_avg ), mean( labor_avg , na.rm = TRUE ) , labor_avg ) ,
          flag_miss_labor_avg = as.numeric( is.na( labor_avg ) ) ) %>% 
  select( -labor_avg )

# create factors
data <- data %>%
  mutate (urban_m = factor( urban_m , levels = c( 1 , 2 , 3 ) ) ,
          ind2_cat = factor( ind2_cat , levels = sort( unique( data$ind2_cat ) ) ) )

#y as factor, as well. HGC stands for High Growth Company
data <- data %>%
  mutate( high_growth_f = factor( high_growth , levels = c( 0 , 1 ) ) %>%
            recode( . , `0` = 'not_HGC', `1` = "HGC") )

########################################################################
# sales 
########################################################################


#we are better off with modeling it with square
#for large companies, it's hard to grow avg 20% annually in 2 years.
ggplot( data = data, aes( x = sales_mil_log , y = high_growth ) ) +
  geom_point( size = .1 , fill = "blue" , color = "blue" ) +
  geom_smooth( method = "lm" , formula = y ~ x , color = 'yellow' , se = F , size = .5 ) +
  geom_smooth( method = "lm" , formula = y ~ poly( x , 2 ) , color = 'black' , se = F , size = .5 ) +
  geom_smooth( method = "loess" , se = F , colour = 'red' , size = .5 , span = 0.9 ) +
  labs( x = "Sales (log, million EUR)" , 
        y = "HGC" , 
        title = "Modelling sales" , 
        subtitle = "Black: quadratic function \nYellow: linear function \nRed: actual variable ") +
  theme_bw()

#quadratic form isn't necessary


########################################################################
# sales change
########################################################################
ggplot( data = data , aes(x = d1_sales_mil_log , y =  high_growth  ) ) +
  geom_point( size=0.1 , fill = 'blue' , color = 'blue' ) +
  geom_smooth( method = "loess" , se = F , colour = 'red' , size = 1 , span = 0.9 ) +
  labs( x = "Historical growth rate (Diff of ln sales)" , y = "HGC (future)" ) +
  theme_bw()

### For winsorizing, let's pick the top and bottom 1% for cut off. 
datasummary( d1_sales_mil_log ~ P001 + P01 + P05 + P95 + P99 + P999 , data = data )
data <- data %>% 
  mutate( flag_low_d1_sales_mil_log = ifelse(d1_sales_mil_log < P01(d1_sales_mil_log) , 1, 0),
          flag_high_d1_sales_mil_log = ifelse(d1_sales_mil_log > 1.5, 1, 0),
          d1_sales_mil_log_mod = ifelse( d1_sales_mil_log < P01(d1_sales_mil_log) , P01(d1_sales_mil_log) , 
                                         ifelse ( d1_sales_mil_log > P99(d1_sales_mil_log) , P99(d1_sales_mil_log) , d1_sales_mil_log ) ) ,
          d1_sales_mil_log_mod_2 = d1_sales_mil_log_mod^2 , 
          d1_sales_mil_log_mod_3 = d1_sales_mil_log_mod^3)

ggplot( data = data , aes(x = d1_sales_mil_log_mod , y = high_growth ) ) +
  geom_point( size=0.1 , fill = 'blue' , color = 'blue' ) +
  geom_smooth( method = "lm" , formula = y ~ poly( x , 3 ) , color = 'black' , se = F , size = .5 ) +
  geom_smooth( method = "loess" , se = F , colour = 'red' , size = .5 , span = 0.9 ) +
  labs( x = "Historical growth rate (Diff of ln sales)" , 
        y = "HGC (future)" ,
        title = "Modelling historical growth" , 
        subtitle = "Black: cubic function \nRed: actual variable ") +
  theme_bw()

#well captured with a 3rd order polynomial, so let's create new variables
data <- data %>% 
  mutate( d1_sales_mil_log_mod_2 = d1_sales_mil_log_mod^2 , 
          d1_sales_mil_log_mod_3 = d1_sales_mil_log_mod^3)

#check missing values
to_filter <- sapply( data , function(x) sum( is.na( x ) ) )
to_filter[ to_filter > 0 ]
# no more imputation, drop obs if key vars missing
data <- data %>%
  filter( !is.na( liq_assets_bs ) , 
          !is.na( foreign ) , 
          !is.na( ind ) , 
          !is.na( age ) , 
          !is.na( material_exp_pl ) , 
          !is.na( m_region_loc ) )

to_filter <- sapply( data , function(x) sum( is.na( x ) ) )
to_filter[ to_filter > 0 ]

#drop remaining variables that we won't use and has many missing values
data <- data %>%
  select( -c( 'D' , 'exit_year' , 'birth_year' , 'exit_date' ) )

to_filter <- sapply( data , function(x) sum( is.na( x ) ) )
to_filter[ to_filter > 0 ]

# drop unused factor levels
data <- data %>%
  mutate_at( vars( colnames( data )[ sapply( data , is.factor ) ] ) , funs( fct_drop ) )

# dropping flags with no variation, again to make sure it's clean and neat
variances <- data %>%
  select( contains( "flag" ) ) %>%
  apply( 2 , var , na.rm = TRUE ) == 0

sum(variances) #there are 7 such flags

data <- data %>%
  select( -one_of( names( variances )[ variances ] ) )

saveRDS( data , "~/Documents/CEU/DA3/DA3_projects/Assignment_3/data/data_engineered.RData" )

