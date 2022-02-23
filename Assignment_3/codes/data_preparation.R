#################################################
# Assignment 3                                  #
# Data Analysis 3                               #
# Predicting firm exit                          #
# Data preparation and sample design            #
# Peter ENDES-NAGY                              #
#################################################
rm( list = ls() )
#libraries
library(tidyverse)
library(data.table)
library(Hmisc)

data <- read_csv('https://osf.io/3qyut/download')

#check missing values
to_filter <- sapply( data , function(x) sum( is.na( x ) ) )
to_filter[ to_filter > 0 ]

# drop variables with many NAs, and filter for our panel (2010-2015)
data <- data %>%
  select( -c( COGS , finished_prod , net_dom_sales , net_exp_sales , wages ) ) %>%
  filter( year < 2016 , 
          year > 2009 )

# add all missing year and comp_id combinations -
# originally missing combinations will have NAs in all other columns
data <- data %>%
  complete( year , comp_id )


# generate status_alive; if sales larger than zero and not-NA, then firm is alive
data  <- data %>%
  mutate( status_alive = sales > 0 & !is.na( sales ) %>%
            as.numeric( . ) )

#Calculate annual growth (compared to previous years)
#calculate variable for high growth
#classic definition: average annual growth of 20% over a 3-year period in terms of turnover or employment
#my definition: average annual growth of 20% over a 2-year period in term of sales

summary(data$sales) 
#we have over 70k NA, some is going to be dropped if not new company

#calculate log sales (in million too)
data <- data %>%
  mutate( sales = ifelse( sales < 0 , 1 , sales ) ,
          ln_sales = ifelse( sales > 0 , log( sales ) , 0 ) ,
          sales_mil = sales/1000000 ,
          sales_mil_log = ifelse( sales > 0, log( sales_mil ) , 0 ) )

#calculate 1 year growth retrospectively as a predictor

data <- data %>%
  group_by( comp_id ) %>%
  mutate( d1_sales_mil_log = sales_mil_log - Lag( sales_mil_log , 1 ) ) %>%
  ungroup( )

# replace w 0 for new firms + add dummy to capture it
data <- data %>%
  mutate( age = ( year - founded_year ) %>%
            ifelse( . < 0 , 0 , . ) ,
          new = as.numeric( age <= 1 ) %>% #  (age could be 0 , 1 )
            ifelse( balsheet_notfullyear == 1 , 1 , . ) ,
          d1_sales_mil_log = ifelse( new == 1 , 0 , d1_sales_mil_log ) ,
          new = ifelse( is.na( d1_sales_mil_log ) , 1 , new ) ,
          d1_sales_mil_log = ifelse( is.na( d1_sales_mil_log ) , 0 , d1_sales_mil_log ) )


###TARGET VARIABLE
#Calculate average annual growth for the subsequent 2 years for the target variable using log
#2 years: log((x/y)^1/2) = 1/2 * ( log(x) - log(y) )
data <- data %>%
  group_by( comp_id ) %>%
  mutate( d2_sales_mil_log_future = ( lead( sales_mil_log , 2 ) - sales_mil_log ) / 2 ) %>%
  ungroup( ) %>% 
  # + calculate the yearly average growth from the log diff for the y
  mutate( growth_sales_2y = exp( d2_sales_mil_log_future ) ) %>% 
  select( -d2_sales_mil_log_future ) %>% 
  #create y dummy variable: average annual growth above 20% in the next 2 years
  mutate( high_growth = ifelse( growth_sales_2y >= 1.2 , 1 , ifelse( is.na( growth_sales_2y ) , NA , 0 ) ) ) #%>% 
  select( -growth_sales_2y )
  

###########################################################
# sample design
###########################################################


# our cross section: year of 2012, the company is alive (so it has room for growth), y variable NOT missing 
data <- data %>%
  filter( ( year == 2012 ) & ( status_alive == 1 ) & !is.na( high_growth ) ) 

#a nice lognormal distribution, the tails are sprawling
ggplot( data ) + 
  geom_density( aes( ln_sales ) )

# let's work with firms below 10m euro revenues and above 50k euros.
# above 10m not SME anymore, under 50k eur way too small and too easy to grow from there looking high
# in the HGC literature, it is recommended to cut out microenterprises under 10 employees, less strict here. 

data <- data %>%
  filter(!(sales_mil > 10)) %>%
  filter(!(sales_mil < 0.05))

#saveRDS( data , "~/Documents/CEU/DA3/DA3_projects/Assignment_3/data/data_prep.RData" )

