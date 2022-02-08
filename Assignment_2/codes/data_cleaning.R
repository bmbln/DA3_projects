#################################################
# Assignment 2                                  #
# Data Analysis 3                               #
# Predicting AirBnB apartment prices in Paris   #
# Peter ENDES-NAGY                              #
#                                               #
# Data cleaning and feature engineering         #
#################################################

##SET UP
rm( list = ls() )
#libraries
library(tidyverse)
library(data.table)
library(modelsummary)

P95 <- function(x){quantile(x,0.95,na.rm=T)}
P99 <- function(x){quantile(x,0.99,na.rm=T)}
P999 <- function(x){quantile(x,0.999,na.rm=T)}
P05 <- function(x){quantile(x,0.05,na.rm=T)}
Range <- function(x){max(x,na.rm=T)-min(x,na.rm=T)}

##################
##LOAD AND CLEAN##
##################
#load data
data <- read_csv( "https://raw.githubusercontent.com/bmbln/DA3_projects/main/Assignment_2/data/raw/listings_for_analysis.csv?token=GHSAT0AAAAAABORYEVOKZMORQIHO24A3OMUYQLLBTA") %>% 
  mutate_if(is.character, factor)

########################
#I. Feature engineering
glimpse(data)

#I.1. Missing values
to_filter <- sapply( data , function(x) sum( is.na( x ) ) )
to_filter[ to_filter > 0 ]

#A) Target variable: price. No missing value, but there are 0 values -> clearly data problem. 
length( filter( data , price == 0 ) ) #only 111 observations, so let's drop
data <- filter( data , price > 0 )

#B) Drop variable: too many missing and not that important
#host acceptance, response time and rate not that important
#we have many review metrics, strongly correlated, the total score is enough for us
to_drop <- c( "host_response_time" , "host_response_rate" , "host_acceptance_rate" , 
              "review_scores_accuracy" , "review_scores_cleanliness" , "review_scores_checkin" , "review_scores_communication" , "review_scores_location" , "review_scores_value" )
data <- data %>%
  select(-one_of(to_drop))

to_filter <- sapply( data , function(x) sum( is.na( x ) ) )
to_filter[ to_filter > 0 ]

#C) the 46 host data is almost consistently missing. 
as.data.table(data)[ is.na(host_identity_verified) ]
#flag with one single variable as f_hostdata_missing
#host_listings_count let's assume only 1
#for the rest, let's assume it is false or 0 (no superhost, no picture, etc.)
data <- data %>% 
  mutate( f_hostdata_missing = ifelse( is.na( host_identity_verified ) , 1 , 0 ) , 
          host_listings_count = ifelse( is.na( host_listings_count) , 1 , host_listings_count ) , 
          host_is_superhost = ifelse( is.na( host_is_superhost ) , 0 , host_is_superhost ) ,
          host_has_profile_pic = ifelse( is.na( host_has_profile_pic ) , 0 , host_has_profile_pic ) ,
          host_identity_verified = ifelse( is.na( host_identity_verified ) , 0 , host_identity_verified ) ,) 

to_filter <- sapply( data , function(x) sum( is.na( x ) ) )
to_filter[ to_filter > 0 ]

#D) bathrooms and bedrooms
data <- data %>% 
  mutate( n_bathrooms = ifelse( is.na( n_bathrooms ) , median( n_bathrooms , na.rm = T ) , n_bathrooms ) , # replace with median
          beds = ifelse( is.na( beds ) , as.integer( round( accommodates/2 ) ) , beds ) , #assume double beds, so based on accommodates variable divide by to and round it up
          bedrooms = ifelse( is.na( bedrooms ) , beds , bedrooms ) ) #assume one bedroom per beds

to_filter <- sapply( data , function(x) sum( is.na( x ) ) )
to_filter[ to_filter > 0 ]

#E) Missing review data
#score replaced with median and flag created
#reviews_per_month is missing where no review - impute with 0 
#first_review imputed with host_since if missing. If both, then with last_scraped by assuming it is a fresh listing

data <- data %>% 
  mutate( f_review_scores_rating = ifelse( is.na( review_scores_rating ) , 1 , 0 ) ,
          review_scores_rating = ifelse( is.na( review_scores_rating ) , median( review_scores_rating , na.rm = T ) , review_scores_rating ) , 
          reviews_per_month = ifelse( is.na( reviews_per_month ) , 0 , reviews_per_month ) , 
          first_review = as.Date( ifelse( is.na( first_review ) , ifelse( 
            is.na (host_since) , last_scraped , host_since) , first_review ) ,  origin='1970-01-01' ) ) %>% 
  select( -host_since )

to_filter <- sapply( data , function(x) sum( is.na( x ) ) )
to_filter[ to_filter > 0 ]

#I.2. Create and transform variables
#A) Price: log or level? Extreme values?
datasummary( price ~ Mean + Median + Min + Max + Range + P05 + P95 + P99 + P999 , data = data )
ggplot( data , aes( price ) ) + 
  geom_histogram()
#there are some very extreme values, some are totally unrealistc. 
#the upper 0.1% starts from 2000, that is similar to very high end luxury hotel prices, so reasonable for entire luxury flat
#let's drop above 2000
data <- filter( data , price < 2000 )
ggplot( data , aes( price ) ) + 
  geom_histogram()
#lognormal, so log transformation needed
data$ln_price <- log( data$price )

#nice lognormal plot
ggplot( data , aes( ln_price ) ) + 
  geom_histogram()
  
#B) days elapsed variable creation
data$days_since <- as.integer( difftime( data$last_scraped , data$first_review , units = "days") , units = "days")
#remove the date variables
data <- select( data , -c( "last_scraped" , "first_review" ))

#C) property and room type
#first let's filter out hotel rooms 
data <- filter( data , room_type != "Hotel room")
datasummary(property_type ~ N + Percent(), data = data )
#property_type is messy and also contains the room type information. 
data <- data %>% 
  mutate( property_type = gsub( "shared room in |room in |private room in |entire |private " , "" , tolower( property_type ) ) ) %>% #let's remove the room_type information
  filter( property_type %in% c("apartment", "condominium", "loft" , "house" , "townhouse" , "villa" ) ) %>% #keep different forms of apartments and houses
  mutate( property_type = as.factor( ifelse( property_type %in% c("apartment", "condominium", "loft" ) , "Apartment" , "House" ) ) ) #pool the different but similar types into one
#shorten the room_type
datasummary(room_type ~ N + Percent(), data = data )
data$room_type <- as.factor( ifelse( data$room_type == "Entire home/apt" , "Entire" , 
                                   ifelse( data$room_type == "Private room" , "Private" , 
                                          ifelse( data$room_type == "Shared room" , "Shared" , ".") ) ) )


#D) Pool variables
#host listing count: 1, 2-5 and above 5 listings
data <- data %>% 
  mutate( host_listings_count = as.factor( ifelse( 
    host_listings_count == 0 , "1" , ifelse( #0 is clearly a wrong data as the host is listing something. let's assume it is 1
      host_listings_count == 1 , "1" , ifelse(
        host_listings_count < 6 , "2-5" , "5+"
      ) ) ) ) )

#n_bathrooms: 0 , 1 , and above
data <- data %>% 
  mutate( d_bathrooms = as.factor( ifelse( 
    n_bathrooms == 0 , "0" , ifelse(
      n_bathrooms == 1 , "1" , "1+" ) ) ) ) %>% 
  select( -n_bathrooms )

#number_of_reviews: 0 , 1-50 , 50+
data <- data %>% 
  mutate( number_of_reviews = as.factor( ifelse( 
    number_of_reviews == 0 , "0" , ifelse(
      number_of_reviews < 51 , "1-50" , "50+" ) ) ) )

#minimum_nights: 1 , 2-5 , 6-29 , 30+
data <- data %>% 
  mutate( minimum_nights = as.factor( ifelse( 
    minimum_nights == 1 , "1" , ifelse(
      minimum_nights < 6 , "2-5" , ifelse(
        minimum_nights < 30 , "6-29" , "30+"
      ) ) ) ) )

#there was at least 1 review in ltm or l30d
data <- data %>% 
  mutate( d_review_ltm = ifelse( number_of_reviews_ltm > 0 , 1 , 0 ) , 
          d_review_l30d = ifelse( number_of_reviews_l30d > 0 , 1 , 0 ) ) %>% 
  select( -c( "number_of_reviews_ltm" , "number_of_reviews_l30d" ) )

#separate bedrooms might be a good predictable (more beds than bedrooms, beds in living rooms are 0 ): bedrooms/beds
#we won't need the beds variable like that
#if beds number 0 (probably a couch offered), missing value - assign 0 to sep_bedroom
data <- data %>% 
  mutate( sep_bedroom = ifelse( bedrooms/beds < 1 , 0 , 1 ) ) %>% 
  mutate( sep_bedroom = ifelse( is.na( sep_bedroom) , 0 , sep_bedroom ) ) %>% 
  select( -beds)

#drop some extra unnecessary variables still kept
to_drop <- c( "maximum_nights" , "id" )
data <- data %>%
  select( -one_of( to_drop ) )

#I.3. Business case related filtering 
#client is interested in renting out 2-6 mid-size apartments
#let's filter out accommodates below 2 and above 6 
data <- filter( data , accommodates < 7 & accommodates > 1 )

#I.4. rename the variables by type so we can easily work with them 
#dummies (without amenities that are already am_something)
dnames <- c( "host_is_superhost" , "host_has_profile_pic" , "host_identity_verified" , "has_availability" , "instant_bookable" , "sep_bedroom" )
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0( "d_", dnames)

#numerical
nnames <- c( "accommodates" , "bedrooms" , "price" , 
             "availability_30" , "availability_60" , "availability_90" , "availability_365" , 
             "review_scores_rating" , "reviews_per_month" , 
             "days_since" )
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0( "n_", nnames)

#factors (not as dummies)
facnames <- c( "host_listings_count" , "neighbourhood_cleansed" , 
               "property_type" , "room_type" , 
               "minimum_nights" , "number_of_reviews" )
facnames_i <- match(facnames, colnames(data))
colnames(data)[facnames_i] <- paste0( "fac_", facnames)

#I.4.Add log and quadratic variables for some numeric 
data <- data %>%
  mutate(
    ln_days_since = log( n_days_since + 1 ) ,
    ln_days_since2 = log( n_days_since + 1 )^2 ,
    ln_days_since3 = log( n_days_since + 1 )^3 ,
    n_days_since2 = n_days_since^2 ,
    n_days_since3 = n_days_since^3 , 
    n_accommodates2 = n_accommodates^2 , 
    n_accommodates3 = n_accommodates^3 , 
    n_bedrooms2 = n_bedrooms^2 , 
    n_bedrooms3 = n_bedrooms^3 , )

##write cleaned dataset into a csv file 
#write_csv( data , "~/Documents/CEU/DA3/DA3_projects/Assignment_2/data/cleaned/paris_listings_clean.csv")


