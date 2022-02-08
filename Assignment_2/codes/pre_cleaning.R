#################################################
# Assignment 2                                  #
# Data Analysis 3                               #
# Predicting AirBnB apartment prices in Paris   #
# Peter ENDES-NAGY                              #
#                                               #
# Preliminary cleaning of the very raw dataset  #
#################################################

##SET UP
rm( list = ls() )
#libraries
library(tidyverse)

#1) Load and clean raw data 
##locally
listings <- as.data.frame ( fread("~/Documents/CEU/DA3/DA3_projects/Assignment_2/data/raw/listings.csv.gz") )
### the .gz can't be read from GitHub directly, keeps giving back a HTTP error 404:
# Error in curl::curl_download(input, tmpFile, mode = "wb", quiet = !showProgress) : 
# HTTP error 404.
### the original file saved into a .csv is way exceeding 100MB, get's refused by GitHub.


#drop unnecessary/redundant/meaningless variables 
to_drop <- c( "listing_url" , "scrape_id" , "name" , "description" , "neighborhood_overview" , "picture_url" , 
              "host_id" , "host_url" , "host_name" , "host_location" , "host_about" , "host_thumbnail_url" , "host_picture_url" , "host_neighbourhood" , "host_total_listings_count" , "host_verifications" ,
              "neighbourhood" , "neighbourhood_group_cleansed" , 
              "latitude" , "longitude" ,
              "minimum_minimum_nights" , "maximum_minimum_nights" , "minimum_maximum_nights" , "maximum_maximum_nights" , 
              "minimum_nights_avg_ntm" , "maximum_nights_avg_ntm" , 
              "calendar_updated" , "calendar_last_scraped" , "last_review" , "license" , 
              "calculated_host_listings_count" , "calculated_host_listings_count_entire_homes" , "calculated_host_listings_count_private_rooms" , "calculated_host_listings_count_shared_rooms")
listings <- listings %>%
  select( -one_of( to_drop ) )

## I. Dealing with amenities
#I.STEP1
#parse through the whole dataframe and save the cleaned amenities into a list
amenities <- list()
for (i in 1:length(listings$amenities)) {
  amenities[[i]] <- gsub( " " , "_" , tolower ( trimws( str_replace_all(  unlist( str_split( listings$amenities[i] , "," ) ) , "[[:punct:]]", "" ) ) ) , fixed = T ) 
}
#save them into the dataframe
listings$amenities_clean <- amenities

#I.STEP2
#create a df that contains the list of amenities and their occurencies
#we have 1010 type of ammenities, for the sake of simplicity, we keep only those that have at least 1000 occurence (out of 61k observations)
amenities_occurence <- data.frame( amenity = unlist( amenities ) ) %>% 
  group_by( amenity ) %>% 
  summarize( n = n()) %>% 
  filter( n > 1000)
#this could be further refined, as well as hunting for similar amenities with different wordings, but this is enough for now. Still 73 type of amenities with more than 1000 observations.  

#I.STEP3
#now let's loop through the cleaned amenities and check which of the 73 amenities are marked for that particular listing
amenities_table <- list()
for (i in 1:length(amenities)) {
  amenities_table[[i]] <- as.list( amenities_occurence$amenity %in% amenities[[i]] )
}
#rbind it
amenities_table <- rbindlist( amenities_table )
#create new variable names for the amenities so we can recognize them later
amenities_varnames <- paste( "am_" , amenities_occurence$amenity , sep = "")
#rename the variables as they are called (73 different names)
names(amenities_table) <- amenities_varnames

#I.STEP4
#merge with the listings dataset
listings <- cbind( listings , amenities_table)
#remove the original aminites columns 
listings <- select( listings , -c( "amenities" , "amenities_clean") )

## II. Fixing datatypes 
#II.1. Transform "N/A" into NA in character type variables 
listings[ , sapply( listings , class) == 'character'][ listings[ , sapply( listings , class ) == 'character'] == "N/A"] <- NA

#II.2. Binary variables
binary_varnames <- c( "host_is_superhost" , "host_has_profile_pic" , "host_identity_verified" , 
                      "has_availability" , "instant_bookable" )

#transform them into 1 or 0 and NA
for (i in binary_varnames) {
  listings <- listings %>% 
    mutate( !!sym(i) := ifelse( !!sym(i) == "t" , 1 , ifelse( !!sym(i) == "f" , 0 , NA ) ) )
}
#the amenities as well! 
listings[amenities_varnames] <-  sapply( listings[amenities_varnames] , as.numeric )

#II.3. Percentage variables
perc_varnames <- c( "host_response_rate" , "host_acceptance_rate" )
#remove % and force transformation to numeric (NA is going to be coerced)
for (i in perc_varnames) {
  listings <- listings %>% 
    mutate( !!sym(i) := as.numeric( gsub( "%" , "" , !!sym(i) ) ) )
}

#II.4. Fix bathroom numbers (missing in bathrooms column, contained by the text)
listings <- listings %>% 
  mutate( bathrooms = ifelse( nchar(bathrooms_text) == 0 , 0 , NA ) ) %>% #if the length is 0, then it's missing. let's store it in a useless variable
  separate( col = bathrooms_text , into = "n_bathrooms" , sep = " ") %>% #separate by first whitespace
  mutate( n_bathrooms = as.numeric( n_bathrooms ) ) %>% #transform into numeric
  mutate( n_bathrooms = ifelse( is.na( n_bathrooms ) , 1 , n_bathrooms) ) %>% #sometimes the first word is something else than a number but NA was coerced - fix with 1
  mutate( n_bathrooms = ifelse ( is.na( bathrooms ),  n_bathrooms , NA ) ) %>% #NA also got a value of 1 in previous step, first step helps with real NA
  select( -bathrooms ) #let's remove the technical, redundant variable

#II.4. Fix price variable (stored as a text, devise incl)
listings <- listings %>% 
  mutate( price = as.numeric( gsub( "," , "" , gsub( "\\$" , "" , price ) ) ) ) #replace dollar sings and thousand separators


##SAVE THE SEMI-RAW DATASET
#write_csv( listings , "~/Documents/CEU/DA3/DA3_projects/Assignment_2/data/raw/listings_for_analysis.csv")

