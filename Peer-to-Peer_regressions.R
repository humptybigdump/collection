# import libraries
library("dplyr")
library("igraph")
library("tidyverse")
library("leaflet")
library("magrittr")
library("stargazer")

# STEP 1: Load and preprocess Data
#-------------------------------------------------------------------------------------

# TODO:  read csv-file
listings = read.csv("listings_amsterdam.csv", sep = ",", stringsAsFactors = FALSE)

#-------------------------------------------------------------------------------------

# TODO: generate column "hostInfo" with:
#                               1: host has a profile description
#                               0: else
listings$hostInfo = ifelse(listings$host_about != "", 1, 0)

# delete column "host_about"
listings$host_about = NULL

#-------------------------------------------------------------------------------------

# TODO: generate a column "priceValue" with each price as a numeric value (without "$") 
# Hint: use the regular expression "[^0-9\\.]"
listings$priceValue = as.numeric(gsub("[^0-9\\.]", "", listings$price))

# TODO: delete column "price"
listings$price = NULL

#-------------------------------------------------------------------------------------

# TODO: generate a column "superhost" with:
#                               1: host is a superhost
#                               0: else
listings$superhost = ifelse(listings$host_is_superhost == "t", 1, 0)

# TODO: delete column "host_is_superhost"
listings$host_is_superhost = NULL

#-------------------------------------------------------------------------------------

# TODO: generate a column "professionalhost" with:
#                               1: host offers more than 1 listing on Airbnb
#                               0: else

listings$professionalhost = ifelse(listings$host_total_listings_count > 1, 1, 0)

# TODO: delete column "host_total_listings_count"
listings$host_total_listings_count = NULL

#-------------------------------------------------------------------------------------

# TODO: generate a column "entireapt" with:
#                               1: listing is an entire apartment
#                               0: else

listings$entireapt = ifelse(listings$room_type == "Entire home/apt", 1 , 0)

# TODO: delete column "room_type"
listings$room_type = NULL

#-------------------------------------------------------------------------------------

# TODO: generate a column "instantbook" with:
#                               1: listing is instant bookable
#                               0: else

listings$instantbook = ifelse(listings$instant_bookable == "t", 1, 0)

# TODO: delete column "instant_bookable"
listings$instant_bookable = NULL

#-------------------------------------------------------------------------------------

# TODO: delete all rows where priceValue = 0

listings = listings %>% filter(priceValue > 0)

#-------------------------------------------------------------------------------------

# TODO: calculate len and lat of Amsterdam's centre

lat_centre = mean(listings$latitude)
lng_centre = mean(listings$longitude)

# TODO: calculate for each listing its distance to the city centre using the Haversine formula (https://en.wikipedia.org/wiki/Haversine_formula)

distance = function (lat1, lng1, lat2, lng2) {
  r_earth = 6371.00 # in KM
  rad = pi/180
  a1 = lat1 * rad
  a2 = lng1 * rad
  b1 = lat2 * rad
  b2 = lng2 * rad
  dlng = b2 - a2
  dlat = b1 - a1
  a = (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlng/2))^2
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  d = r_earth * c
  return(d)
}

listings$distance = distance(listings$latitude, listings$longitude, lat_centre, lng_centre)

#-------------------------------------------------------------------------------------

# TODO: Plot the distribution of prices,
hist(listings$priceValue, breaks=200)

x = listings %>% filter(priceValue<=1000) %>% select(priceValue)
hist(x$priceValue, breaks=100)

#-------------------------------------------------------------------------------------

# TODO: ... rating scores
hist(listings$review_scores_rating)

#-------------------------------------------------------------------------------------

# TODO: ... and number of ratings
hist(listings$number_of_reviews)

#-------------------------------------------------------------------------------------

# STEP 2: Apply regression models

# TODO: Apply a hedonic price regression to determine the influence of
#
#       Listing attributes:
#       - accommodation size
#       - entire apartment
#       - distance to city centre
#
#       Host attributes:
#       - host's profile description
#       - Superhost status
#       - number of reviews
#       - review score
#
#       on listing price using regression analysis

# Regression model with only listing attributes
reg1 = lm(data = listings, priceValue ~ accommodates + entireapt + distance)

# Regression model with listing attributes and host attributes
reg2 = lm(data = listings, priceValue ~ accommodates + entireapt + distance + hostInfo + superhost + professionalhost + number_of_reviews + review_scores_rating)

# print regression tables to html file
stargazer(reg1, reg2, type="html", out="regtable1.html", initial.zero = FALSE, star.cutoffs = c(0.05, 0.01, 0.001))


reg3 = lm(data = listings, priceValue ~ accommodates + entireapt + distance + hostInfo + superhost + professionalhost + number_of_reviews * review_scores_rating)

# print regression tables to html file
stargazer(reg1, reg2, reg3, type="html", out="regtable2.html", initial.zero = FALSE, star.cutoffs = c(0.05, 0.01, 0.001))

