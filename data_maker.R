rm(list = ls())

library(tidyverse)

#################################################################
### create sales data ###########################################
#################################################################

####################
# general framing, products, mapping, time
####################

## products
brand <- sprintf("%02d",c(1:9))
brand <- paste0("brand",brand)

## create mapping
region <- paste0("region",sprintf("%02d",c(1:24)))

salesMap <- data.frame(region)
salesMap$zone <- NA

cut1 <- round(.3*nrow(salesMap),0)
cut2 <- round(.5*nrow(salesMap),0)
cut3 <- round(.65*nrow(salesMap),0)
cut4 <- round(.75*nrow(salesMap),0)
cut5 <- nrow(salesMap)

salesMap$zone[1:cut1] <- "central"
salesMap$zone[(cut1 + 1) : cut2] <- "north"
salesMap$zone[(cut2 + 1) : cut3] <- "south"
salesMap$zone[(cut3 + 1) : cut4] <- "east"
salesMap$zone[(cut4 + 1) : cut5] <- "west"

## combine brands and mapping. Make it so some brands are not available everywhere.
brand_map <- data.frame()

set.seed(42)
brandDraws <- runif(length(brand))
for (b in 1:length(brand)) {
  brandB <- brand[b]
  brandD <- brandDraws[b]
  
  temp <- salesMap
  
  if(brandD > 0.6) {
    temp <- temp %>%
      sample_frac(.8*(brandD), replace = F)
  } 
  
  if(brandD >0.9 & brandD < 0.95) {
    temp <- subset(temp, zone != "west")
  }
  if(brandD >0.93) {
    temp <- subset(temp, zone != "north")
  }
  
  temp$brand <- brandB
  
  brand_map <- rbind(brand_map,temp)
}

# calendar
year <- 2018:2024
month <- 1:12
year_month <- expand.grid(year,month)

brand_time_map <- data.frame()

for(i in 1:nrow(year_month)) {
  y = year_month$Var1[i]
  m <- year_month$Var2[i]
  
  temp <- brand_map
  temp$year <- y
  temp$month <- m
  
  brand_time_map <- rbind(brand_time_map,temp)
}



#customers - year variation. I want repeat customers, but not always there.  
num_customer <- 123 # 50 customers
customer_id <- c(1:num_customer) # 50 customer IDs
yqc_replicated <- replicate(num_customer, year_month, simplify = FALSE)
yqc <- do.call(rbind, Map(cbind, yqc_replicated, customer_id = customer_id))

yqc <- yqc %>% rename(year = Var1, 
                      month = Var2)

yqc <- yqc %>% sample_frac(0.4, replace = FALSE)

# assign brands to different customers and different times
brands <- unique(brand_time_map$brand)
# I realize this is really hand coded. Sorry. But I am lazy. 
probabilities <- c(0.03, 0.09, 0.15, 0.18, 0.27, 0.1, 0.1, 0.05, 0.03)

# Assuming 'yqc' is your data frame
yqc$brand <- sample(brands, size = nrow(yqc), replace = TRUE, prob = probabilities)

# add customers to other data
# don't really care if duplicates are created. I will allow customers to make multiple purchases of one or multiple products. 
# this is fake data. I'll do what I want!
brand_time_map_cust <- 
  left_join(brand_time_map,yqc, 
            by = c("month","year","brand"), multiple = "all")


# list prices
# first get base price per brand. Then I will mess with it. 
brand

brand_num <- round(rnorm(length(brand),400,170),0)

brand_base_prices <- data.frame(brand = brand, price0 = brand_num)

brand_map_prices <- full_join(brand_base_prices,brand_map, by = "brand", multiple = "all")

# now get some variation by geography
zone_ratios <- data.frame(
  zone = unique(brand_map$zone),
  zone_effect = runif(length(unique(brand_map$zone)), min = 0.8, max = 1.2))

region_ratios <- data.frame(
  region = region,
  region_effect = rnorm(length(region),1,.1))

brand_map_prices <- left_join(brand_map_prices, zone_ratios, by = "zone")
brand_map_prices <- left_join(brand_map_prices, region_ratios, by = "region")

brand_map_prices <- brand_map_prices %>%
  mutate(price = price0*zone_effect*region_effect) %>%
  select(brand,zone,region,price) %>%
  arrange(brand,zone,region)

# now some time trends
time <- year_month %>%
  rename(year = Var1,
         month = Var2) %>%
  arrange(year,month)

time$time1 <- seq(1,nrow(time),1) - round(0.3*nrow(time),0) # made sequence off center for polynomial expansion to be more interesting. Not important though.
time$time2 <- time$time1^2
time$time3 <- time$time1^3
time$time4 <- time$time1^4

time <- time %>%
  mutate(trend = 
           0.9*time1 + 
           .2*time2 + 
           0.001*time3 + 
           0.0008*time4) %>%
  select(year,month,trend)

brand_time_map <- left_join(brand_time_map,
                            time, 
                            by = c("year","month"))

brand_time_map_prices <- full_join(brand_map_prices,
                                   brand_time_map, 
                                   by = c("brand","zone","region"), multiple = "all")

brand_time_map_prices$trend <- brand_time_map_prices$trend * runif(nrow(brand_time_map_prices),min = 0.9,max = 1.1)

brand_time_map_prices$price <- brand_time_map_prices$price + brand_time_map_prices$trend

brand_time_map_prices$trend = NULL

brand_time_map_prices <- brand_time_map_prices %>%
  group_by(brand, year, month, zone) %>%
  mutate(list_price = 1.06*max(price)) %>%
  rename(sales_price = price) %>%
  ungroup()

#volume
# start with a base volume for each customer-brand

cb <- unique(brand_time_map_cust[,c("brand","customer_id")])
cb$base_volume <- round(runif(nrow(cb),-500,1000))
cb$base_volume[cb$base_volume < 0] <- 0

# add map variation
cb_map <- full_join(cb, brand_map, by = "brand", multiple = "all")
cb_map$shifter <- runif(nrow(cb_map),.7,2.3)
cb_map$base_volume <- cb_map$base_volume*cb_map$shifter
cb_map$shifter <- NULL

cb_map$d_shift1 <- runif(nrow(cb_map),.8,3)
cb_map$d_shift2 <- rnorm(nrow(cb_map),.02,.006)
cb_map$d_shift3 <- rnorm(nrow(cb_map),.00006,.000001)
cb_map$d_shift4 <- rnorm(nrow(cb_map),.0,.00000003)


# now that we have bases, add price impacts. price impacts are already varying by time, so no time effect needed
# bring together customers and prices

my_data <- full_join(brand_time_map_cust,
                     brand_time_map_prices,
                     by = intersect(names(brand_time_map_cust),names(brand_time_map_prices)))

# bring in base volumes.

my_data <- left_join(my_data, 
                     cb_map,
                     by = intersect(names(my_data),names(cb_map)),
                     multiple = "all")

# nonlinear demand

my_data <- my_data %>%
  group_by(brand,zone,year) %>%
  mutate(volume0 = mean(base_volume),
         volume_diff = base_volume - volume0,
         volume = abs( base_volume + 
                         volume_diff*d_shift1 -
                         volume_diff^2*d_shift2 +
                         volume_diff^3*d_shift3 +
                         volume_diff^4*d_shift4),
         discount = (sales_price - list_price)/list_price) %>%
  ungroup() %>%
  select(year, month, brand, zone, region, customer_id,
         list_price, discount, sales_price, volume)

# add days, dates
my_data$day <- ceiling(runif(nrow(my_data),0,31))
my_data$day[(my_data$month %in% c(4,6,9,11) & my_data$day == 31) |
              (my_data$month ==2 & my_data$day > 28)] <- 
  my_data$day[(my_data$month %in% c(4,6,9,11) & my_data$day == 31) |
                (my_data$month ==2 & my_data$day > 28)] - 12

my_data$date_string <- paste0(my_data$year,"-",sprintf("%02d", my_data$month),"-", sprintf("%02d", my_data$day))


write.csv(my_data,"~/DS Videos/multi video series/true_project_data.csv", row.names = F)


## now lets add some real world charm to the data

# missing volumes
selection <- round(runif(179,1,nrow(my_data)),0)
my_data$volume[unique(selection)] <- NA

# outliers
rows <- round(runif(round(.0009*nrow(my_data),0), 1, nrow(my_data)),0)
my_data$volume[rows] <- my_data$volume[rows] ^ 2

# commas in volume, just to be annoying
my_data$volume <- formatC(my_data$volume, format = "f", digits = 0, big.mark = ",")


# multiple cases for brands and zones and regions
my_data$brand[my_data$year %in% c(2020, 2024)] <- toupper(my_data$brand[my_data$year %in% c(2020, 2024)])
my_data$region[my_data$year %in% c(2020, 2024)] <- toupper(my_data$region[my_data$year %in% c(2020, 2024)])
my_data$zone[my_data$year %in% c(2020, 2024)] <- toupper(my_data$zone[my_data$year %in% c(2020, 2024)])

#Annoying prefixes
my_data$brand[my_data$year %in% c(2021,2022)] <- paste0("br code: ",my_data$brand[my_data$year %in% c(2021,2022)])
my_data$region[my_data$year %in% c(2021,2022)] <- paste0("# ",my_data$region[my_data$year %in% c(2021,2022)])
my_data$customer_id[my_data$zone == "south"] <- paste0("CID_",my_data$customer_id[my_data$zone == "south"])

# misspell some regions
rows <- round(runif(10, 1, nrow(my_data)),0)
my_data$region[rows] <- gsub("r","rr",my_data$region[rows])

rows <- round(runif(6, 1, nrow(my_data)),0)
my_data$region[rows] <- gsub("region","regoin",my_data$region[rows])

rows <- round(runif(1034, 1, nrow(my_data)),0)
my_data$region[rows] <- gsub("region","region ",my_data$region[rows])

# misspell some brands
rows <- round(runif(103, 1, nrow(my_data)),0)
my_data$brand[rows] <- gsub("brand","# brnd",my_data$brand[rows])

rows <- round(runif(34, 1, nrow(my_data)),0)
my_data$brand[rows] <- gsub("brand","branD",my_data$brand[rows])

rows <- round(runif(3034, 1, nrow(my_data)),0)
my_data$brand[rows] <- gsub("brand","brand ",my_data$brand[rows])



# missing prices
rows <- round(runif(round(.07*nrow(my_data),0), 1, nrow(my_data)),0)
my_data$list_price[rows] <- NA
my_data$sales_price[rows] <- NA
my_data$discount <- NA

# lazy reporting in the east
my_data$discount[my_data$zone == "east"] <- NA

# screw up dates
# my_data$date_string <- as.Date(my_data$date_string)
my_data$date_string <- ymd(my_data$date_string)

my_data$date_string[my_data$year == 2020] <- gsub("2020","20",my_data$date_string[my_data$year == 2020])

my_data$date_string <- as.character(my_data$date_string)
my_data$date_string[my_data$year == 2022] <- gsub("2022-","",my_data$date_string[my_data$year == 2022])
my_data$date_string[my_data$year == 2022] <- paste0(my_data$date_string[my_data$year == 2022],"-2022")

# price outlier
rows <- round(runif(round(.004*nrow(my_data),0), 1, nrow(my_data)),0)

my_data$sales_price[rows] <- my_data$sales_price[rows]^1.6

write.csv(my_data,"~/DS Videos/multi video series/project_data.csv", row.names = F)
