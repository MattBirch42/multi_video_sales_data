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
brandMap <- data.frame()

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
  
  brandMap <- rbind(brandMap,temp)
}

brandMap$segment <- NA
brandMap$segment[brandMap$brand %in% c("brand01","brand02","brand06")] <- "A"
brandMap$segment[brandMap$brand %in% c("brand08","brand09")] <- "B"
brandMap$segment[is.na(brandMap$segment) ] <- "C"

unique(brandMap$brand[brandMap$segment == "A"])

# calendar
year <- 2018:2024
month <- 1:12
yearMonth <- expand.grid(year,month)

brandTimeMap <- data.frame()

for(i in 1:nrow(yearMonth)) {
  y = yearMonth$Var1[i]
  m <- yearMonth$Var2[i]
  
  temp <- brandMap
  temp$year <- y
  temp$month <- m
  
  brandTimeMap <- rbind(brandTimeMap,temp)
}



#customers - year variation. I want repeat customers, but not always there.  
numCustomer <- 123 # 50 customers
customerID <- c(1:numCustomer) # 50 customer IDs
yqcReplicated <- replicate(numCustomer, yearMonth, simplify = FALSE)
yqc <- do.call(rbind, Map(cbind, yqcReplicated, customerID = customerID))

yqc <- yqc %>% rename(year = Var1, 
                      month = Var2)

yqc <- yqc %>% sample_frac(0.4, replace = FALSE)

# assign brands to different customers and different times
brands <- unique(brandTimeMap$brand)
# I realize this is really hand coded. Sorry. But I am lazy. 
probabilities <- c(0.03, 0.09, 0.15, 0.18, 0.27, 0.1, 0.1, 0.05, 0.03)

# Assuming 'yqc' is your data frame
yqc$brand <- sample(brands, size = nrow(yqc), replace = TRUE, prob = probabilities)

# add customers to other data
# don't really care if duplicates are created. I will allow customers to make multiple purchases of one or multiple products. 
# this is fake data. I'll do what I want!
brandTimeMapCust <- 
  left_join(brandTimeMap,yqc, 
            by = c("month","year","brand"), multiple = "all")


# list prices
# first get base price per brand. Then I will mess with it. 
brand

brandNum <- round(rnorm(length(brand),400,170),0)

brandBasePrices <- data.frame(brand = brand, price0 = brandNum)

brandMapPrices <- full_join(brandBasePrices,brandMap, by = "brand", multiple = "all")

# now get some variation by geography
zoneRatios <- data.frame(
  zone = unique(brandMap$zone),
  zoneEffect = runif(length(unique(brandMap$zone)), min = 0.8, max = 1.2))

regionRatios <- data.frame(
  region = region,
  regionEffect = rnorm(length(region),1,.1))

brandMapPrices <- left_join(brandMapPrices, zoneRatios, by = "zone")
brandMapPrices <- left_join(brandMapPrices, regionRatios, by = "region")

brandMapPrices <- brandMapPrices %>%
  mutate(price = price0*zoneEffect*regionEffect) %>%
  select(brand,segment,zone,region,price) %>%
  arrange(brand,zone,region)

# now some time trends
time <- yearMonth %>%
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

brandTimeMap <- left_join(brandTimeMap,
                            time, 
                            by = c("year","month"))

brandTimeMapPrices <- full_join(brandMapPrices,
                                   brandTimeMap, 
                                   by = c("brand","zone","region","segment"), multiple = "all")

brandTimeMapPrices$trend <- brandTimeMapPrices$trend * runif(nrow(brandTimeMapPrices),min = 0.9,max = 1.1)

brandTimeMapPrices$price <- brandTimeMapPrices$price + brandTimeMapPrices$trend

brandTimeMapPrices$trend = NULL

brandTimeMapPrices <- brandTimeMapPrices %>%
  group_by(brand, segment, year, month, zone) %>%
  mutate(listPrice = 1.06*max(price)) %>%
  rename(salesPrice = price) %>%
  ungroup()

#volume
# start with a base volume for each customer-brand

cb <- unique(brandTimeMapCust[,c("brand","customerID")])
cb$baseVolume <- round(runif(nrow(cb),-500,1000))
cb$baseVolume[cb$baseVolume < 0] <- 0

# add map variation
cbMap <- full_join(cb, brandMap, by = "brand", multiple = "all")
cbMap$shifter <- runif(nrow(cbMap),.7,2.3)
cbMap$baseVolume <- cbMap$baseVolume*cbMap$shifter
cbMap$shifter <- NULL

cbMap$dShift1 <- runif(nrow(cbMap),.8,3)
cbMap$dShift2 <- rnorm(nrow(cbMap),.02,.006)
cbMap$dShift3 <- rnorm(nrow(cbMap),.00006,.000001)
cbMap$dShift4 <- rnorm(nrow(cbMap),.0,.00000003)


# now that we have bases, add price impacts. price impacts are already varying by time, so no time effect needed
# bring together customers and prices

myData <- full_join(brandTimeMapCust,
                     brandTimeMapPrices,
                     by = intersect(names(brandTimeMapCust),names(brandTimeMapPrices)))

# bring in base volumes.

myData <- left_join(myData, 
                     cbMap,
                     by = intersect(names(myData),names(cbMap)),
                     multiple = "all")

# nonlinear demand
names(myData)
myData <- myData %>%
  group_by(brand, segment, zone,year) %>%
  mutate(volume0 = mean(baseVolume),
         volumeDiff = baseVolume - volume0,
         volume = abs( baseVolume + 
                         volumeDiff*dShift1 -
                         volumeDiff^2*dShift2 +
                         volumeDiff^3*dShift3 +
                         volumeDiff^4*dShift4),
         discount = (salesPrice - listPrice)/listPrice) %>%
  ungroup() %>%
  select(year, month, brand, segment, zone, region, customerID,
         listPrice, discount, salesPrice, volume)

# add days, dates
myData$day <- ceiling(runif(nrow(myData),0,31))
myData$day[(myData$month %in% c(4,6,9,11) & myData$day == 31) |
              (myData$month ==2 & myData$day > 28)] <- 
  myData$day[(myData$month %in% c(4,6,9,11) & myData$day == 31) |
                (myData$month ==2 & myData$day > 28)] - 12

myData$date <- paste0(myData$year,"-",sprintf("%02d", myData$month),"-", sprintf("%02d", myData$day))


write.csv(myData,"~/DS Videos/07 multi video series/multi_video_sales_data/true_project_data.csv", row.names = F)


## now lets add some real world charm to the data

# missing volumes
selection <- round(runif(179,1,nrow(myData)),0)
myData$volume[unique(selection)] <- NA

# outliers
rows <- round(runif(round(.0009*nrow(myData),0), 1, nrow(myData)),0)
myData$volume[rows] <- myData$volume[rows] ^ 2

# commas in volume, just to be annoying
myData$volume <- formatC(myData$volume, format = "f", digits = 0, big.mark = ",")


# multiple cases for brands and zones and regions
myData$brand[myData$year %in% c(2020, 2024)] <- toupper(myData$brand[myData$year %in% c(2020, 2024)])
myData$region[myData$year %in% c(2020, 2024)] <- toupper(myData$region[myData$year %in% c(2020, 2024)])
myData$zone[myData$year %in% c(2020, 2024)] <- toupper(myData$zone[myData$year %in% c(2020, 2024)])

#Annoying prefixes
myData$brand[myData$year %in% c(2021,2022)] <- paste0("br code: ",myData$brand[myData$year %in% c(2021,2022)])
myData$region[myData$year %in% c(2021,2022)] <- paste0("# ",myData$region[myData$year %in% c(2021,2022)])
myData$customerID[myData$zone == "south"] <- paste0("CID_",myData$customerID[myData$zone == "south"])

# misspell some regions
rows <- round(runif(10, 1, nrow(myData)),0)
myData$region[rows] <- gsub("r","rr",myData$region[rows])

rows <- round(runif(6, 1, nrow(myData)),0)
myData$region[rows] <- gsub("region","regoin",myData$region[rows])

rows <- round(runif(1034, 1, nrow(myData)),0)
myData$region[rows] <- gsub("region","region ",myData$region[rows])

# misspell some brands
rows <- round(runif(103, 1, nrow(myData)),0)
myData$brand[rows] <- gsub("brand","# brnd",myData$brand[rows])

rows <- round(runif(34, 1, nrow(myData)),0)
myData$brand[rows] <- gsub("brand","branD",myData$brand[rows])

rows <- round(runif(3034, 1, nrow(myData)),0)
myData$brand[rows] <- gsub("brand","brand ",myData$brand[rows])



# missing prices
rows <- round(runif(round(.07*nrow(myData),0), 1, nrow(myData)),0)
myData$listPrice[rows] <- NA
myData$salesPrice[rows] <- NA
myData$discount[rows] <- NA

# lazy reporting in the east
myData$discount[myData$zone == "east"] <- NA

# screw up dates
# myData$date <- as.Date(myData$date)
myData$date <- ymd(myData$date)

myData$date[myData$year == 2020] <- gsub("2020","20",myData$date[myData$year == 2020])

myData$date <- as.character(myData$date)
myData$date[myData$year == 2022] <- gsub("2022-","",myData$date[myData$year == 2022])
myData$date[myData$year == 2022] <- paste0(myData$date[myData$year == 2022],"-2022")

# price outlier
rows <- round(runif(round(.004*nrow(myData),0), 1, nrow(myData)),0)

myData$salesPrice[rows] <- myData$salesPrice[rows]^1.6

myData$year <- NULL
myData$month <- NULL
myData$day <- NULL

write.csv(myData,"~/DS Videos/07 multi video series/multi_video_sales_data/project_data.csv", row.names = F)
