#cleaning function for woman shoe
library("imager")
library("magick")
library("dplyr")
library("tidyverse")
library("stringr")
library(zoo)
library(xts)
require("jsonlite")

data<- "7210_1.csv"

#insert file location to the function
clean <- function(data){
  df<- read.csv(data)
  #select features
  df$date_parsed <-(zoo::as.yearmon(strptime(Sys.Date(), format = "%Y-%m-%d"))-zoo::as.yearmon(strptime(parse_date(as.Date(df$dateAdded)), format = "%Y-%m-%d")))*12
  new <- c("id",	"brand"	,"categories",	"colors",	"descriptions",	"features","imageURLs",	
           "manufacturer",	"merchants",	"name",	"prices.amountMin",	"prices.amountMax",	"prices.condition",	"prices.currency",
           "prices.isSale",	"prices.merchant",	"prices.size",	"reviews",	"sizes",	"skus",
           "sourceURLs",	"weight", "date_parsed")
  newdf<- df[new]
  
  
  length(levels(newdf[,1])) #9996 products
  #=====================================================================================================================
  #Transform Price
  #All normalize to USD
  convertPrice <- function(currency, amount){
    ifelse(currency=='AUD', amount * 0.75,
           ifelse(currency=='CAD', amount * 0.73,
                  ifelse(currency=='EUR', amount * 1.1,
                         ifelse(currency=='GPB', amount * 1.3, 
                                amount))))
  }
  
  #Standardize Price
  newdf$prices.amountMax <- as.numeric(as.character(newdf$prices.amountMax))
  newdf$prices.amountMin <- as.numeric(as.character(newdf$prices.amountMin))
  newdf$prices.amountMin <- convertPrice(newdf$prices.currency, df$prices.amountMin)
  newdf$prices.amountMax <- convertPrice(newdf$prices.currency, df$prices.amountMax)
  table(newdf$prices.currency)
  newdf$prices.currency <- "USD"
  mutate(newdf,average.prices = mean(prices.amountMax+prices.amountMin))
  #=====================================================================================================================
  
  #split image to URL
  newdf$imageURLs <- as.character(newdf$imageURLs) %>% strsplit(",")
  
  #=====================================================================================================================
  #Standardize weight
  
  newdf<-separate(newdf,weight,c("weight","denominator"),sep=" ")
  
  #standardize to kgs
  convertweight <- function(denominator, amount){
    ifelse(denominator=='g', amount * 0.001,
           ifelse(denominator=='grams', amount * 0.001,
                  ifelse(denominator=='lbs', amount * 0.45,
                         ifelse(denominator=='ounces', amount * 0.03, 
                                ifelse(denominator == 'oz',amount*0.03,
                                       ifelse(denominator == 'pounds',amount*0.45,
                                              ifelse(denominator == 'Pounds',amount*0.45,
                                                     amount)))))))
  }
  
  newdf$weight[is.na(newdf$weight)]<- 0
  newdf$weight[(df$weight=="")]<- 0
  newdf$weight <-as.numeric(newdf$weight)
  newdf$weight<-convertweight(newdf$denominator,newdf$weight)
  newdf$denominator <- 'Kg'
  #=====================================================================================================================
  #standardize price.conditions
  newdf$prices.condition[newdf$prices.condition == "new"]<-"New"
  newdf$prices.condition[newdf$prices.condition == "Brand New"]<-"New"
  
  
  #=====================================================================================================================
  #Brand cleaning
  as.character(newdf$brand)
  newdf$brand<-toupper(newdf$brand)
  newdf$brand[newdf$brand==""]<-"Others"
  
  
  #=====================================================================================================================
  #weight cleaning
  a<- subset(newdf,!is.na(newdf$weight))
  #=====================================================================================================================
  
  
  #Group by ID
  a <- a %>% 
    group_by(id) %>% 
    summarise(count = n(), 
              brand = first(brand),
              price.max = mean(prices.amountMax, na.rm=TRUE),
              price.min = mean(prices.amountMin, na.rm=TRUE),
              urls = imageURLs[1],
              name = name[1],
              Onlinemerchant = first(prices.merchant),
              price.avg_USD = round((price.max + price.min) / 2),
              weight_KG = first(weight)
              ,date_trend =  mean(date_parsed,na.rm = TRUE)
    ) %>%
    filter(!is.nan(price.avg_USD))
  
  
  
  
  
  
  return(a)
  
  
  
}