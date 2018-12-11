
========================================================
author: Group ASAP
date: 18/12/2018
title: Women Shoes Recommender 
autosize: true


Loading required packages and dataset 
========================================================


```r
library(imager)
library(magick)
library(dplyr)
library(tidyverse)
library(stringr)
library(zoo)
library(xts)
require("jsonlite")

data<- "7210_1.csv"
```

Defining the data cleaning function
========================================================
(i)   Dates parsed are calculated by subtracting date Added with Current date.

(ii)  Only 23 features are selected from the total of 52 as the rest are deemed as irrelevant to our project purpose.

(iii) Prices are all normalized to USD then mean is calculated by getting the average of the maximum and minumum price of each item.

(iv)  Images are split to URL.

(v)   Measurement of weights are standardized to KGs.

(vi)  Price condition column is standardized to reduce redundancy.

(vii) NAs in Brand column are imputed with "Others".

(viii)Date trend is calculated by taking the distance in months between the date of data added to database and the current date.

(ix)  Average price is computed by taking the mean of the maximum and minimum price of the observation across all available merchants.

(x)   Observations with NA in the needed columns are removed.

(xi)  Observations with NaN in the average price column are removed.



```r
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
 
  
  #split image to URL
  newdf$imageURLs <- as.character(newdf$imageURLs) %>% strsplit(",")
  
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
  
  #standardize price.conditions
  newdf$prices.condition[newdf$prices.condition == "new"]<-"New"
  newdf$prices.condition[newdf$prices.condition == "Brand New"]<-"New"
  
  #Brand cleaning
  as.character(newdf$brand)
  newdf$brand<-toupper(newdf$brand)
  newdf$brand[newdf$brand==""]<-"Others"
  
  #weight cleaning
  a<- subset(newdf,!is.na(newdf$weight))
  
  #Group by ID
  a <- a %>% 
    group_by(id) %>% 
    summarise(count = n(), 
              brand = first(brand),
              price.max = mean(prices.amountMax, na.rm=TRUE),
              price.min = mean(prices.amountMin, na.rm=TRUE),
              urls = imageURLs[1],
              colors = first(colors),
              name = name[1],
              Onlinemerchant = first(prices.merchant),
              price.avg_USD = round((price.max + price.min) / 2),
              weight_KG = first(weight)
              ,date_trend =  mean(date_parsed,na.rm = TRUE)
    ) %>%
    filter(!is.nan(price.avg_USD))
  
  return(a)

}
```

Applying cleaning function to the dataset 
========================================================
The data cleaning function is then applied into the dataset and head of the dataset is viewed to ensure that the output is as expected.


```r
cleaned_data<-clean(data)
head(cleaned_data)
```

```
# A tibble: 6 x 12
  id    count brand price.max price.min urls  colors name  Onlinemerchant
  <fct> <int> <chr>     <dbl>     <dbl> <lis> <fct>  <fct> <fct>         
1 AVpe~     1 Othe~     133.      133.  <chr~ White~ Yu&y~ Amazon.ca     
2 AVpe~     2 Othe~      35.3      35.3 <chr~ Black~ Ankl~ UrbanInspirat~
3 AVpe~     1 Othe~      56.5      56.5 <chr~ White~ Yu&y~ Amazon.ca     
4 AVpe~     1 Othe~      62.8      62.8 <chr~ ""     Snee~ YuTin Jia     
5 AVpe~     1 Othe~     133.      133.  <chr~ Black~ Yu&y~ Amazon.ca     
6 AVpe~     1 Othe~      77.8      77.8 <chr~ White~ Yu&y~ Amazon.ca     
# ... with 3 more variables: price.avg_USD <dbl>, weight_KG <dbl>,
#   date_trend <dbl>
```

Categorizing Colours
========================================================
White space of the color column is cleared and colours are group into general color group, unspecified colors are imputed with "Other".


```r
cleaned_data$colors<-trimws(cleaned_data$colors)


cleaned_data<-cleaned_data%>%
  separate(colors,"colors",",")

#Impute empty strings with "Other"
cleaned_data$colors<-ifelse(cleaned_data$colors=="","Other",
                  ifelse(cleaned_data$colors %in% c("Red","d","Pink","Fuchsia",
                              "Coral"),"Red",
                         ifelse(cleaned_data$colors %in% c("Grey","Grey  Pink"),"Grey",
                                ifelse(cleaned_data$colors %in% c("Pink","Fuchsia","Peach","Hot Pink"),"Pink",
                                       ifelse(cleaned_data$colors %in% c("Tan","TAN","Camel","Khaki","Almond","Ivory","Bone","Rose",
                                                               "Coffee","Chocolate","Beige","Moccasin","Sassy Brown","nude color"),"Brown",
                                              ifelse(cleaned_data$colors %in% c("Black","Oyster","c","Black and White","black","Blacks","b","Black Blue","Charcoal","Natural",
                                                                      "Black Coral","Black Paris","Black Patent","mysterious black"),"Black",
                                                     ifelse(cleaned_data$colors %in% c("WhitePurpleLavender","White","WhiteSilverWhite"),"White",
                                                            ifelse(cleaned_data$colors %in% c("Blue","Navy","Dark BlueWhiteDkblue","Royal Blue"),"Blue",
                                                                   ifelse(cleaned_data$colors %in% c("Leopard","CAMO"),"Prints",
                                                                          ifelse(cleaned_data$colors %in% c("Multicolor","Multi Color"),"Multi",
                                                                                 ifelse(cleaned_data$colors %in% c("Gold","golden","Golden"),"Gold",
                                                                                        ifelse(cleaned_data$colors %in% c("Light Green","Green","PurpleRedGreen"),"Green",
                                                                                               ifelse(cleaned_data$colors %in% c("MangoLavenderHot Coral","Yellow","a"),"Yellow",cleaned_data$colors)))))))))))))
```

Exporting cleaned data
========================================================
The cleaned and ready-to-use dataset is then finally exported as an CSV file.

```r
cleaned_data<-as.data.frame(cleaned_data)
cleaned_data$urls <- vapply(cleaned_data$urls, paste, collapse = ", ", character(1L))
write.table(cleaned_data,"cleaned_data_color.csv")
```

