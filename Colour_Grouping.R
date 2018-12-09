library(dplyr)
library(tidyr)

data<-read.csv("7210_1.csv")
source("cleaning function_with color.R")
df<-clean(data)
df$colors<-trimws(df$colors)


df<-df %>%
  separate(colors,"colors",",")

#Retrieve URL to check vague colour
##unlist(df[which(df$col1=="Natural"),"urls"])

#Impute empty strings with "Other"
df$colors<-ifelse(df$colors=="","Other",df$colors)

df$colors<-ifelse(df$colors %in% c("Red","d","Pink","Fuchsia",
                              "Coral"),"Red",df$colors)

df$colors<-ifelse(df$colors %in% c("Grey","Grey  Pink"),"Grey",df$colors)

df$colors<-ifelse(df$colors %in% c("Pink","Fuchsia","Peach","Hot Pink"),"Pink",df$colors)

df$colors<-ifelse(df$colors %in% c("Tan","TAN","Camel","Khaki","Almond","Ivory","Bone","Rose",
                               "Coffee","Chocolate","Beige","Moccasin","Sassy Brown","nude color"),"Brown",df$colors)

df$colors<-ifelse(df$colors %in% c("Black","Oyster","c","Black and White","black","Blacks","b","Black Blue","Charcoal","Natural",
                               "Black Coral","Black Paris","Black Patent","mysterious black"),"Black", df$colors)

df$colors<-ifelse(df$colors %in% c("WhitePurpleLavender","White","WhiteSilverWhite"),"White",df$colors)

df$colors<-ifelse(df$colors %in% c("Blue","Navy","Dark BlueWhiteDkblue","Royal Blue"),"Blue",df$colors)

df$colors<-ifelse(df$colors %in% c("Leopard","CAMO"),"Prints",df$colors)

df$colors<-ifelse(df$colors %in% c("Multicolor","Multi Color"),"Multi", df$colors)

df$colors<-ifelse(df$colors %in% c("Gold","golden","Golden"),"Gold",df$colors)

df$colors<-ifelse(df$colors %in% c("Light Green","Green","PurpleRedGreen"),"Green",df$colors)

df$colors<-ifelse(df$colors %in% c("MangoLavenderHot Coral","Yellow","a"),"Yellow",df$colors)

unique(df$colors)

