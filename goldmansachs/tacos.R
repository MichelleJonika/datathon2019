library(dplyr)
tacos <- read.csv("../goldmansachs/just tacos and burritos.csv")
head.tacos<-head(tacos)

tacos.X <- dplyr::select(tacos, starts_with("X"))
tacos[is.na(tacos)] = ""

apply(is.na(tacos.X), 2, all )
sum(tacos$menus.description == "")                  
sum(tacos$menus.name == "")
sum(tacos$city == "")
sum(tacos$country=="")
sum(tacos$cuisines == "")
sum(tacos$latitude == "")
sum(tacos$longitude == "")
sum(tacos$postalCode =="")
sum(tacos$name == "")
sum(tacos$province == "")
sum(tacos$menus.amountMax == "")
sum(tacos$menus.amountMin == "")
sum(tacos$menus.category == "")
sum(tacos$name == "")
sum(tacos$priceRangeCurrency == "")
sum(tacos$priceRangeMin == "")
sum(tacos$priceRangeMax == "")
range(tacos$priceRangeMin)


tacos.trimmed <- tacos[,-c(1,5,7,8,9,12,15,16,17,22:236)]
sum(as.numeric(is.na(tacos.trimmed)))
tacos.trimmed1 <- tacos.trimmed[!tacos.trimmed$latitude==0,]
length(unique(tacos$address))
length(unique(tacos$id))
