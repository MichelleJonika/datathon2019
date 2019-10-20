library(dplyr)
library(phytools)
library(caret)
library(klaR)
library(ggplot2)
library(ggrepel)
library(mapproj)
library(maps)
library(RANN)

tacos <- read.csv('just tacos and burritos.csv') #load dataset

tacos.X <- dplyr::select(tacos, starts_with("X")) #identify empty variables
apply(is.na(tacos.X), 2, all)

tacos[is.na(tacos)] = "" #clear out NAs 

tacos.trimmed <- tacos[,-c(1,5,7,8,9,12,15,16,17,22:236)] #isolate important variables

tacos.trimmed1 <- tacos.trimmed[!tacos.trimmed$latitude=="",]#remove observations with missing geographical coordinates
tacos.trimmed1$latitude <- as.numeric(tacos.trimmed1$latitude) #change to numeric data type
tacos.trimmed1$longitude <- as.numeric(tacos.trimmed1$longitude)

traits.iter <- c("beef", "chicken", "pork", "cheddar", "american", "jack", "colby", "fresco",
                 "fish", "onion", "lettuce", "tomato", "avocado", "cabbage", "potato", "mushroom",
                 "kale", "pepper", "olive", "cactus", "jicama", "quinoa", "carrot", "cucumber",
                 "corn", "pacilla", "kimchi", "pico", "sour cream", "chipotle", "chimichurri", 
                 "sriracha", "ranch", "crema", "red", "rojo", "verde", "guac", "refried", "black bean",
                 "pinto", "colorado", "wahoo", "mahi", "roughy", "whitefish", "tuna", "cajun", "haddock",
                 "cod", "salmon", "shrimp", "crab", "lobster", "pulled pork", "adobada", "pastor", "bacon",
                 "belly", "sausage", "sirloin", "steak", "cheesesteak", "shortrib", "rib", "primerib", "prime rib",
                 "short rib", "barbacoa", "asada", "suadero", "tempeh", "tofu", "seitan", "hamburger",
                 "ground beef", "kale", "flour", "crispy", "peanut", "soft", "egg","sauce", "tomatillo",
                 "chihuahua", "spaghetti", "almond", "rice", "calamari", "octopus", "relleno", "carnita",
                 "asado", "seafood", "sorpesa", "choco", "green chile", "brisket", "agave", "ahi",
                 "bean", "chorizo", "machaca","chili", "doritos", "crunchy", "flatbread", "puffy", 
                 "cheese", "ham", "wasabi", "roja", "snapper", "dorado", "prosciutto", "queso", "pescado", 
                 "dorrados", "walleye", "vegetarian", "turkey", "tilapia", "maiz", "duck", "portobello",
                 "yam", "sweet potato", "grouper", "mignon", "spinach", "eel", "ice cream", "swai", 
                 "kobe", "bulgogi", "prawn", "cilantro", "pastrami", "lentil", "lamb", "goat", "squash",
                 "feta", "hard", "yuca", "yucca", "taco", "burrito") #specific traits to look for

traits.iter.mat <- c()
tacos.search <- paste(tacos.trimmed1$menus.description, tacos.trimmed1$menus.name, sep = " ") #vector for searches

for(trait in traits.iter){
  traits.iter.mat <- cbind(traits.iter.mat,sapply(tacos.search, grepl,pattern = trait, ignore.case = TRUE))
}#create matrix of search results composed of logical vectors

traits.df <- as.data.frame(traits.iter.mat) #create dataframe of search results
colnames(traits.df) <- traits.iter

traits.df.trues<-apply(traits.df, 1, sum)#identify useful variables

tacos.trimmed.merge <- data.frame(tacos.trimmed1, traits.df) #merge the search results with the original data
tacos.unpared.cols <- colnames(tacos.trimmed.merge)
tacos.trimmed.merge$latitude <- as.numeric(tacos.trimmed.merge$latitude)
tacos.trimmed.merge$longitude<- as.numeric(tacos.trimmed.merge$longitude)

tacos.trimmed.merge[tacos.trimmed.merge$jack, "cheese"] = TRUE #take care of redundancies and relationships among the dummy vars 
tacos.trimmed.merge[tacos.trimmed.merge$colby, "cheese"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$cheddar, "cheese"]= TRUE
tacos.trimmed.merge[tacos.trimmed.merge$american, "cheese"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$fresco, "cheese"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$cheesesteak, "cheese"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$feta, "cheese"] = TRUE


tacos.trimmed.merge[tacos.trimmed.merge$steak, "beef"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$shortrib, "beef"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$short.rib, "beef"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$prime.rib, "beef"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$primerib, "beef"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$bulgogi, "beef"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$kobe, "beef"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$brisket, "beef"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$ground.beef, "beef"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$hamburger, "beef"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$kobe, "beef"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$pastrami, "beef"] = TRUE

tacos.trimmed.merge[tacos.trimmed.merge$refried, "bean"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$black.bean, "bean"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$lentil, "bean"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$pinto, "bean"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$colorado, "bean"] = TRUE

tacos.trimmed.merge[tacos.trimmed.merge$wahoo, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$mahi, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$roughy, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$tuna, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$whitefish, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$haddock, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$cod, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$salmon, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$ahi, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$tuna, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$snapper, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$dorado, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$pescado, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$dorrados, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$walleye, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$tilapia, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$grouper, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$eel, "fish"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$swai, "fish"] = TRUE

tacos.trimmed.merge[tacos.trimmed.merge$ham, "pork"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$prosciutto, "pork"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$carnita, "pork"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$bacon, "pork"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$pastor, "pork"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$pulled.pork, "pork"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$machaca, "pork"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$belly, "pork"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$sausage, "pork"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$adobada, "pork"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$chorizo, "pork"] = TRUE

tacos.trimmed.merge[tacos.trimmed.merge$sauce, "sauce"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$tomatillo, "sauce"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$chihuahua, "sauce"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$ranch, "sauce"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$guac, "sauce"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$sour.cream, "sauce"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$sriracha, "sauce"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$crema, "sauce"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$red, "sauce"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$roja, "sauce"]= TRUE
tacos.trimmed.merge[tacos.trimmed.merge$rojo, "sauce"]=TRUE
tacos.trimmed.merge[tacos.trimmed.merge$pico, "sauce"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$verde, "sauce"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$choco, "sauce"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$wasabi, "sauce"]=TRUE


tacos.trimmed.merge[tacos.trimmed.merge$fish, "seafood"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$calamari, "seafood"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$octopus, "seafood"]=TRUE
tacos.trimmed.merge[tacos.trimmed.merge$prawn, "seafood"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$shrimp, "seafood"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$crab, "seafood"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$lobster, "seafood"] = TRUE

tacos.trimmed.merge$onion <- NULL #remove uninformative dummy vars
tacos.trimmed.merge$tomato <- NULL
tacos.trimmed.merge$lettuce <- NULL
tacos.trimmed.merge$kale.1 <- NULL
tacos.trimmed.merge$specialtyveg <- c()

tacos.trimmed.merge[tacos.trimmed.merge$avocado, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$olive, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$yuca, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$yucca, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$squash, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$agave, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$relleno, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$sweet.potato, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$spinach, "specialtyveg"]=TRUE
tacos.trimmed.merge[tacos.trimmed.merge$yam, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$agave, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$peanut, "specialtyveg"]= TRUE
tacos.trimmed.merge[tacos.trimmed.merge$portobello, "specialtyveg"]=TRUE
tacos.trimmed.merge[tacos.trimmed.merge$almond, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$kale, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$cactus, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$jicama, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$carrot, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$cucumber, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$kimchi, "specialtyveg" ] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$chipotle, "specialtyveg"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$pacilla, "specialtyveg"] = TRUE
tacos.trimmed.merge[is.na(tacos.trimmed.merge$specialtyveg), "specialtyveg"] = FALSE

tacos.trimmed.merge[tacos.trimmed.merge$tofu, "vegetarian"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$seitan, "vegetarian"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$tempeh, "vegetarian"] = TRUE

tacos.trimmed.merge[tacos.trimmed.merge$pulled.pork, "carnita"] = TRUE
tacos.trimmed.merge$pulled.pork <- NULL                    

tacos.trimmed.merge[tacos.trimmed.merge$prawn, "shrimp"] = TRUE
tacos.trimmed.merge$prawn<- NULL
tacos.trimmed.merge[tacos.trimmed.merge$prime.rib, "primerib"] = TRUE
tacos.trimmed.merge$prime.rib <- NULL
tacos.trimmed.merge[tacos.trimmed.merge$short.rib, "shortrib"] = TRUE
tacos.trimmed.merge$short.rib <- NULL
tacos.trimmed.merge[tacos.trimmed.merge$maiz, "corn"] = TRUE
tacos.trimmed.merge$maiz <- NULL
tacos.trimmed.merge[tacos.trimmed.merge$roja, "red"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$rojo, "red"]= TRUE
tacos.trimmed.merge$roja <- NULL
tacos.trimmed.merge$rojo<- NULL
tacos.trimmed.merge[tacos.trimmed.merge$ground.beef, "hamburger"] = TRUE
tacos.trimmed.merge$ground.beef <- NULL
tacos.trimmed.merge[tacos.trimmed.merge$sausage, "chorizo"] = TRUE
tacos.trimmed.merge$sausage <- NULL
tacos.trimmed.merge[tacos.trimmed.merge$sweet.potato, "yam"] = TRUE
tacos.trimmed.merge$sweet.potato <- NULL
tacos.trimmed.merge[tacos.trimmed.merge$green.chile, "pepper"] = TRUE
tacos.trimmed.merge$green.chile <- NULL
tacos.trimmed.merge[tacos.trimmed.merge$pescado, "fish"] = TRUE
tacos.trimmed.merge$pescado <- NULL
tacos.trimmed.merge[tacos.trimmed.merge$yuca, "yucca"] = TRUE
tacos.trimmed.merge$yuca <- NULL
tacos.trimmed.merge[tacos.trimmed.merge$dorrados, "dorados"] = TRUE
tacos.trimmed.merge$dorrados<- NULL
tacos.trimmed.merge[tacos.trimmed.merge$portobello, "mushroom"] = TRUE
tacos.trimmed.merge$portobello <- NULL

cuisine <-C() #parse through other qualitative variables to create more meaningful variables
for(obs in 1:nrow(tacos.trimmed.merge)){
  if(grepl("american", tacos.search2[obs], ignore.case = TRUE)){
    cuisine[obs] <- "american"
  }
  else if(grepl("mexican", tacos.search2[obs], ignore.case = TRUE)){
    cuisine[obs] <- "mexican"
  }
  else if(grepl("latin", tacos.search2[obs],ignore.case=TRUE)){
    cuisine[obs] <- "latin"
  }
  else if(grepl("asian", tacos.search2[obs],ignore.case=TRUE)){
    cuisine[obs] <- "asian"
  }
  else if(grepl("greek", tacos.search2[obs],ignore.case=TRUE)){
    cuisine[obs] <- "latin"
  }
  else if(grepl("mediterranean", tacos.search2[obs],ignore.case=TRUE)){
    cuisine[obs] <- "greek"
  }
  else if(grepl("tex", tacos.search2[obs],ignore.case=TRUE)){
    cuisine[obs] <- "texmex"
  }
  else if(grepl("italian", tacos.search2[obs],ignore.case=TRUE)){
    cuisine[obs] <- "italian"
  }
  else if(grepl("french", tacos.search2[obs],ignore.case=TRUE)){
    cuisine[obs] <- "french"
  }
  else if(grepl("korean", tacos.search2[obs],ignore.case=TRUE)){
    cuisine[obs] <- "asian"
  }
  else if(grepl("japanese", tacos.search2[obs],ignore.case=TRUE)){
    cuisine[obs] <- "asian"
  }
}
tacos.trimmed.merge$cuisine <- cuisine
levels(tacos.trimmed.merge$cuisine) <- unique(cuisine)

venue.type <- c()
for(obs in 1:nrow(tacos.trimmed.merge)){
  if(grepl("coffee", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "coffeeshop"
  }
  else if(grepl("takeout", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "delivery"
  }
  else if(grepl("take-out", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "delivery"
  }
  else if(grepl("take out", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "delivery"
  }
  else if(grepl("delivery", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "delivery"
  }
  else if(grepl("fast", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "fastfood"
  }
  else if(grepl("wings", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "wings"
  }
  else if(grepl("sandwich", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "sandwich"
  }
  else if(grepl("diner", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "diner"
  }
  else if(grepl("music", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "bar"
  }
  else if(grepl("buffet", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "buffet"
  }
  else if(grepl("convenience", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "convenience"
  }
  else if(grepl("bar", tacos.search2[obs], ignore.case = TRUE)){
    venue.type[obs] <- "bar"
  }
}

venue.type[55636] <- ""
tacos.trimmed.merge$venue.type <- venue.type
tacos.trm2.merge <- tacos.trimmed.merge[,-c(2,4)]

tacos.trm2.merge1 <- tacos.trm2.merge[which(tacos.trm2.merge$longitude<=0),]#remove specious or extra-continental observations
tacos.trm2.merge1 <- tacos.trm2.merge[which(tacos.trm2.merge1$latitude>=10),]
rownames(tacos.trm2.merge1)<-c() #reset observation indices

#change the latitude and longitude to numeric instead of character
tacos.trm2.merge1$latitude <- as.numeric(tacos.trm2.merge1$latitude)
tacos.trm2.merge1$longitude <- as.numeric(tacos.trm2.merge1$longitude)
#subset the original dataset to have just latitude and longitude
tacos.points <- tacos.trm2.merge1[,c(5:6)]

colnames(tacos.points) <- c("Latitude", "Longitude")
kmeans <- kmeans(tp.trimmed, 
                 centers = 10, nstart = 25)

tp.kmeans <- cbind(tacos.points, kmeans$cluster)
us <- map_data('world',
               c('usa', 'canada', "hawaii", "mexico"))
ggplot()+
  geom_polygon(data=us, aes(x=long, y=lat, group = group), 
               colour="grey20", fill= "grey50")+
  geom_point(data = tacos.points, aes(x=longitude, y = latitude,  
                                    color= as.factor(tp.kmeans$`kmeans$cluster`)), 
             show.legend = F, cex=0.2, pch=1)+
  coord_map(projection = "mercator", xlim=c(-162, -60), ylim = c(10,62))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()

tacos.kmeans <- cbind(tacos.trm2.merge1, kmeans$cluster)

tacos.kmeans$`kmeans$cluster` <- as.character(tacos.kmeans$`kmeans$cluster`)
tacos.kmeans.knn<- tacos.kmeans[, -c(6,7,8)]#impute missing values with knn
tacos.kmeans.knn$menus.amountMax <- as.numeric(tacos.kmeans.knn$menus.amountMax)
preProcess_missingdata_model <- preProcess(tacos.kmeans.knn, method='knnImpute')
tacos.kmeans.knn <- predict(preProcess_missingdata_model, newdata = tacos.kmeans.knn)

tacos.kmeans.imputed <- tacos.kmeans.knn[,-c(3,4,140)]
tacos.kmeans.imputed <- cbind(tacos.kmeans.imputed, tacos.kmeans[,c(3,4,143)])
tacos.kmeans.imputed$`kmeans$cluster` <- as.character(tacos.kmeans.imputed$`kmeans$cluster`)

kmodefit <- kmodes(na.omit(tacos.kmeans.imputed[,-c(3, 138, 139)]), modes = 15)#remove quantitative vars for kmodes
tacos.final <- na.omit(tacos.kmeans.imputed[,-c(3,138,139)])
restore.indices <- rownames(tacos.final)
kmodefit$cluster
tacos.final$kmode <- kmodefit$cluster
tacos.final <- tacos.final[,-c(1:4, 135,136)]#remove qualitative vars for hclust
tacos.final <- cbind(tacos.final, tacos.kmeans.imputed[rownames(tacos.final),c(3,138,139)])
tacos.final <- as.data.frame(sapply(tacos.final, as.numeric))
finalspecies <- c()
for(i in 1:15){
  sum<-apply(tacos.final[tacos.final$kmode == i,], 2, sum)
  finalspecies <- rbind(finalspecies,sum/nrow(tacos.final[tacos.final$kmode == i,]))
}
finalspecies <- as.data.frame(finalspecies)

taco.dist <- dist(finalspecies)
taco.phylo <- hclust(taco.dist)
plot(taco.phylo)

taco.phylo <- as.phylo(taco.phylo)
conttrait <- finalspecies$beef 
conttrait <- finalspecies$kale 
conttrait <- finalspecies$fish 
conttrait <- finalspecies$vegetarian 
conttrait <- finalspecies$burrito 
conttrait <- finalspecies$taco 
conttrait <- finalspecies$sriracha
names(conttrait) <- taco.phylo$tip.label
smp <- contMap(taco.phylo,conttrait, legend = F, plot = T)
n<-length(smp$cols)
smp$cols[1:n]<-rainbow(n, end = 4/6)
gradientLegend(depth = .03, valRange = c(round(min(conttrait),3),
                                         round(max(conttrait),3)), 
               side = 1, pos = .17, color = rainbow(n, end = 4/6))
legend(x = 'bottomleft', legend = '', bg="transparent", bty = 'n')

par(mfrow = c(1,2))
conttrait <- finalspecies$kimchi
names(conttrait) <- taco.phylo$tip.label
smp <- contMap(taco.phylo,conttrait, legend = F, plot = T)
n<-length(smp$cols)
smp$cols[1:n]<-rainbow(n, end = 4/6)
# gradientLegend(depth = .02, valRange = c(round(min(conttrait),3),
#                                          round(max(conttrait),3)), 
#                side = 1, pos = .17, color = rainbow(n, end = 4/6),dec = -1)
# legend(x = 'bottomleft', legend = '', title = 'Standardized Trait Value', bg="transparent", bty = 'n')
title(main = '\nKimchi Trait Mapping')
conttrait <- finalspecies$bulgogi
names(conttrait) <- taco.phylo$tip.label
smp <- contMap(taco.phylo,conttrait, legend = F, plot = T)
n<-length(smp$cols)
smp$cols[1:n]<-rainbow(n, end = 4/6)
# gradientLegend(depth = .02, valRange = c(round(min(conttrait),3),
#                                          round(max(conttrait),3)), 
#                side = 1, pos = .17, color = rainbow(n, end = 4/6),dec = -1)
# legend(x = 'bottomleft', legend = '', title = 'Standardized Trait Value', bg="transparent", bty = 'n')
title(main = '\nBulgogi Trait Mapping')
par(mfrow = c(1,2))
conttrait <- finalspecies$beef
names(conttrait) <- taco.phylo$tip.label
smp <- contMap(taco.phylo,conttrait, legend = F, plot = T)
n<-length(smp$cols)
smp$cols[1:n]<-rainbow(n, end = 4/6)
# gradientLegend(depth = .02, valRange = c(round(min(conttrait),3),
#                                          round(max(conttrait),3)),
#                side = 1, pos = .17, color = rainbow(n, end = 4/6),dec = -1)
# legend(x = 'bottomleft', legend = '', title = 'Standardized Trait Value', bg="transparent", bty = 'n')
title(main = '\nBeef Trait Mapping')
conttrait <- finalspecies$vegetarian
names(conttrait) <- taco.phylo$tip.label
smp <- contMap(taco.phylo,conttrait, legend = F, plot = T)
n<-length(smp$cols)
smp$cols[1:n]<-rainbow(n, end = 4/6)
# gradientLegend(depth = .02, valRange = c(round(min(conttrait),3),
#                                          round(max(conttrait),3)), 
#                side = 1, pos = .17, color = rainbow(n, end = 4/6),dec = -1)
# legend(x = 'bottomleft', legend = '', title = 'Standardized Trait Value', bg="transparent", bty = 'n')
title(main = '\nVegetarian Trait Mapping')

