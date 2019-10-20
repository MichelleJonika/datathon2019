data <- read.csv('just tacos and burritos.csv')
x <- head(data)
x
data <- data[,1:26]
# data <- cbind(data, 'beef')

beefcol <- vector(length = nrow(data))
for(i in 1:nrow(data)){
  if(grepl('Beef', as.character(data$menus.description[i]))){
    beefcol[i] <- 1
  }else if(grepl('beef', as.character(data$menus.description[i]))){
    beefcol[i] <- 1
  }else{beefcol[i] <- 0}
}
colnames(beefcol) <- c('Beef')  

all.desc <- ''
for(i in 1:nrow(data)){
  all.desc <- paste(all.desc, as.character(data$menus.description[i]), sep = ',')
}

diff.ing <- strsplit(all.desc, split = ',')
diff.ing <- diff.ing[[1]]
diff.ing <- unique(diff.ing)



tacovec <- vector(length = nrow(data))
burritovec <- vector(length = nrow(data))
for(i in 1:nrow(data)){
  if(grepl('taco', tolower(as.character(data$menus.name[i])))){
    tacovec[i] <- 1
  }else{tacovec[i] <- 0}
  if(grepl('burrito', tolower(as.character(data$menus.name[i])))){
    burritovec[i] <- 1
  }else{burritovec[i] <- 0}
}

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
                 "feta", "hard", "yuca", "yucca", "taco", "burrito"
)
traits.iter.mat <- c()
tacos.search <- paste(tacos.trimmed1$menus.description, tacos.trimmed1$menus.name, sep = " ")

# for(obs in 1:nrow(tacos.trimmed1))
# {
#   if(tacos.trimmed1$menus.description!= "")
#   {
#     tacos.search <- c(tacos.search, tacos.trimmed1$menus.description[obs])
#   }
#   else
#   {
#     tacos.search <- c(tacos.search, tacos.trimmed1$menus.name[obs])
#   }
# }

for(trait in traits.iter){
  traits.iter.mat <- cbind(traits.iter.mat,sapply(tacos.search, grepl,pattern = trait, ignore.case = TRUE))
}

traits.df <- as.data.frame(traits.iter.mat)
colnames(traits.df) <- traits.iter

traits.df.trues<-apply(traits.df, 1, sum)

tacos.trimmed.merge <- data.frame(tacos.trimmed1, traits.df)
tacos.unpared.cols <- colnames(tacos.trimmed.merge)
tacos.trimmed.merge$latitude <- as.numeric(tacos.trimmed.merge$latitude)
tacos.trimmed.merge$longitude<- as.numeric(tacos.trimmed.merge$longitude)

tacos.trimmed.merge[tacos.trimmed.merge$jack, "cheese"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$colby, "cheese"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$cheddar, "cheese"]= TRUE
tacos.trimmed.merge[tacos.trimmed.merge$american, "cheese"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$fresco, "cheese"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$cheesesteak, "cheese"] = TRUE

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

tacos.trimmed.merge[tacos.trimmed.merge$refried, "bean"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$black.bean, "bean"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$lentil, "bean"] = TRUE
tacos.trimmed.merge[tacos.trimmed.merge$pinto, "bean"] = TRUE

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


ifelse(tacos.trimmed.merge$jack, tacos.trimmed.merge$cheese = T)
## family
meatvec <- vector(length = nrow(data)) # beef, chicken, pork
cheesevec <- vector(length = nrow(data)) # cheddar, american, jack, colby, fresco
seavec <- vector(length = nrow(data))# fish, othersea
rice <- vector(length = nrow(data))
vegevec <- vector(length = nrow(data)) # onion, lettuce, tomato, avocado, cabbage, potatoes, mushroom, kale,
##  peppers, olive, cactus, jicama, quinoa, carrot, cucumber, corn, pacilla, kimchi
saucevec <- vector(length = nrow(data))# pico, sourcream, chipotle, chimichurri, sriracha, ranch, crema, red,
## salsa, chileverde, guac
beans <- vector(length = nrow(data)) # refried, black, pinto, colorado

## ingredients
fish <- vector(length = nrow(data))# wahoo, mahi, white, tuna, cajun, haddock,cod, salmon, roughy
othersea <- vector(length = nrow(data)) # shrimp, crab, lobster
pork <- vector(length = nrow(data)) # pulled, adobada, pastor, bacon, belly, sausage, sirloin
beef <- vector(length = nrow(data)) # steak, cheesesteak, shortrib, primerib, barbacoa, carneasada, suadero,
vegesubs <- vector(length = nrow(data)) # tempeh
onion <- vector(length = nrow(data))
cheddar <- vector(length = nrow(data))
lettuce<- vector(length = nrow(data))
refried<- vector(length = nrow(data))
tomato <- vector(length = nrow(data))
chicken <- vector(length = nrow(data))
pico <- vector(length = nrow(data))
wahoo <- vector(length = nrow(data))
avocado <- vector(length = nrow(data))
cabbage <- vector(length = nrow(data))
pulledcarnitaskalua <- vector(length = nrow(data))
steak <- vector(length = nrow(data))
shrimp <- vector(length = nrow(data))
corntortilla <- vector(length = nrow(data))
black <- vector(length = nrow(data))
rice <- vector(length = nrow(data))
pinto <- vector(length = nrow(data))
potatoes <- vector(length = nrow(data))
mushrooms <- vector(length = nrow(data))
kale <- vector(length = nrow(data))
cheesesteak <- vector(length = nrow(data))
spaghetti <- vector(length = nrow(data))
mahi <- vector(length = nrow(data))
shortrib <- vector(length = nrow(data))
primerib <- vector(length = nrow(data))
adobada <- vector(length = nrow(data))
barbacoa <- vector(length = nrow(data))
carneasada <- vector(length = nrow(data))
american <- vector(length = nrow(data))
sourcream <- vector(length = nrow(data))
peppers <- vector(length = nrow(data))
flourtortilla <- vector(length = nrow(data))
colorado <- vector(length = nrow(data))
chipotle <- vector(length = nrow(data))
jack <- vector(length = nrow(data))
white <- vector(length = nrow(data))
tuna <- vector(length = nrow(data))
crab <- vector(length = nrow(data))
olive <- vector(length = nrow(data))
pastor <- vector(length = nrow(data))
cajun <- vector(length = nrow(data))
colby <- vector(length = nrow(data))
haddock <- vector(length = nrow(data))
cactus<- vector(length = nrow(data))
lobster<- vector(length = nrow(data))
cod <- vector(length = nrow(data))
suadero <- vector(length = nrow(data))
jicama <- vector(length = nrow(data))
bacon <- vector(length = nrow(data))
tempeh <- vector(length = nrow(data))
fresco <- vector(length = nrow(data))
chimichurri <- vector(length = nrow(data))
salmon <- vector(length = nrow(data))
quinoa <- vector(length = nrow(data))
cucumber <- vector(length = nrow(data))
carrot <- vector(length = nrow(data))
sriracha <- vector(length = nrow(data))
roughy <- vector(length = nrow(data))
corn <- vector(length = nrow(data))
pacilla <- vector(length = nrow(data))
belly <- vector(length = nrow(data))
sausagechorizo <- vector(length = nrow(data))
ranch <- vector(length = nrow(data))
crema <- vector(length = nrow(data))
sirloin <- vector(length = nrow(data))
redrojo <- vector(length = nrow(data))
salsa <- vector(length = nrow(data))
chileverde <- vector(length = nrow(data))
guac <- vector(length = nrow(data))
kimchi <- vector(length = nrow(data))














load("~/GitHub/datathon2019/conocophillips/tacos.trimmed1.RData")
for(i in 5:8){
  tacos.trimmed1[,i] <- as.numeric(tacos.trimmed1[,i])
}
# count <- c()
# for(i in 1:nrow(tacos.trimmed1)){
#   if(!is.na(tacos.trimmed1$menus.amountMax)[i]){
#     if(sum(tacos.trimmed1$menus.amountMax[i] != tacos.trimmed1$menus.amountMin[i])){
#       count <- c(count, i)
#     }
#   }
# }


tacos.trimmed1 <- tacos.trimmed1[,-8]

preProcess_missingdata_model <- preProcess(tacos.trimmed1, method='knnImpute')

trainData <- predict(preProcess_missingdata_model, newdata = tacos.trimmed1)
anyNA(trainData)

##  converting categorical variables to numeric to be used in machine learning
# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
# dummies_model <- dummyVars(Purchase ~ ., data=trainData)
randomsample <- trainData[(sample(1:nrow(tacos.trimmed1), 5000)),]

X.quanti <- PCAmixdata::splitmix(randomsample)$X.quanti
X.quali <- PCAmixdata::splitmix(randomsample)$X.quali
tree <- hclustvar(t(X.quanti))
plot(tree)
tree <- as.phylo(tree)
trait <- X.quanti$Fruity
names(trait) <- tree$tip.label
plot(contMap(tree,trait))

taco.dist <- dist(finalspecies)
taco.phylo <- hclust(taco.dist)
plot(taco.phylo)



new.data <- read.csv('kaggle_income.csv')
new.data$Lat
matched.data <- array(dim = c(nrow(tacos.trimmed1),2))
count <- 1
used <- array(dim = c(nrow(tacos.trimmed1),2))
for(i in 1:nrow(new.data)){
  lat <- new.data$Lat[i]
  lon <- new.data$Lon[i]
  for(j in 1:nrow(tacos.trimmed1)){
    latit <- tacos.trimmed1$latitude[j]
    longit <- tacos.trimmed1$longitude[j]
    if(abs(lat - latit) < .01 & abs(lon - longit) < .01){
      matched.data[count,1] <- lat
      matched.data[count,2] <- lon
      used[count,1] <- i
      used[count,2] <- j
      count <- count + 1
    }
  }
  
}


load("~/GitHub/datathon2019/tacos.trm2.merge.RData")
preProcess_missingdata_model <- preProcess(tacos.kmeans, method='knnImpute')
tacos.imputed <- predict(preProcess_missingdata_model, newdata = tacos.kmeans)


taco.phylo <- as.phylo(taco.phylo)
# taco.phylo$edge.length <- taco.phylo$edge.length / max(branching.times(taco.phylo))
conttrait <- finalspecies$beef
names(conttrait) <- taco.phylo$tip.label
smp <- contMap(taco.phylo,conttrait, ftype = 'off', legend = F, plot = T)
n<-length(smp$cols)
smp$cols[1:n]<-rainbow(n, end = 4/6)
plot(smp, legend = F,ftype = 'off')
gradientLegend(depth = .03, valRange = c(.24,2), side = 1, pos = .17, color = rainbow(n, end = 4/6))
legend(x = 'bottomleft', legend = '', title = '           Cont. trait value', bg="transparent", bty = 'n')
