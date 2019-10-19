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
                 "sriracha", "ranch", "crema", "red", "rojo", "verde", "guac", "refried", "black beans",
                 "pinto", "colorado", "wahoo", "mahi", "roughy", "whitefish", "tuna", "cajun", "haddock",
                 "cod", "salmon", "shrimp", "crab", "lobster", "pulled pork", "adobada", "pastor", "bacon",
                 "belly", "sausage", "sirloin", "steak", "cheesesteak", "shortrib", "rib", "primerib", "prime rib",
                 "short rib", "barbacoa", "carne asada", "suadero", "tempeh", "tofu", "seitan", "hamburger",
                 "ground beef", "kale", "flour", "crispy", "peanut", "soft", "egg","sauce", "tomatillo",
                 "chihuahua", "spaghetti", "almond"
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
traits.iter.mat <- cbind(traits.iter.mat,lapply(tacos.search, grepl,pattern = trait))
}

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