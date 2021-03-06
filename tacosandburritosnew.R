data <- read.csv('goldmansachs/just tacos and burritos.csv')
x <- head(data)
x
data <- data[,1:26]

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
  all.desc <- paste(all.desc, as.character(data$menus.description[i]))
}

diff.ing <- strsplit(all.desc, split = ',')


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

## compare burrito/taco vector with the menu name to make sure it works 
name<-data$menus.name
burritocompare<-cbind.data.frame(data$menus.name,burritovec)

-## family
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

<<<<<<< HEAD:tacosandburritosnew.R
## look for ingerdients in menu name column 
### beef 
beefvec <- vector(length = nrow(data))
for(i in 1:nrow(data)){
  if(grepl('beef', tolower(as.character(data$menus.name[i])))){
    beefvec[i] <- 1
  }else{beefvec[i] <- 0}
}
### chicken 
chickenvec <- vector(length = nrow(data))
for(i in 1:nrow(data)){
  if(grepl('chicken', tolower(as.character(data$menus.name[i])))){
    chickenvec[i] <- 1
  }else{chickenvec[i] <- 0}
}
=======
ingredients <- data.frame(meatvec, cheesevec, seavec, rice, vegevec, saucevec ,# pico, sourcream, chipotle, chimichurri, sriracha, ranch, crema, red,
                          ## salsa, chileverde, guac
                          beans , # refried, black, pinto, colorado
                          ## ingredients
                          fish ,# wahoo, mahi, white, tuna, cajun, haddock,cod, salmon, roughy
                          othersea , # shrimp, crab, lobster
                          pork , # pulled, adobada, pastor, bacon, belly, sausage, sirloin
                          beef , # steak, cheesesteak, shortrib, primerib, barbacoa, carneasada, suadero,
                          vegesubs , # tempeh
                          onion ,
                          cheddar ,
                          lettuce,
                          refried,
                          tomato ,
                          chicken ,
                          pico ,
                          wahoo ,
                          avocado ,
                          cabbage ,
                          pulledcarnitaskalua ,
                          steak ,
                          shrimp ,
                          corntortilla ,
                          black ,
                          rice ,
                          pinto ,
                          potatoes ,
                          mushrooms ,
                          kale ,
                          cheesesteak ,
                          spaghetti ,
                          mahi ,
                          shortrib ,
                          primerib ,
                          adobada ,
                          barbacoa ,
                          carneasada ,
                          american ,
                          sourcream ,
                          peppers ,
                          flourtortilla ,
                          colorado ,
                          chipotle ,
                          jack ,
                          white ,
                          tuna ,
                          crab ,
                          olive ,
                          pastor ,
                          cajun ,
                          colby ,
                          haddock ,
                          cactus,
                          lobster,
                          cod ,
                          suadero ,
                          jicama ,
                          bacon ,
                          tempeh ,
                          fresco ,
                          chimichurri ,
                          salmon ,
                          quinoa ,
                          cucumber ,
                          carrot ,
                          sriracha ,
                          roughy ,
                          corn ,
                          pacilla ,
                          belly ,
                          sausagechorizo ,
                          ranch ,
                          crema ,
                          sirloin ,
                          redrojo ,
                          salsa ,
                          chileverde ,
                          guac ,
                          kimchi)
>>>>>>> 49204b5c900491917b16729512cd1dbbfd2377f3:tacosandburritos.R
