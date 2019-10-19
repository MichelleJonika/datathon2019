data <- read.csv('just tacos and burritos.csv')
x <- head(data)
x
data <- data[,1:26]
data <- cbind(data, 'beef')

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
vurritovec <- vector(length = nrow(data))
for(i in 1:nrow(data)){
  if(grepl('taco', tolower(as.character(data$menus.name[i])))){
    tacovec[i] <- 1
  }else{tacovec[i] <- 0}
  if(grepl('burrito', tolower(as.character(data$menus.name[i])))){
    burritovec[i] <- 1
  }else{burritovec[i] <- 0}
}