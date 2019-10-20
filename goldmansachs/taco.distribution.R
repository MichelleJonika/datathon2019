#change the latitude and longitude to numeric instead of character
tacos.trimmed1$latitude <- as.numeric(tacos.trimmed1$latitude)
tacos.trimmed1$longitude <- as.numeric(tacos.trimmed1$longitude)
#subset the original dataset to have just latitude and longitude
tacos.points <- tacos.trimmed1[c(5:6)]
#remove the rows that are not in the United States
tp.trimmed <- tacos.points[-c(10093:10095,21016:21018,38006:38010,43943:43958,45836:45837,51536:51543),]
tp.trimmed <- tp.trimmed[-c(8325,30583,47567),]
#load in library for map
library(maps)
#create the map of the united states (filled with gray)
map('usa', fill = T, col = "gray70")
#do kmeans on the geographic locations of the tacos, 10 clusters, 
#25 random starts
colnames(tp.trimmed) <- c("Latitude", "Longitude")
kmeans <- kmeans(tp.trimmed, 
       centers = 10, nstart = 25)
#plot points from tacos.points onto the map of the US
plot_usmap(region = "states")
points(tp.trimmed$longitude, 
       tp.trimmed$latitude, 
       pch = 21, 
       col = kmeans$cluster,
       cex = 0.7)
#gives the kmeans cluster centers
kmeans$centers
#gives the count of data points in each cluster
table(kmeans$cluster)
library(ggplot2)
library(ggrepel)
library(mapproj)

us <- map_data('world',
               c('usa', 'canada', "hawaii", "mexico"))
ggplot()+
  geom_polygon(data=us, aes(x=long, y=lat, group = group), 
               colour="grey20", fill= "grey50")+
  geom_point(data = tp.trimmed, aes(x=longitude, y = latitude,  
                                    color= as.factor(tp.kmeans$`kmeans$cluster`)), 
             show.legend = F, cex=0.2, pch=1)+
  coord_map(projection = "mercator", xlim=c(-162, -60), ylim = c(10,62))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()

