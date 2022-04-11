# https://api.spotify.com/v1
# 5XLXrm5JVMdOus1fWmTOFw

beats <- readRDS("beats.rds")
library(spotifyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# spotify wrapper for rlang https://www.rcharlie.com/spotifyr/index.html######
Sys.setenv(SPOTIFY_CLIENT_ID = 'b5d9bde4ece2478fbcce6d203790cab2')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '4ebd3a9a0ffb4df89a0ae4fc95211c96')
access_token <- get_spotify_access_token()
##############################################################################

beats2 <- subset(beats, select= c("track_id", "danceability", "energy", "instrumentalness", "liveness", "loudness", "speechiness", "tempo", "valence"))
beats2 <- unique(beats2, by = "track_id")
rownames(beats2) <- beats2$track_id
df <- scale(beats2[,2:9])

############ K-means https://uc-r.github.io/kmeans_clustering#################

#k2 <- kmeans(df, centers = 2, nstart = 25)
k3 <- kmeans(df, centers = 3, nstart = 25)
fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
#k4 <- kmeans(df, centers = 4, nstart = 25)
#k5 <- kmeans(df, centers = 5, nstart = 25)


#p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
#p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
#p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
#p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

#library(gridExtra)
#grid.arrange(p1, p2, p3, p4, nrow = 2)

# viendo estos resultados nos vamos a quedar con el modela que tiene 3 clusters, 
# ya que es el que mejor representa el dataset en grupos sin tener mayores overlaps,
# con 4 o mas clusters, tenemos que existen clusters que cubren gran parte de otros.

