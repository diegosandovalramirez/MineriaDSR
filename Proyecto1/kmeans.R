# K-means Diego Sandoval R.

beats <- readRDS("beats.rds")
library(secret)
library(spotifyr)   # Spotify wrapper
library(dplyr)
library(purrr)
library(knitr)
library(httr)       # HTTP requests
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(data.table)

# spotify wrapper for rlang https://www.rcharlie.com/spotifyr/index.html######
# client id y client secret
Sys.setenv(SPOTIFY_CLIENT_ID = "SPOTIFY_CLIENT_ID")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "SPOTIFY_CLIENT_SECRET")

# generación de tokens
access_token <- get_spotify_access_token()
auth_token <- get_spotify_authorization_code(scope = scope)
##############################################################################

#user_id = "xjkgrfqwqfbe92nuuvfu0r66f"

#pregunta al usuario su id de spotify, el nombre de la playlist a crear, 
#y el nombre la cancion a usar para crear la playlist

user_id <- readline(prompt = "Ingrese su user id: ")
nombre_play = readline(prompt="Ingrese nombre la playlist a crear: ")
selected_track = readline(prompt="Ingrese nombre de la cancion: ")

#Busqueda de spotifyr
search_track = search_spotify(selected_track, type = c("track"),
                              market = NULL, limit = 20, offset = 0, include_external = NULL,
                              authorization = get_spotify_access_token(),
                              include_meta_info = FALSE)

# Filtro de search_track a solo las variables que nos interesan
search_filtered = subset(search_track, select= c("name", "album.name", "uri", "id"))
print(search_filtered)

#Le pregunta al usuario cual cancion es la que quiere. Muchas tienen el mismo nombre
selected_search <- readline(prompt = "Seleccione numero de la canción: ")

#Extrae valores interesantes de la busqueda
selected_track = search_filtered[selected_search,3:4]
selected_id = pull(selected_track, var=2)
selected_uri = pull(selected_track, var=1)


## reducción de columnas a aquellas que nos interesan más. Los features.
beats2 <- subset(beats, select= c("track_uri", "danceability", "energy", "instrumentalness", "liveness", "loudness", "speechiness", "tempo", "valence"))

# cambio de nombre "track_uri" a "uri"
names(beats2)[1] <- 'uri'

# Eliminando todos los tracks que esten duplicados
beats2 <- unique(beats2, by = "uri")

## tomamos los features de la cancion seleccionada
temp_features = get_track_audio_features(selected_id, authorization = get_spotify_access_token())

## mismo cambio de nombre anterior
#names(temp_features)[13] <- 'uri'

## seleccionamos los mismos features que antes en la track seleccionada
track_features = subset(temp_features, select= c("uri", "danceability", "energy", "instrumentalness", "liveness", "loudness", "speechiness", "tempo", "valence"))

## unimos la base de beats.rds filtrada junto con la track que acabamos de 
## ingresar
beats3 <- rbind(beats2, track_features)

## Para no perder los uri al normalizar, los volvemos el nombre de la fila
rownames(beats3) <- beats3$uri

## escalamos los datos
df <- scale(beats3[,2:9])


############ K-means https://uc-r.github.io/kmeans_clustering#################

## Prueba de varios kmeans
#k2 <- kmeans(df, centers = 2, nstart = 25)
k3 <- kmeans(df, centers = 3, nstart = 25)
#fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
#k4 <- kmeans(df, centers = 4, nstart = 25)
#k5 <- kmeans(df, centers = 5, nstart = 25)

##Visualizacion de varios kmeans
#p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
#p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
#p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
#p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

#library(gridExtra)
#grid.arrange(p1, p2, p3, p4, nrow = 2)

# viendo estos resultados nos vamos a quedar con el modela que tiene 3 clusters, 
# ya que es el que mejor representa el dataset en grupos sin tener mayores overlaps,
# con 4 o mas clusters, tenemos que existen clusters que cubren gran parte de otros.

# 3 cluster en este dataset significa que podemos clasificar todas las
# canciones en 3 tipos. Lo que puede resultar en muchos generos y muchos
# estilos esten bajo la misma categoria (cluster). Sería necesario una
# validacion del modelo para ver si efectivamente un kmeans de 3 clusters
# es una buena representacion de la libreria de spotify, lo cual es debatible y
# se escapa del scope del proyecto. Por ahora, nos quedamos con que el algoritmo
# de kmeans puede que no sea el indicado para esta tarea por la complejidad
# de los datos y su alta dimensionalidad. Intentar crear clusters esfericos
# podría estar forzando clasificaciones que realmente pueden no ser pertinentes.

## Ordenamos las canciones por clusters
cluster_tracks=order(k3$cluster)
## pasamos los datos de las canciones por cluster a un dataframe
cluster_data = data.frame(beats3$uri[cluster_tracks],k3$cluster[cluster_tracks])

## Reconocemos el cluster seleccionado
selected_cluster = cluster_data[cluster_data$beats3.uri.cluster_tracks.== selected_uri, 2]

##creamos un dataframe con las canciones del cluster seleccionado
playlist_data = cluster_data[cluster_data$k3.cluster.cluster_tracks.== selected_cluster, ]
##creamos una nueva playlist con aproximadamente 3hrs de reproduccion (45 canciones)
## de manera aleatoria dentro del cluster. Hay ventajas y desventajas con hacer
##esto. Una ventaja es que por probabilidad, incluso con canciones similares,
## no obtendremos listas similares entre si. La desventaja es que se pueden 
## tomar valores extremos del cluster que puede que no representen bien a la
## cancion o se sienta que no pertenezcan a la recomendacion. 
new_playlist = playlist_data[sample(nrow(playlist_data), 45), ]
new_playlist[nrow(new_playlist) + 1,] = c(selected_uri,selected_cluster)

## Con spotifyr creamos una nueva playlist publica llamada Proyecto1
playlist = create_playlist(user_id, nombre_play, public = TRUE, collaborative = FALSE,
                description = NULL, authorization = get_spotify_authorization_code())

## Tomamos la playlist que creamos con las canciones aleatorias del cluster 
## y las volvemos un vector para crear el request y agregarlas a nuestra
##playlist.
playlist_vector = as.character(new_playlist[,1])

## Del vector de la playlist, obtenemos un string, pre-formateado de una
## manera oportuna para el futuro json
uris = paste0('\"',playlist_vector, collapse = '\",', recycle0 = TRUE)


### Parsing para el request en formato JSON, esto va acompañando las URIs
preData = '{
  "uris": [
'

## Por como funciona el paste en rstudio, es necesario explicitar una track,
## de caso contrario, la ultima track llevara un espacio haciendo que el ultimo
## URI sea invalido, botando todo el request.

postData ='"
  ],
  "position": 0
}'


## como cat() tecnicamente no devuelve ningun valor, y necesitamos esos
## saltos de linea, usamos capture.output() para lograr tener cada
## nueva linea en una fila independiente. Luego usamos un paste0() con
## collapse para lograr guardar el formato que nos dio originalmente cat()

actualData <- paste0(preData,uris,postData)
actualData <- paste0(actualData, collapse = '\n')

## urlapi es la base del endpoint para agregar canciones a una playlist
urlapi = paste("https://api.spotify.com/v1/playlists/",playlist$id,"/tracks",sep = "")

## spotifyr no logra hacer un buen parsing de la info, por lo que siempre queda
## en bad requests. Por lo que tuve que manejar el request por mi cuenta.
## es un simple POST() que me tomo mas tiempo configurar de lo que estoy
## dispuesto a admitir.

POST(url = urlapi,config(content_type_json(),token = get_spotify_authorization_code()),body = actualData)
