
#Te instalas estas librerías

install.packages("emodnet.wfs")
install.packages("sf")

library(emodnet.wfs)
library(sf)


#https://emodnet.ec.europa.eu/en/emodnet-web-service-documentation
#De ese link encuentras la etiqueta para ver la categoría de las bases de datos

wfs <- emodnet_init_wfs_client("biology")

#Corres este código y te da las etiquetas. las layers son las bases de datos.
#Si abres los links de la página web te viene un documento que dice los nombres
#de los datasets y qué contiene
layers_info <- emodnet_get_wfs_info(wfs)
print(layers_info$layer_name) 

#Con esto descargas. Habrá que ver cuales son compatibles con csv y que tengan el
#mismo sistema de coordenadas. O si no mínimo que sea espacial para poder usar regiones

data_sf <- emodnet_get_layers(wfs = wfs,layers = "Species_gridded_abundances_2year",
                              crs = 4326, 
                              outputFormat = "csv")

head(data_sf)

