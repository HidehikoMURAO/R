
#install.packages("sf")
#install.packages("spData")
#install.packages("mapview")

library(sf)
library(spData)
library(mapview)

data(house, package="spData")#
house_sf <- st_as_sf(house)  #conver to sf format

######################## Mapping
mapview(house_sf,
        zcol="price",   # Variable name
        cex=3, 		# Size of the dotts
        lwd=0, 		# Width of frame borders
        at = c(-Inf, quantile(house_sf$price ,probs=seq(0.1,0.9,0.1)), Inf), #Break points for colors
        legend=T # If true ledgend is added
)

