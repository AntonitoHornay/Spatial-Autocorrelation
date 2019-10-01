## Libraries

library(spdep)
library(maptools)
library(DT)
library(sf)
library(ggplot2)
library(leaflet)
library(tmap)
library("sp")
library("rgdal")
library("rgeos")

## Set directory
setwd("E:/ADI BUANA/SEMESTER 7/PETA JATIM 2017 AKB")
data <- read.csv("data spasial.csv")
gambar <- readShapeSpatial("jatim.shp")
mydata <- readr::read_csv("E:/ADI BUANA/SEMESTER 7/PETA JATIM 2017 AKB/data spasial.csv")

map <- st_read("E:/ADI BUANA/SEMESTER 7/PETA JATIM 2017 AKB/jatim.shp", stringsAsFactors = FALSE)
names(gambar)

## Statistik deskriptif
summa <- summary(gambar$IPM)
print(summa)

standar <- sd(gambar$IPM)
standar

str(map)

map_dan_data <- merge(map,mydata)

str(map_dan_data)

## Persebaran IPM Jatim

gplot(map_dan_data) + geom_sf(aes(fill = IPM)) + 
  geom_sf_label(aes(label = NAMA_KAB), label.size = 0.20, position = "identity") + 
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Persebaran IPM di Jawa Timur")

ggplot(map_dan_data) + geom_sf(aes(fill = IPM)) + 
  scale_fill_gradient(high = "#56B1F7", low = "#132B43")

tm_shape(map_dan_data) + 
  tm_polygons("IPM", id = "NAMA_KAB", palette = "Greens")

tm_shape(map_dan_data) + tm_fill("IPM", id = "NAMA_KAB", 
                                 style = "quantile", n = 4,  palette = "Paired")


tm_shape(map_dan_data) + tm_fill("IPM", id = "NAMA_KAB", style = "quantile", n = 4, 
                                 palette = "Accent") + tm_borders(alpha = .4) 


tm_shape(map_dan_data) + tm_fill("IPM", id = "NAMA_KAB", 
                                 style = "quantile", n = 4,  palette = "Accent") + 
  tm_borders(alpha = .6) + tm_compass()



tm_shape(map_dan_data) + tm_fill("IPM", id = "NAMA_KAB", style = "quantile", n = 4,  
                                 palette = "Accent") + tm_borders(alpha = .6) + 
  tm_layout(title = "East Java, Indonesia", 
            legend.text.size = 1.1, legend.title.size = 1.4, 
            legend.position = c("right", "top"), frame = FALSE) 

tmap_mode("view")

tmap_last()


tm_shape(map_dan_data) +
  tm_dots(title = "IPM East Java", border.col = "black",
          border.lwd = 0.1, border.alpha = 0.2,
          col = "IPM", id = "NAMA_KAB", style = "quantile", palette = "Reds")


tm_shape(map_dan_data) + 
  tm_bubbles(size = "IPM", id = "NAMA_KAB", 
             title.size = "IPM East Java", border.col = "black", 
             border.lwd = 0.1, border.alpha = 0.4, legend.size.show = TRUE)


tm_shape(map_dan_data) + tm_fill("IPM", id = "NAMA_KAB", 
                                 palette = "Reds", style = "quantile", 
                                 n = 4, title = "IPM East Java") + tm_borders(alpha=.5)


### Menampilkan Peta Jatim
plot(gambar, main="Peta Jawa Timur", axes=T)


queen.c <- poly2nb(gambar, row.names = gambar$NAMA_KAB, queen = T) 
summary(queen.c)

str(queen.c)

plot(gambar, col="brown", border = "black",lwd="1")  

plot(queen.c, coordinates(gambar), add = TRUE, lwd = "2", col = "blue")

## Standarisasi matriks bobot queen

queen.wl <- nb2listw(queen.c, style = "W")  
summary(queen.wl)


## Bobot nilai W standarisasi
queen.mw = nb2mat(queen.c, style = "W")   
print(queen.mw, digits = 3)

## Plot matrik bobot W Queen
plot(gambar, border = "black")
plot.queen <- plot(queen.c, coordinates(gambar), add = TRUE, col = "red")


##  Moran's Scatter Plot
r.queen.s = mat2listw(queen.mw, style = "W")
y = gambar$IPM
moran.plot(y, r.queen.s, main = "Scatter Plot Global Moran's I")

## Morans test
moran.test(gambar$IPM, queen.wl)

moran <- moran.plot(gambar$IPM, listw = nb2listw(queen.c, style = "W"))

globalmorans <- moran.test(y, listw = queen.wl, alternative = "two.sided")
print(globalmorans)

## Local Moran's
localmorans = localmoran(y, 
                         listw = queen.wl, 
                         alternative = "two.sided")

print(localmorans, digits = 2)


nilai_moran_lokal <- localmoran(x = gambar$IPM,
                                listw = nb2listw(queen.c, style = "W"), 
                                alternative = "two.sided")

print(nilai_moran_lokal, digits = 2)

### Visualisasi Local Moran's di Peta

moran.mapa <- cbind(gambar, nilai_moran_lokal)

# maps the results
tm_shape(moran.mapa) + tm_fill(col = "Ii", style = "quantile",
                               title = "local moran statistic")


## LISA

kuadran <- vector(mode="numeric",length=nrow(nilai_moran_lokal))

# centers the variable of interest around its mean
ipm_klasifikasi <- gambar$IPM-mean(gambar$IPM)
# centers the local Moran's around the mean
I_moran_local <- nilai_moran_lokal[,1] - mean(nilai_moran_lokal[,1])


# significance threshold
signif <- 0.5
# builds a data quadrant
kuadran[ipm_klasifikasi > 0 & I_moran_local>0] <- 4
kuadran[ipm_klasifikasi < 0 & I_moran_local<0] <- 1
kuadran[ipm_klasifikasi < 0 & I_moran_local>0] <- 2
kuadran[ipm_klasifikasi > 0 & I_moran_local<0] <- 3
kuadran[nilai_moran_lokal[,5]>signif] <- 0


brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")

plot(gambar,border="lightgray",
     col=colors[findInterval(kuadran,
                             brks,all.inside=FALSE)])

box()

legend("topleft",legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")

# creates centroid and joins neighbours within 0 and x units
nbb <- dnearneigh(coordinates(gambar),0,38)
# creates listw
nb_lww <- nb2listw(nbb, style = 'B')

# plot the data and neighbours
plot(gambar, border = 'lightgrey')
plot(nbb, coordinates(gambar), add=TRUE, col = 'red')


# compute Getis-Ord Gi statistic
local_gg <- localG(gambar$IPM, nb_lww)
local_gg <- cbind(gambar, as.matrix(local_gg))
local_gg

## Peta Penyebaran IPM
names(local_gg)[6] <- "Nilai IPM"
tm_shape(local_gg) + tm_fill("Nilai IPM", palette = "RdBu", style = "pretty") +
  tm_borders(alpha=.4)


