# Kernel Density Estimate for Bears in Southern Interior: -----------------
####### Here we conduct a kernel density estimate using our WARP conflict points within the Southern Interior Ecoprovince to compare
# the densities and overlap of black bears and grizzly bears - ideally to see if there are spatial patterns where they co-occur. This
# is to determine if we have support for people reporting bears as a collective more than as distinct species.


# Load Packages: ----------------------------------------------------------
library(sf)
#install.packages("maptools")
library(maptools)
library(raster)
#install.packages("spatstat")
library(spatstat)
library(tidyverse)
library(sp)
#install.packages("spatialEco")
library(spatialEco)
library(terra)

# Bring in Data: ----------------------------------------------------------
warp.crop.10km <- st_read("Data/processed/warp_crop_10km_buf.shp")
south.int.10k.buf <- st_read("Data/processed/SOI_10km_buf.shp")

# Filter Our Data: --------------------------------------------------------

  # Create a Black bears and Grizzly Population raster:
  # Crop our df to black bears and grizz seperately
black.bears <- warp.crop.10km %>% 
  dplyr::filter(warp.crop.10km$spcs_nm == "BLACK BEAR" )

grizz.bears <- warp.crop.10km %>% 
  dplyr::filter(warp.crop.10km$spcs_nm == "GRIZZLY BEAR" )

head(black.bears)
head(grizz.bears)

#b.bears.rast <- raster(black.bears)
#g.bears.rast <- raster(grizz.bears)

# Format all Data: --------------------------------------------------------
  # First, our boundary shapefile:
s <- south.int.10k.buf
w <- as.owin(s)
w.km <- rescale(w, 1000)

  # Next, our black bears points shapefile:
sw <- black.bears
bb.pts <- as.ppp(sw)
marks(bb.pts) <- NULL
#bb.pts <- rescale(bb.pts, 1000)
Window(bb.pts) <- w

  # Lastly, our grizzly bears points shapefile:
gw <- grizz.bears
gb.pts <- as.ppp(gw)
marks(gb.pts) <- NULL
#gb.pts <- rescale(gb.pts, 1000)
Window(gb.pts) <- w

# Visualize our Data Points -----------------------------------------------
  # Plot the points and boundary:
plot(gb.pts, main=NULL, cols=rgb(0,0,0,.2), pch=20)


# Density Based Analysis - Quadrat Density: -------------------------------

# BLACK BEARS:----------
Q.b <- quadratcount(bb.pts, nx= 6, ny=3) # assign quadrats

  # Plot our points with quadrats:
plot(bb.pts, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q.b, add=TRUE)  # Add quadrat grid

  # Compute the density for each quadrat
Q.bd <- intensity(Q.b)

  # Plot the density
plot(intensity(Q.b, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(bb.pts, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE) # add points
# The density values are reported as the number of points (stores) per square kilometer, per quadrat

  # Kernel Density Estimate:
K1.bb <- raster::density(bb.pts) # Using the default bandwidth
plot(K1.bb, main=NULL, las=1)
contour(K1.bb, add=TRUE)
title("Black Bear KDE for Southern Interior") 

  # Do this with a 50km bandwidth:
K2.bb <- density(bb.pts, bw="SJ") # Using a 50km bandwidth
plot(K2.bb, main=NULL, las=1)
contour(K2.bb, add=TRUE)
title("Black Bear KDE w/ 50km Bandwith for Southern Interior") 

  # Try out with different function:
K3.bb <- density(bb.pts, kernel = "disc", sigma=50) # Using a 50km bandwidth
plot(K3.bb, main=NULL, las=1)
contour(K3.bb, add=TRUE)
title("Black Bear KDE w/ 50km Bandwidth for Southern Interior") 

# GRIZZLY BEARS: ---------
Q.g <- quadratcount(gb.pts, nx= 6, ny=3) # assign quadrats

  # Plot our points with quadrats:
plot(gb.pts, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q.g, add=TRUE)  # Add quadrat grid

  # Compute the density for each quadrat
Q.gd <- intensity(Q.g)

  # Plot the density
plot(intensity(Q.g, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(gb.pts, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points
# The density values are reported as the number of points (stores) per square kilometer, per quadrat

  # Kernel Density Estimate:
K1.gb <- density(gb.pts) # Using the default bandwidth
plot(K1.gb, main=NULL, las=1)
contour(K1.gb, add=TRUE)
title("Grizzly Bear KDE for Southern Interior") # Add points

  # Do this with a 50km bandwidth:
K2.gb <- density(gb.pts, bw="SJ", sigma=2) # Using a 50km bandwidth
plot(K2.gb, main=NULL, las=1)
contour(K2.gb, add=TRUE)
title("Grizzly Bear KDE w/ 50km Bandwidth for Southern Interior") 

  # Try out with different function:
K3.gb <- density(gb.pts, kernel = "disc", sigma=50) # Using a 50km bandwidth
plot(K3.gb, main=NULL, las=1)
contour(K3.gb, add=TRUE)
title("Grizzly Bear KDE w/ 50km Bandwidth for Southern Interior") 



density_rep <- function(pts1, pts2, adj){
  dens1 <- density(pts1,  adjust=adj)
  dens2 <- density(pts2,  adjust=adj)
  rast1 <- rast(dens1)
  rast2 <- rast(dens2)
  corrast <- rasterCorrelation(rast1, rast2, type = "pearson")
}

cors <- lapply(c(0.25, 0.5, 1, 2, 4), function(x) density_rep(bb.pts, gb.pts, adj=x))
cor.stack <- rast(cors)
cor.stack.recl <- terra::ifel(cor.stack > 0.7, 1, 0)
# Make these into rasters: ------------------------------------------------
K1.bb.raster <- rast(K1.bb)
K2.bb.raster <- rast(K2.bb)
K1.gb.raster <- rast(K1.gb)
K2.gb.raster <- rast(K2.gb)


# Calculate the Correlation: ----------------------------------------------
#install.packages("spatialEco")
library(spatialEco)

bears.kde.corr <- rasterCorrelation(K1.bb.raster, K1.gb.raster, type = "pearson")
plot(bears.kde.corr)
contour(bears.kde.corr, add=TRUE)
title("Grizzly & Black Bear KDE Correlation for Southern Interior") 

bears.kde.50km.corr <- rasterCorrelation(K2.bb.raster, K2.gb.raster, type = "pearson")
plot(bears.kde.50km.corr)

  # Filter these for values less than 0.7:
bears.kde.corr[bears.kde.corr > 0.7] <- 1
bears.kde.corr[bears.kde.corr < 0.69] <- 0

plot(bears.kde.corr)
contour(bears.kde.corr, add=TRUE)
title("Grizzly & Black Bear KDE Correlation for Southern Interior") 


# Plotting the Correlation: -----------------------------------------------
  # Assign Projection to Rasters:
crs(cor.stack) <- CRS('+init=EPSG:3153')


  # Reproject the vector data:
b.bears.reproj <- st_make_valid(black.bears) %>% 
  st_transform(crs=crs(cor.stack[[3]]))
g.bears.reproj <- st_make_valid(grizz.bears) %>% 
  st_transform(crs=crs(cor.stack[[3]]))

  # Make these spatvectors & rasters:
g.bears.vect <- vect(g.bears.reproj)
b.bears.vect <- vect(b.bears.reproj)

bears.kde.corr.sr <- as(bears.kde.corr, "SpatRaster")

  # Match the extents
ext(bears.kde.corr.sr) <- ext(b.bears.vect) 

  # Plot all together:
png(file = "plots/KDE_update.png", bg = "transparent")
plot(cor.stack[[3]])
plot(b.bears.vect, pch=2, col = "red", add=TRUE) #this works...
plot(g.bears.vect, pch=19, col = "black",add=TRUE)
title("Grizzly & Black Bear KDE Correlation for Southern Interior") 
legend("topright",   # set position
       inset = c(0.025, 0.125), # Distance from the margin as a fraction of the plot region
       legend = c("Black Bears", "Grizzly Bears"),
       pch = c(2, 19),
       col = c("red","black"))
dev.off()

# Write as .tif files: ----------------------------------------------------
raster::writeRaster(K1.bb.raster, "Data/processed/black_bear_kde.tif")
raster::writeRaster(K2.bb.rastr, "Data/processed/black_bear_kde_50km.tif")
raster::writeRaster(K1.gb.raster, "Data/processed/grizz_bear_kde.tif")
raster::writeRaster(K2.gb.raster, "Data/processed/grizz_bear_kde_50km.tif")

raster::writeRaster(bears.kde.corr, "Data/processed/gbears_kde_correlation.tif")

