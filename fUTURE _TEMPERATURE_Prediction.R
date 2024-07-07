rm(list = ls())
library(stars)      # To process the raster data
library(sf)         # To work with vector data
library(ggplot2)    # For plotting
library(patchwork)  # To combine different ggplot plots

#install.packages('stars', dependencies = TRUE, repos='http://cran.rstudio.com/')

file_path <- "C:/Users/ADMIN/Desktop/Bioclim"
raster::getData(name = 'worldclim', var = 'bio', res = 10,
                path = file_path)
raster::getData(name = 'CMIP5', var = 'bio', res = 10,
                rcp = 45, model = 'IP', year = 70,
                path = file_path)
annual_T <- stars::read_stars(paste0(file_path, "/wc10/bio1.bil"))
annual_T <- annual_T/10
annual_T_70 <- stars::read_stars(paste0(file_path, "/cmip5/10m/ip45bi701.tif"))
annual_T_70 <- annual_T_70/10

# The result, temp_colors, is a function with argument n for the number of
# colors.
temp_colors <- colorRampPalette(c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"))

nbreaks <- 20
{
  par(mfrow = c(1,2))
  plot(annual_T, main = "Annual temperature - 1960-1990",
       nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))
  plot(annual_T_70, main = "Annual temperature - RCP 4.5 projection for 2061-2080",
       nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))

  india_map <- rnaturalearth::ne_countries(country = "india", returnclass = "sf")
  # The precision has to be set to a value > 0 to resolve internal boundaries.
  st_precision(india_map) <- 1e9 # Required to
  india_map <- st_union(india_map)
  
  {
    par(mar = c(0,0,0,0))
    plot(india_map)
  }
}
#install.packages("rnaturalearth")
st_crs<-st_crs(india_map)

annual_T_SA <- annual_T[india_map]
# CRS for projected T and india map are the same (EPSG 4326) but the
# proj4string includes more details for annual_T_70. Thus, they have to
# be made identical before cropping.
st_crs(annual_T_70) <- st_crs(india_map)
annual_T_70_SA <- annual_T_70[india_map]

{
  par(mfrow = c(1, 2))
  plot(annual_T_SA, main = "Annual temperature - 1960-1990",
       nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))
  plot(main = "Annual temperature - RCP 4.5 projection for 2061-2080",
       annual_T_70_SA, nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))
}
annual_T_SA
names(annual_T_SA) <- "recent"
annual_T_SA$projected <- annual_T_70_SA$ip45bi701.tif
annual_T_SA$change <- annual_T_SA$projected  - annual_T_SA$recent
annual_T_SA
recent_T_plot <- ggplot() + 
  geom_stars(data = annual_T_SA) +
  scale_fill_gradientn(name = "Annual T [°C]",
                       colors = temp_colors(5),
                       limits = c(-7, 32),
                       na.value = "white") +
  geom_sf(data = india_map, fill = NA) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ggtitle("1960-1990") +
  theme_void() +
  theme(legend.position = "none")+
  coord_sf(lims_method = "geometry_bbox")
recent_T_plot


projected_T_plot <- ggplot() + 
  geom_stars(data = annual_T_SA["projected"]) +
  scale_fill_gradientn(name = "Annual T [°C]",
                       colors = temp_colors(5),
                       limits = c(-7, 32),
                       na.value = "white") +
  geom_sf(data = india_map, fill = NA) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ggtitle("b) 2061-2080 (projected)") +
  theme_void() +
  theme(legend.position = "bottom")+
  coord_sf(lims_method = "geometry_bbox")
projected_T_plot

projected_change_T_plot <- ggplot() + 
  geom_stars(data = annual_T_SA["change"]) +
  scale_fill_gradientn(name = "Change in T [°C]",
                       colors = temp_colors(5)[3:5],
                       limits = c(1, 5),
                       na.value = "white") +
  geom_sf(data = india_map, fill = NA) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ggtitle("c) Projected change") +
  theme_void() +
  theme(legend.position = "bottom")+
  coord_sf(lims_method = "geometry_bbox")
projected_change_T_plot


Final_Patch<-(recent_T_plot / projected_T_plot + plot_layout(guides = "keep")) | 
  projected_change_T_plot +
  theme(plot.margin = margin(c(0, 0, 0, 0))) +
  coord_sf(lims_method = "geometry_bbox")
Final_Patch


