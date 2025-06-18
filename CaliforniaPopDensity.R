library(sf)
library(tigris)
library(ggplot2)
library(tidyverse)
library(rayshader)
library(stars)
library(MetBrewer)
library(colorspace)

# Kontur population density data for US
# This takes approx 2 mins to load
# Data downloaded from https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_US_20231101.gpkg.gz
# data <- sf::st_read("data/kontur_population_US_20231101.gpkg")

# Load the states
states <- tigris::states()

# Load the data that was calculated separately
load("data/state_ca.RData")

# Convert data to a matrix format for Rayshader
# First, define a bounding box that encompasses the data.
bbox <- st_bbox(state_ca)

# Then calculate the bottom left and bottom right
bottom_left <- st_point(c(bbox[["xmin"]], bbox[["ymin"]])) |>
  st_sfc(crs = 3857)

bottom_right <- st_point(c(bbox[["xmax"]], bbox[["ymin"]])) |>
  st_sfc(crs = 3857)

# Plot this to check the corners of the bbox
florida |>
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) |>
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

# Handle conditions of width or height being the logest side

if( width > height ) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}

# Convert to raster format to then convert to matrix
size <- 5000

florida_rast <- st_rasterize(state_florida, nx = floor(size * w_ratio), ny = floor(size * h_ratio))

matrix <- matrix(florida_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# Create a color palette

c1 <- met.brewer("OKeeffe2")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias=2)(256)
swatchplot(texture)

# Plot it!
rgl::rgl.close()

matrix |>
  height_shade(texture = texture) |>
  plot_3d(heightmap = matrix, zscale = 20, solid = FALSE, shadowdepth = 0)

render_camera(theta = -20, phi = 45, zoom = 0.8)

outfile <- "final_plot.png"

{
  start_time <- Sys.time()
  cat( crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target=outfile)
  }
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = 280,
    lightaltitude = c(20, 80),
    lightcolor = c(c1[2], "white"),
    lightintensity = c(600, 100),
    samples = 500,
    width = 6000,
    height = 6000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}
