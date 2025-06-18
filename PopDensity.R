library(sf)
library(tigris)
library(ggplot2)
library(tidyverse)
library(rayshader)
library(stars)
library(MetBrewer)
library(colorspace)
library(magick)
library(raster)
library(terra)

STATE_NAME <- "Florida"

# Kontur population density data for US
# This takes approx 2 mins to load
# Data downloaded from https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_US_20231101.gpkg.gz
# data <- sf::st_read("data/kontur_population_US_20231101.gpkg")

# Load the states
states <- tigris::states()

# Filter to Florida
state_data <- states |>
  filter(NAME == STATE_NAME) |>
  st_transform(crs = 3857)

# Plot the state data
state_data |>
  ggplot() +
  geom_sf()

# Create an intersection of the national data
# bounded to the geometry of Florida
#state_florida <- sf::st_intersection(data, florida)

# This takes forever to calculate, so lets waste
# some disk space by saving it so that we can reload
# it later on.
#save(state_florida, file = "data/state_florida.RData")

# Load the data if needed
load("data/state_florida.RData")

# Convert data to a matrix format for Rayshader
# First, define a bounding box that encompasses the data.
bbox <- st_bbox(state_florida)

# Then calculate the bottom left and bottom right
bottom_left <- st_point(c(bbox[["xmin"]], bbox[["ymin"]])) |>
  st_sfc(crs = 3857)
bottom_right <- st_point(c(bbox[["xmax"]], bbox[["ymin"]])) |>
  st_sfc(crs = 3857)
top_left = st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>% 
  st_sfc(crs = 3857)
top_right = st_point(c(bbox[["xmax"]], bbox[["ymax"]])) %>% 
  st_sfc(crs = 3857)

# Plot the boundaries of the box
state_data |>
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left, color = "red") + 
  geom_sf(data = bottom_right, color = "red") +
  geom_sf(data = top_left, color = "red") +
  geom_sf(data = top_right, color = "red")

# Calculate the aspect ratio of the box
width <- st_distance(bottom_left, bottom_right)
height <- st_distance(bottom_left, top_left)

if ( width > height ){
  w_ratio <- 1
  h_ratio <- as.numeric(height / width)
} else {
  w_ratio <- as.numeric(width / height)
  h_ratio <- 1
}

size <- 2000

state_rast <- st_rasterize(state_florida,  nx = floor(size * w_ratio), ny = floor(size * h_ratio))
matrix <- matrix(state_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

plot <- function(matrix, outline, raster, zscale = 100, size = 400, sample_count = 20, outfile="temp_render.png") {
  c1 <- met.brewer("OKeeffe2")
  texture <- grDevices::colorRampPalette(c1, bias=2)(256)
  
  matrix |>
    height_shade(texture = texture) |>
    plot_3d(heightmap = matrix, zscale = zscale, solid = FALSE, shadowdepth = 0)
  render_camera(theta = -20, phi = 45, zoom = 0.8, fov = 10)
  
  #lightcolor = c(c1[2], "white"),
  
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = 200,
    lightaltitude = c(20, 80),
    lightcolor = c(c1[2], "white"),
    lightintensity =c(600, 100),
    samples = sample_count,
    width = size,
    height = size
  )
  
  img <- image_read(outfile)
  label_text <- "Florida Population Density"
  
  img_labelled <- image_annotate(
    img,
    text = label_text,
    size = floor(size / 25),
    color = "#555",
    font = "Cambria",
    gravity = "north",
    location = "+0+20",
    boxcolor = "transparent"
  )
  
  img_labelled <- image_annotate(
    img_labelled,
    text = "2023 Population Reported by Kontur",
    size = floor(size / 35),
    color = "#555",
    font = "Cambria",
    gravity = "south",
    location = "+0+20",
    boxcolor = "transparent"
  )
  
  image_write(img_labelled, path=outfile)
}

plot(matrix, state_rast, outline, size = 6000, sample_count = 500)
