library(terra)
library(rayshader)

dem <- rast("tiff/olympus_mons_clip.tif")
elev <- matrix(values(dem), nrow = ncol(dem), ncol = nrow(dem))
elev <- t(elev)[, nrow(elev):1]

shaded <- sphere_shade(elev, texture="desert")
shaded <- shaded %>% add_shadow(ray_shade(elev, zscale=50), 0.5)
rgl::open3d()
args(plot_3d)
plot_3d(shaded, elev, zscale = 50, theta=135, phi=45)
render_camera(theta = -20, phi = 25, zoom = 0.7, fov = 10)

render_highquality(
  filename = "OlympusMons.png",
  interactive = FALSE,
  lightdirection = 200,
  lightaltitude = c(20, 80),
  lightcolor = c("white", "white"),
  lightintensity =c(600, 100),
  samples = 200,
  width = 1500,
  height = 1500
)
