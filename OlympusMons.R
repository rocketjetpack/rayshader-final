library(terra)
library(rayshader)

dem <- rast("tiff/olympus_mons_clip.tif")
elev <- matrix(values(dem), nrow = ncol(dem), ncol = nrow(dem))
elev <- t(elev)[, nrow(elev):1]

shaded <- sphere_shade(elev, texture="desert")
shaded <- shaded %>% add_shadow(ray_shade(elev, zscale=50), 0.5)
rgl::open3d()
plot_3d(shaded, elev, zscale = 50, theta=135, phi=45)
render_snapshot()
