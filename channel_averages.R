library(magick)

channel_averages <- function(path_img, alpha = 0.4, ...) {
  img <- as.integer(image_data(image_read(path = path_img), ...))
  
  vec_rgb <- "names<-"(vapply(
    X = 1L:3L,
    FUN = function(i)
      median(img[, , i]) / 255,
    FUN.VALUE = double(length = 1L)
  ),
  c("r", "g", "b"))
  
  rgb(red = vec_rgb["r"], green = vec_rgb["g"], blue = vec_rgb["b"], alpha = alpha)

}

color <- channel_averages(path_img = "~/Downloads/VibrantColorR/logo.png")
hist(rnorm(100), col = color)
