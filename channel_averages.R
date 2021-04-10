library(magick)

channel_averages <- function(path_img, alpha = 0.4, probs = 0.75, ...) {
  
  is_hex <- grepl("^#(\\d|[a-f]){6,8}$", path_img, ignore.case = TRUE)
  
  if(is_hex) path_img
  else {
    img <- as.integer(image_data(image_read(path = path_img)))
    
    vec_rgb <- "names<-"(vapply(
      X = 1L:3L,
      FUN = function(i)
        quantile(img[, , i], probs = probs, ...) / 255,
      FUN.VALUE = double(length = 1L)
    ),
    c("r", "g", "b"))
    
    rgb(
      red = vec_rgb["r"],
      green = vec_rgb["g"],
      blue = vec_rgb["b"],
      alpha = alpha
    )
  }
}

# .book .book-summary ul.summary li a:hover, .book .book-summary ul.summary li.active > a { color: #000000; background: 0 0; text-decoration: none; background-color: #FFAF24; }
#     
# .book .book-summary ul.summary li a:hover { background-color: #aaaaaa; color: #000000; }
      
color <- channel_averages(path_img = "/home/prdm0/Downloads/VibrantColorR/logo.png")
pie(1, col = color, labels = NA)
