library(tidyverse)

# Need to convert all jpgs to pngs
convert_jpg <- function(img) {
  library(jpeg)
  path <- paste0("Invasive_Plants/images/", img)
  img_new <-readJPEG(path)
  plant <- str_remove(img, ".jpg|.JPG")
  plant <- paste0(plant, ".png")
  path2 <- paste0("Invasive_Plants/images/png/", plant)
  library(png)
  writePNG(img_new, path2)
}


images <- list.files("Invasive_Plants/images", pattern = ".jpg|.JPG")

pngs <- map(images, convert_jpg)
