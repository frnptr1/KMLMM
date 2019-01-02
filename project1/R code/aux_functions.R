load_install_packages <- function(packageName)
{
  if(!require(packageName, character.only=TRUE)){
    print(paste(packageName, "not exist, installation in process.." ))
    install.packages(packageName)
    if(!require(packageName, character.only=TRUE))
      print(paste("failed to install:", packageName ))
  }
}

load_packages <- function()
{
   load_install_packages("data.table")
   load_install_packages("magick")
   load_install_packages("FactoMineR")
   load_install_packages("jpeg")
   load_install_packages("stringr")
   load_install_packages("pixmap")
   load_install_packages("Rgb")
   load_install_packages("grid")
   load_install_packages("imager")
   load_install_packages("OpenImageR")
   load_install_packages("e1071")
   load_install_packages("foreach")
   load_install_packages("kernlab")
   load_install_packages("caret")
   load_install_packages("ggplot2")
   load_install_packages("parallel")
   load_install_packages("doParallel")
   load_install_packages("caTools")
   load_install_packages("RColorBrewer")
   #load_install_packages("match")
   #install.packages("match", dependencies=TRUE, repos='http://cran.rstudio.com/')
   
}


