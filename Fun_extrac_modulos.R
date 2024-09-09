
library(tidyverse)
library(haven)
library(dplyr)


setwd("D:/Documents/GitHub/INEI_ENAHO")
base_path <- "D:/Documents/GitHub/INEI_ENAHO"

modules <- c("module 01 Clase")
years <- 2017:2018




summarize_stata_files <- function(modules, years, base_path) {
  results <- list()
  
  for (m in modules) {
    for (t in years) {
      # Construccion de los directorios especificos por anho
      year_dir <- file.path(base_path, m, as.character(t))
      
      if (!is_directory(year_dir)) {
        warning(paste("Directorio no existe o no es accesible:", year_dir))
        next
      }
      
      # Construccion de la direccion completa
      file_path <- file.path(year_dir, paste0(t, ".dta"))
      
      if (!is_readable(file_path)) {
        warning(paste("Archivo no existe o no es leible:", file_path))
        next
      }
      
      data <- tryCatch({
        read_dta(file_path)
      }, error = function(e) {
        warning(paste("Error cargando el archivo:", file_path, "\n", e))
        return(NULL)
      })
      
      if (is.null(data)) {
        next
      }
      
      summary <- summary(data)
      
      results[[paste0(m, "_", t)]] <- list(
        "file_path" = file_path,
        "summary" = summary
      )
    }
  }
  
  return(results)
}


# Run the function
results <- summarize_stata_files(modules, years, base_path)