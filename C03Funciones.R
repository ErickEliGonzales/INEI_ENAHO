
#### INEI-2024: Programa de Extensión Universitaria (Cienia de Datos) ####

#### TEMA 3: Funciones=========================================================
#### Profesores:   ARMAS Carmen, 
#                  DELGADO Augusto, 
#                  ROMERO Vilma,  
#                  Ysique Mayté

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Las funciones nos permiten automatizar tareas comunes o repetitivas,
# evitando el uso del copy-paste y reduciendo errores.

# Ventajas:
# a) Nombres intuitivos: Podemos dar nombres descriptivos a las funciones.
# b) Cambio de inputs en un solo lugar: Permite cambiar fácilmente los inputs.
# c) Reduce errores: Menos probabilidad de errores en comparación con copy-paste.

# Requisito: Usar funciones cuando hay una acción repetida más de tres veces.

#### C03: Funciones ================================================

#### Ejemplo 3-01: funciones =====================================================

# Ejemplo B?sico - Reescalando Variables
# Vamos a crear un dataframe aleatorio con cuatro columnas.
n <- 20
df <- data.frame(a = rnorm(n), 
                 b = rnorm(n), 
                 c = rnorm(n), 
                 d = rnorm(n))

# Reescalamos las variables para que están entre 0 y 1.
df$a <- (df$a - min(df$a, na.rm = TRUE)) / (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / (max(df$b, na.rm = TRUE) - min(df$b, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

# Repetir este proceso para n variables puede ser tedioso y propenso a errores.

# Que obtenemos con esta funcion?
(df$a - min(df$a, na.rm = TRUE)) /
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))

(df$a - mean(df$a, na.rm = TRUE)) / (sd(df$a, na.rm = TRUE))

# Cuantos inputs necesitamos para obtener resultados arriba?

# Usamos la función range para encontrar el mínimo y máximo de una variable.
x <- df$a
rango <- range(x, na.rm = TRUE, finite = TRUE)

# Podemos usar estos valores para reescalar:
(x - rango[1]) / (rango[2] - rango[1])

# bonus: Para ver si dos variables son iguales: identical(x,y)

#Exploremos las posibilidades, usemos la funcion range
range(x, na.rm = TRUE, finite = TRUE)

rango <- range(x, na.rm = TRUE, finite = TRUE)
rango[1]
rango[2]

# Entonces, podemos crear el re-escalamiento usando la funcion range
(x - rango[1]) / (rango[2] - rango[1])


#### C03-Creando una funcion====================================================

# Ahora, vamos a crear nuestra funcion:

escalamiento01 <- function(x){
  rango <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rango[1]) / (rango[2] - rango[1])
}

escalamiento01(c(1,2,3,4,5))
escalamiento01(rnorm(100,mean = 5, sd = 3))
escalamiento01(c(1,2,3,4,NA,5))
escalamiento01(c(1,2,3,4,NA,5,-Inf,NaN))

# Aplicamos la funci?n a las columnas del dataframe.
df$a <- escalamiento01(df$a)
df$b <- escalamiento01(df$b)
df$c <- escalamiento01(df$c)
df$d <- escalamiento01(df$d)

estandarizacion <- function(x){
  media <- mean(x, na.rm = TRUE)
  dsv <- sd(df$a, na.rm = TRUE)
  (x-media) / dsv
}

estandarizacion(c(1,2,3,4,5))


# RECUERDE: Para las funciones debe, elegir un nombre adecuado,
# Identificar los inputs necesarios, escribir las instrucciones en el 
# cuerpo de la funcion, i.e. entre {} 

#### Ejemplo 3-02: NAs counter=====================================================

# Vamos a crear una funcion q captura el numero de NAs en ambos
sum_na <- function(x, y) {
  sum(is.na(x) + is.na(y))
}

# Inputs de ejemplos
x <- c(1:10, 4, NA, 30, NA, 15)
y <- c(1:5, NA, 5:1, NA, 3, 4, NA)

sum_na(x, y)


#### Ejemplo 3-03: NA finder ======================================================
# Funcion para capturar la posicion del primer NA en los vectores
primer_na <- function(x, y) {
  total_na <- is.na(x) | is.na(y)
  primer_na_indice <- which(total_na)[1] #captura el primer TRUE
  return(primer_na_indice)
}

ultimo_na <- function(x, y) {
  total_na <- is.na(x) | is.na(y)
  ultimo <- length(total_na)
  primer_na_indice <- which(total_na)[ultimo] #captura el primer TRUE
  return(primer_na_indice)
}

ultimo_na(x,y)

primer_na(x, y)


#### ED3-01: Crear su propia funcion =========================================== 
#hacemos lo mismo con la funcion de estandarizacion

#### Recordar: Estructuras Basicas ===========================================

#### Ejemplo 3-04: loops ==========================================================
calificaciones <- c(85, 92, 38, 35, 89, 90, 55, 73)

# Initializar las variables
aprobado <- NA
distancia_media <- numeric(length(calificaciones))

# Calcular la media del vector
media <- mean(calificaciones)

# Loop a través de un vector
for (i in seq_along(calificaciones)) {
  # Crear variable dummy
  if (calificaciones[i] >= 60) {
    aprobado[i] <- 1
  } else {
    aprobado[i] <- 0
  }
  
  # Calcular la distancia respecto a la media
  distancia_media[i] <- calificaciones[i] - media
}

# Crear un data frame para guardar los resultados
resultado <- data.frame(calificaciones = calificaciones,
                        aprobado = aprobado,
                        distancia_media = distancia_media)

# Imprimir el resultado
print(resultado)

#### ED3-02: Tarea loops ======================================================= 

 


# (1) Crear una una funcion que agregue una curva de puntos de +10 puntos para los
# los alumnos que tengan una nota menor de 60.



# (2) Crear una una funcion que agregue una curva de puntos de +10 puntos para
# todos los alumnos. Incorpore la condicion que no puede haber una nota superior
# a 100 puntos.

curva_2 <- function(c){
  for(i in 1:length(c)){
    if ((c[i]+10)<100){
      c[i] <- c[i]+10
    } else {next}
  }
  return(c)
}

curva2 <- curva_2(notas)

data_fr <- data.frame(notas,curva_puntos,curva2)

# (3) Crear una una funcion que haga una curva proporcional donde la mayor nota
# de la clase se transforme en 100 puntos y los demas se reescalen en base a esta.

#### Ejemplo 3-05: Funciones con strings ==========================================

## Cancion los Elefantes ##

library(magrittr)

# Funcion que nos retorna los versos de la cancion
elefante_verso <- function(n) {
  if (n == 1) {
    verso <- paste(n, "elefante se balanceaba sobre la tela de una ara�a,",
                   "cuando ve�an que resist�a, fueron a llamar otro elefante.\n")
  } else {
    verso <- paste(n, "elefantes se balanceaban sobre la tela de una ara�a,",
                   "cuando ve�an que resist�aa, fueron a llamar otro elefante.\n")
  }
  return(verso)
}

elefante_verso(5)

# Funcion que nos retorna la cancion completa basada en el numero de elefantes.
elefante_song <- function(n) {
  song_text <- sapply(1:n, elefante_verso)
  return(cat(song_text, sep = "\n"))
}

# LAs siguientes funciones siren para concatenar
# cat : solo imprime
# paste : guarda la informaci�n

pensamiento <- "Peru va ganar hoy" 
resultado <- "Peru metio dos goles"   

GuardarResultado <- paste(pensamiento,resultado)
GuardarResultado <- cat(pensamiento,resultado) #Se guarda como NULL


# Ejemplo usando 10 elefantes.
elefante_song(10)


#### C03-Global and Local Variables ============================================

# Global variable
x <- 10  # Se encuentra en mi ambiente de trabajo

mi_funcion <- function() {
  # Variable local
  y <- 20 #Solo sera usda cuando la funcion sea llamada
  print(paste("Dentro de la funcion, x =", x))
  print(paste("Dentro de la funcion, y =", y))
}

mi_funcion()

# Fuera de la funcion
print(paste("Fuera de la funcion, x =", x))
# Esta linea siguiente produce un error porque y no es una variable global
print(paste("Fuera de la funcion, y =", y)) #Esto da error


#### C03-Data Wrangling ========================================================

library(tidyverse)
library(haven)
library(dplyr)

setwd("D:/Desktop/R/INEI")

data <- read_dta("5ta_Clase/module 01/2018/2018.dta")

# Funcion para revisar si el directorio existe
is_directory <- function(x) {
  info <- file.info(x)
  !is.na(info$isdir) && info$isdir
}

# Funcion para revisar si el archivo es leible
is_readable <- function(x) {
  !is.na(file.access(x, 4)) && file.access(x, 4) == 0
}

#### C03-Datos Reales del INEI =================================================

#Definiendo los modulos y a?os con los que deseamos trabajar
modules <- c("5ta_Clase/module 01 Clase")
years <- 2017:2018

## Paso 1: Defininiendo el directorio basico ##
base_path <- "D:/Desktop/R/INEI"

## Paso 2: Creando la lista ##
# Creando listas vacias para completarlas despues
results <- list()

## Paso 3: Doble loop ##
#Veamos si los doble loops funcionan bien: 
for (m in modules) {
  for (t in years) {
    print(paste("Procesando el", m, "para el a?o", t))
  }
}

## Paso 4: Construccion de las direcciones ##

# Comprobemos como funciona los directorios para un modulo y año particular
module <- "5ta_Clase/module 01 Clase"
year <- 2018
year_dir <- file.path(base_path, module, as.character(year))
print(year_dir)

# Ahora preparemos la direccion completa del archivo STATA
file_path <- file.path(year_dir, paste0(year, ".dta"))
print(file_path)

## Paso 5: Revisando los archivos ##

# Chequeando si el directorio existe
if (!is_directory(year_dir)) {
  warning(paste("Directorio no existe o no es accesible:", year_dir))
} else {
  print(paste("Directorio existe:", year_dir))
}

# Chequeando si el archivo es leible
if (!is_readable(file_path)) {
  warning(paste("Archivo no leible o no existe:", file_path))
} else {
  print(paste("Archivo es leible:", file_path))
}


## Paso 6: Leyendo archivos STATA ##
# Leamos los archivos STATA
data <- tryCatch({
  read_dta(file_path)
}, error = function(e) {
  warning(paste("Error leyendo el archivo:", file_path, "\n", e))
  return(NULL)
})

# Chequeando si los archivos fueron efectivamente cargados
if (is.null(data)) {
  print("Fallo para leer los datos.")
} else {
  print("Datos leidos exitosamente.")
}


## Paso 7: datos ##
# Resumiendo los datos
if (!is.null(data)) {
  data_summary <- summary(data)
  print(data_summary)
}


## Paso 8: listas ##
# Guardando los datos en una lista
results[[paste0(module, "_", year)]] <- list(
  "file_path" = file_path,
  "summary" = data_summary
)

#### C03-Funcion Completa ======================================================

## Paso 9: Todo junto ##
# Funcion completa para importar y leer archivos STATA
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

# Print the summaries
for (name in names(results)) {
  result <- results[[name]]
  cat("Resumen para el archivo:", result$file_path, "\n")
  print(result$summary)
  cat("\n")
}


#### C03-EXTRA-Webscrapping: Descargando modulos del INEI ======================

# Archivos para bajar de la web directamente;

# Definiendo una funcion para descargar los archivos ZIP
download_zip_files <- function(projects, modules, base_url, dest_dir) {
  # Loop a traves de cada modulo y projecto
  for (p in projects) {
    for (m in modules) {
      # Construccion del URL
      file_url <- paste0(base_url, "/", p, "-Modulo", m, ".zip")
      
      # Construccion de la direccion de descarga
      dest_file <- paste0(dest_dir, "/", p, "-Modulo", m, ".zip")
      
      # Descarga del Archivo
      tryCatch({
        download.file(file_url, destfile = dest_file, mode = "wb")
        message("Descargado: ", file_url)
      }, error = function(e) {
        message("Falla al descargar el archivo: ", file_url, "\nError: ", e)
      })
    }
  }
}

# Ejemplo
projects <- c("280", "281", "282")  # Proyectos
modules <- c("01", "02", "03", "04", "05", "06")  # Modulos
base_url <- "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/STATA"
dest_dir <- "C:/Users/delga/Dropbox/INEI/datos-enaho/"  # Destino

# Aplicando la funcion
download_zip_files(projects, modules, base_url, dest_dir)

# Descomprimir todos los archivos 
zip_files <- list.files(dest_dir, full.names = TRUE, pattern = "\\.zip$")

unzip_file <- function(z){
  name <- basename(z)
  name <- str_remove_all(name, ".zip")	
  unzip(z, exdir = paste0(dest_dir, name))
}

# Aplicar la funcion para cada archivo zip_files de la lista:
map(zip_files, unzip_file)





