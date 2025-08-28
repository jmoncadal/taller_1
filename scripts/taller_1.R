rm(list = ls())

# Cargando paquetes -------------------------------------------------------

library(pacman)
p_load(tidyverse, rvest, writexl, readxl)

# Estableciendo rutas -----------------------------------------------------

wd_main <- "C:/Users/Juan/OneDrive - Universidad de los andes/Escritorio/Universidad/Posgrado/1. Primer Semestre/Big Data y Machine Learning/Trabajos/taller_1"
wd_code <- "/scripts"
wd_output <- "/stores"

# Definiciones necesarias -------------------------------------------------

geih <- data.frame()

# Scrapeando los datos de la página ---------------------------------------

url_base <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

# Encontrando los hipervínculos del HTML

links <- read_html(url_base) %>%
  html_nodes("a") %>% 
  html_attr("href")

pages <- links[which(substring(links, 1, 4) == "page")]

# Construyendo la base de datos -------------------------------------------

# La página es dinámica. Al buscar de dónde vienen los datos de cada página,
# se ve que tienen un link en común (url_base_tablas). Lo único que varía
# entre bases es el número. 

# Se puede hacer un loop para iterar sobre cada link y extraer la tabla. 
# Así, se creo el data frame "geih" y se une anexa con cada base scrapeada.
# El resultado es una base de 32177 observaciones y 178 variables.

url_base_tablas <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_'

for (i in seq(1, length(pages))){
  
  url_tabla <- paste0(url_base_tablas, i, ".html")
  base <- read_html(url_tabla) %>%
    html_table()
  
  geih <- rbind(geih, base[[1]])
  print(paste0("Base ", i ," cargada."))
}

write_xlsx(geih, paste0(wd_main, wd_output, "/base_geih.xlsx"))

# Ejercicio 2 -------------------------------------------------------------

# Ejercicio 3 -------------------------------------------------------------

# Ejercicio 4 -------------------------------------------------------------

# Ejercicio 5 -------------------------------------------------------------





