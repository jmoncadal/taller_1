# Formateando y estableciendo preferencias --------------------------------

rm(list = ls())
set.seed(07092025)
options("scipen"=100, "digits"=4)

# Cargando paquetes -------------------------------------------------------

library(pacman)
p_load(tidyverse, rvest, writexl, readxl,
       gt, gtsummary, caret, boot, stargazer)
source(paste0(wd_main, wd_code, "/aux_functions.R"))

# Estableciendo rutas -----------------------------------------------------

wd_main <- "C:/Users/Juan/OneDrive - Universidad de los andes/Escritorio/Universidad/Posgrado/1. Primer Semestre/Big Data y Machine Learning/Trabajos/taller_1"
wd_code <- "/scripts"
wd_output <- "/stores"

# Definiciones necesarias -------------------------------------------------

geih <- data.frame()

# Ejercicio 1 -------------------------------------------------------------
# Scrapeando datos de la página

url_base <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

links <- read_html(url_base) %>%
  html_nodes("a") %>% 
  html_attr("href")

pages <- links[which(substring(links, 1, 4) == "page")]

url_base_tablas <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_'

for (i in seq(1, length(pages))){
  
  url_tabla <- paste0(url_base_tablas, i, ".html")
  base <- read_html(url_tabla) %>%
    html_table()
  
  geih <- rbind(geih, base[[1]])
  print(paste0("Base ", i ," cargada."))
}

geih[1] <- NULL

write_xlsx(geih, paste0(wd_main, wd_output, "/base_geih.xlsx"))

# Explorando los datos

hist(geih$y_salary_m, breaks = 150,
     main = "Histogram of salary",
     xlab = "Salary")
abline(v = mean(geih$y_salary_m, na.rm = TRUE), col = 'red', lty = 2, lwd = 2)
abline(v = median(geih$y_salary_m, na.rm = TRUE), col = 'blue', lty = 2, lwd = 2)
text(x = 30000000,
     y = 3000,
     labels = paste0("Mean is: ", round(mean(geih$y_salary_m, na.rm = TRUE))))
text(x = 30000000,
     y = 2800,
     labels = paste0("Median is: ", round(median(geih$y_salary_m, na.rm = TRUE))))

# Imputación

geih <- geih %>% 
  mutate(y_salary_m = ifelse(is.na(y_salary_m), median(y_salary_m, na.rm = TRUE), y_salary_m))

# Histograma después de la imputación.
hist(geih$y_salary_m, breaks = 150,
     main = "Histogram of salary",
     xlab = "Salary")
abline(v = mean(geih$y_salary_m, na.rm = TRUE), col = 'red', lty = 2, lwd = 2)
abline(v = median(geih$y_salary_m, na.rm = TRUE), col = 'blue', lty = 2, lwd = 2)
text(x = 30000000,
     y = 20000,
     labels = paste0("Mean is: ", round(mean(geih$y_salary_m, na.rm = TRUE))))
text(x = 30000000,
     y = 18000,
     labels = paste0("Median is: ", round(median(geih$y_salary_m, na.rm = TRUE))))

# Ejercicio 2 -------------------------------------------------------------

# Creando las variables del modelo (2)
geih <- geih %>% 
  mutate(age_sq = age^2,
         log_salary_m = log(y_salary_m))

# Corriendo el modelo (2)
model2 <- lm(log_salary_m ~ age+age_sq, data = geih)
stargazer(model2, type = "text")

# Hallando los intervalos de confianza utilizando bootstrap.

ci_model2 <- function(data, index){

  model2 <- lm(log_salary_m ~ age+age_sq, data = geih, subset = index)
  coefs <- model2$coefficients
  
  b1 <- coefs[1]
  # b2 <- coefs[2]
  
}

boot(geih, ci_model2, R = 1000)

# Ejercicio 3 -------------------------------------------------------------

# Estimando modelo

geih <- rename(geih, 'bin_male'='sex')

model3 <- lm(log_salary_m ~ bin_male, data = geih)
stargazer(model3, type = 'text')



# Ejercicio 4 -------------------------------------------------------------

# Ejercicio 5 -------------------------------------------------------------





