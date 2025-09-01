# Formateando y estableciendo preferencias --------------------------------

rm(list = ls())

# Cargando paquetes -------------------------------------------------------

library(pacman)
p_load(tidyverse, rvest, writexl, readxl,
       gt, gtsummary, caret, boot, stargazer)

# Estableciendo rutas -----------------------------------------------------

wd_main <- "C:/Users/Juan/OneDrive - Universidad de los andes/Escritorio/Universidad/Posgrado/1. Primer Semestre/Big Data y Machine Learning/Trabajos/taller_1"
wd_code <- "/scripts"
wd_output <- "/stores"
wd_graphics <- "/views"

# Configuraciones adicionales ---------------------------------------------

set.seed(07092025)
theme_set(theme_bw())
options("scipen"=100, "digits"=4)

source(paste0(wd_main, wd_code, "/aux_functions.R"))
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

# Cargando datos desde Excel

geih <- read_xlsx(paste0(wd_main, wd_output, "/base_geih.xlsx"))

# Limitando la muestra a los mayores de edad

geih <- geih %>% 
  filter(age>18,
         dsi == 0)

# Explorando los datos

# Distribución de los salarios de la muestra
histogram(geih, "y_salary_m", "salary_no_inputation")

# Distribución de los salarios de acuerdo al sexo

boxplot(geih, "sex", "bp_sex", c("Salario", "Sexo"))
boxplot(geih, "age", "bp_age", c("Salario", "Edad"))

# Imputación

geih <- geih %>% 
  mutate(y_salary_median = ifelse(is.na(y_salary_m), median(y_salary_m, na.rm = TRUE), y_salary_m),
         y_salary_mean = ifelse(is.na(y_salary_m), mean(y_salary_m, na.rm = TRUE), y_salary_m))

histogram(geih, "y_salary_median", "salary_inputation_median")
histogram(geih, "y_salary_mean", "salary_inputation_mean")

boxplot(geih, "sex", "bp_sex", c("Salario", "Sexo"))
boxplot(geih, "age", "bp_age", c("Salario", "Edad"))
boxplot(geih, "relab", "bp_relab", c("Salario", "Ocupación"))
boxplot(geih, "oficio", "bp_oficio", c("Salario", "Oficio"))

# Ejercicio 2 -------------------------------------------------------------

# Creando las variables del modelo (2)
geih <- geih %>% 
  mutate(age_sq = age^2,
         log_salary_m = log(y_salary_m))

# Creando el plot

geih_avg <- geih %>% 
  group_by(age) %>% 
  summarise(avg_salary = mean(log_salary_m, na.rm = TRUE))

ggplot(geih_avg, aes(x = age, y = avg_salary)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue")

ggplot(geih_avg, aes(x = age_sq, y = avg_salary)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue")

# Corriendo el modelo (2)
model2 <- lm(log_salary_m ~ age+age_sq, data = geih)
stargazer(model2, type = "text")

# Hallando los intervalos de confianza utilizando bootstrap.

boot(geih, ci_model2, R = 1000)

# Ejercicio 3 -------------------------------------------------------------

# Estimando modelo

geih <- rename(geih, 'bin_male'='sex')
control_variables <- c("oficio", "bin_male", "age", "cuentaPropia")
model3 <- lm(log_salary_m ~ bin_male, data = geih)
stargazer(model3, type = 'text')

fwl_1 <- lm(log_salary_m ~ bin_male, data = geih)
residuals_fwl_1 <- residuals(fwl_1)
stargazer(fwl_1, type = "text")

fwl_2 <- lm(as.formula(paste0("log_salary_m ~",
                              paste0(control_variables, collapse = " + "))),
            data = geih)
residuals_fwl_2 <- residuals(fwl_2)
stargazer(fwl_2, type = "text")

fwl_3 <- lm(residuals_fwl_2 ~ residuals_fwl_1)
residuals_fwl_3 <- residuals(fwl_3)
stargazer(fwl_3, type = "text")

# Ejercicio 4 -------------------------------------------------------------

# Ejercicio 5 -------------------------------------------------------------





