# Formateando y estableciendo preferencias --------------------------------

rm(list = ls())
set.seed(07092025)
options("scipen"=100, "digits"=4)

# Cargando paquetes -------------------------------------------------------

library(pacman)
p_load(tidyverse, rvest, writexl, readxl,
       gt, gtsummary, caret, boot, stargazer)
# source(paste0(wd_main, wd_code, "/aux_functions.R")) Descomentar esto luego

# Estableciendo rutas -----------------------------------------------------

wd_main <- "taller_1"
wd_code <- "scripts"
wd_output <- "stores"
wd_views <- "views"

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

write_xlsx(geih, paste0(wd_output, "/base_geih.xlsx"))

geih <- read_xlsx(paste0(wd_output, "/base_geih.xlsx"))
# Explorando los datos v1 -------------------------------------------------------------

#hist(geih$y_salary_m, breaks = 150,
#     main = "Histogram of salary",
#     xlab = "Salary")
#abline(v = mean(geih$y_salary_m, na.rm = TRUE), col = 'red', lty = 2, lwd = 2)
#abline(v = median(geih$y_salary_m, na.rm = TRUE), col = 'blue', lty = 2, lwd = 2)
#text(x = 30000000,
#     y = 3000,
#     labels = paste0("Mean is: ", round(mean(geih$y_salary_m, na.rm = TRUE))))
#text(x = 30000000,
#     y = 2800,
#     labels = paste0("Median is: ", round(median(geih$y_salary_m, na.rm = TRUE))))

# Imputación 

# Crreo que podemos hacer ejercicios más complejos de imputación más complejo y 
# con una limpieza de datos más profunda
#geih <- geih %>% 
#  mutate(y_salary_m = ifelse(is.na(y_salary_m), median(y_salary_m, na.rm = TRUE), y_salary_m))

# Histograma después de la imputación.
#hist(geih$y_salary_m, breaks = 150,
#     main = "Histogram of salary",
#     xlab = "Salary")
#abline(v = mean(geih$y_salary_m, na.rm = TRUE), col = 'red', lty = 2, lwd = 2)
#abline(v = median(geih$y_salary_m, na.rm = TRUE), col = 'blue', lty = 2, lwd = 2)
#text(x = 300000000
#     y = 20000,
#     labels = paste0("Mean is: ", round(mean(geih$y_salary_m, na.rm = TRUE))))
#text(x = 30000000,
#     y = 18000,
#     labels = paste0("Median is: ", round(median(geih$y_salary_m, na.rm = TRUE))))

#Explorando los datos v1
# Explorando los datos v2 ------------------------------------------------------
df <- geih
table(df$dominio)

# Definimos nuestra población de interés
# Edad objetivo Mayores de 16 (de acuerdo con el código de infancia y adolescencia)

df <- df %>% filter(age>=15)

# We examine the variable age in depth

quantile(df$age, probs = 0.99, na.rm = TRUE)

df <- filter(df, age<=85)
#Variables of interes:
#y_ingLab_m_ha ; labor income salaried - nomial hourly - all occ.
#y_total_m_ha ; income salaried + independents total - nominal hourly

#We check which of the variables has less missing values;

colSums(is.na(df[c("y_ingLab_m_ha", "y_total_m_ha")]))
#y_total_m_ha

#Data imputation: two approaches: 1
#Basic conditions for a value that is imputated ingtot != 0
#If it has any other value in the hourly nominal variables the maximum vakue 
#that is the value it needs imputated
# If it has a value on the monthly nominal variables, the maximun needs to be divided by the hours worked

impute_y_total_m_ha_level1 <- function(df) {
  hourly_vars  <- c("y_gananciaIndep_m_hu")  
  monthly_vars <- c("y_gananciaIndep_m", "y_ingLab_m", "y_salary_m", "y_total_m")
  
  df %>% 
    rowwise() %>%
    mutate(
      # Rowwise max among hourly vars
      max_hourly = {
        vals <- c_across(all_of(hourly_vars))
        if (all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE)
      },
      # Rowwise max among monthly vars
      max_monthly = {
        vals <- c_across(all_of(monthly_vars))
        if (all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE)
      },
      # Convert monthly max to hourly
      candidate_from_monthly = if (!is.na(max_monthly) && !is.na(hoursWorkUsual) && hoursWorkUsual > 0) {
        max_monthly / (hoursWorkUsual * 4)
      } else {
        NA_real_
      },
      candidate = coalesce(max_hourly, candidate_from_monthly),
      
      # Overwrite y_total_m_ha only when imputation applies
      y_total_m_ha = case_when(
        ingtot != 0 & (is.na(y_total_m_ha) | y_total_m_ha <= 0) & !is.na(max_hourly) ~ max_hourly,
        ingtot != 0 & (is.na(y_total_m_ha) | y_total_m_ha <= 0) & is.na(max_hourly) & !is.na(candidate_from_monthly) ~ candidate_from_monthly,
        TRUE ~ y_total_m_ha
      )
    ) %>%
    ungroup() %>%
    select(-max_hourly, -max_monthly, -candidate_from_monthly, -candidate) # drop helpers
}

df <- impute_y_total_m_ha_level1(df)

#Replace with 0 if ingtot==0, to account for people who had no income at all

df <- df %>%
  mutate(y_total_m_ha = ifelse(ingtot == 0, 0, y_total_m_ha))
# Check for people who spend hours working but had no income
sum(is.na(df$y_total_m_ha))

#There is 1524 individuals that worked that week but have no hourly income

table(is.na(df$y_total_m_ha), is.na(df$hoursWorkUsual))

# If someone worked anytimes of hours, but has no hourly wage, then the mean 
#of the people that have the same sex and oficio are imputated as hourly wage

df <- df %>%
  group_by(sex, oficio) %>%
  mutate(
    # Compute group mean (exclude NA automatically)
    group_mean = if (all(is.na(y_total_m_ha))) NA_real_ else mean(y_total_m_ha, na.rm = TRUE),
    
    # Replace directly into y_total_m_ha
    y_total_m_ha = if_else(
      !is.na(hoursWorkUsual) & is.na(y_total_m_ha) & !is.na(group_mean),
      group_mean,
      y_total_m_ha
    )
  ) %>%
  ungroup() %>%
  select(-group_mean)

# ---- Usage ----

# We drop useless variables:

df <- select(df, -dominio, -depto, -fex_dpto, -clase)

#Check the nature of relevant, non binary variables:
sapply(df[c("estrato1", "oficio","maxEducLevel", "relab", "p6240", "p7040", "p7050")], class)

# Change the values to factors

df$estrato1      <- as.factor(df$estrato1)
df$oficio        <- as.factor(df$oficio)
df$maxEducLevel  <- as.factor(df$maxEducLevel)
df$relab         <- as.factor(df$relab)
df$p6240         <- as.factor(df$p6240)
df$p7040         <- as.factor(df$p7040)
df$p7050         <- as.factor(df$p7050)

#rename some variables:

df <- df %>% rename(
  type_occup = relab,
  activity_time = p6240,
  second_job = p7040,
  activity_second_job = p7050
)


# We check the amount of na's based on 
colSums(is.na(df[c("estrato1", "oficio","maxEducLevel", "type_occup", "activity_time", "second_job", "activity_second_job")]))

# second_job has a lot of NA's the same number as in oficio
table(df$second_job) # Valores muy bajos de sí, con muchos missings. No es una buena idea. bad controls?

# Ejercicio 3 -------------------------------------------------------------

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

boot(geih, ci_model2, R = 1000) # Creo que está sacando el beta 0

# Propuesta bootstrap
ci_model2 <- function(data, index){
  model2 <- lm(log_salary_m ~ age + age_sq, data = data, subset = index)
  return(coef(model2)[c("age", "age_sq")])
}

results <- boot(geih, ci_model2, R = 1000)

boot.ci(results, type = "perc", index = 1)  # For age
boot.ci(results, type = "perc", index = 2)  # For age_sq



# Ejercicio 4 -------------------------------------------------------------

# Estimando modelo
df <- rename(df, 'bin_male'='sex')
df <- df %>% mutate(
  ln_ingtot_h = log(y_total_m_ha+1e-10)
)

df <- df %>% mutate(
  age_sq = age^2)

model3 <- lm(ln_ingtot_h ~ bin_male, data = df)
out_tex <- file.path(wd_views, "model3.tex")
stargazer(model3, type = 'text')
stargazer(model3, type = 'latex', out=out_tex)


# Define all variables used in both models
vars_needed <- c("ln_ingtot_h", "bin_male", "age", "age_sq", "estrato1", 
                 "oficio", "hoursWorkUsual", "maxEducLevel")

# Filter out rows with any missing values in those variables
df_clean <- df %>% filter(if_all(all_of(vars_needed), ~ !is.na(.)))

# Now run the FWL steps
controles <- ~ age + age_sq + estrato1 + oficio + hoursWorkUsual + maxEducLevel

y_tilde <- resid(lm(update(controles, ln_ingtot_h ~ .), data = df_clean))
d_tilde <- resid(lm(update(controles, bin_male ~ .), data = df_clean))

model4_fwl <- lm(y_tilde ~ 0 + d_tilde)
stargazer(model4_fwl, type = 'text')

fwl_boot <- function(data, indices) {
  df_sample <- data[indices, ]
  
  controles <- ~ age + age_sq + estrato1 + oficio + hoursWorkUsual + maxEducLevel
  
  y_tilde <- resid(lm(update(controles, ln_ingtot_h ~ .), data = df_sample))
  d_tilde <- resid(lm(update(controles, bin_male ~ .), data = df_sample))
  
  coef(lm(y_tilde ~ 0 + d_tilde))[1]
}

library(boot)

set.seed(123) 
boot_results <- boot(data = df_clean, statistic = fwl_boot, R = 1000)

# View results
boot_results

se_fwl <- summary(model4_fwl)$coefficients[1, "Std. Error"]

se_boot <- sd(boot_results$t)

comparison <- data.frame(
  Method = c("FWL OLS", "Bootstrap"),
  Std_Error = c(se_fwl, se_boot)
)

print(comparison)

# Intento 4c ----------------------------
# =========================
# 0) Libraries
# =========================
library(dplyr)
library(ggplot2)
library(boot)

# =========================
# 1) Data prep & types
#    - Ensure factors are factors
#    - Keep bin_male as 0/1 numeric
# =========================
vars_needed <- c("ln_ingtot_h","bin_male","age","age_sq",
                 "estrato1","oficio","hoursWorkUsual","maxEducLevel")

df_clean <- df %>%
  filter(if_all(all_of(vars_needed), ~ !is.na(.))) %>%
  mutate(
    estrato1     = as.factor(estrato1),
    oficio       = as.factor(oficio),
    maxEducLevel = as.factor(maxEducLevel)
    # bin_male should be numeric 0/1; if not, map it:
    # bin_male = as.integer(bin_male == "male")
  )

# =========================
# 2) Modelo
# (2.1) Por si acaso, garantizamos que exista age_sq (edad al cuadrado).
#       Esto hace que el perfil edad–salario pueda ser cóncavo (sube y luego baja).
if (!"age_sq" %in% names(df_clean)) {
  df_clean <- df_clean %>% mutate(age_sq = age^2)
}

model_age <- lm(
  ln_ingtot_h ~ age + age_sq +
    bin_male + bin_male:age + bin_male:age_sq +
    estrato1 + oficio + hoursWorkUsual + maxEducLevel,
  data = df_clean
)

# =========================
# 4) Bootstrap para curvas completas
# =========================

library(boot)

# (4.1) Función bootstrap:
#       - data: df_clean
#       - indices: remuestreo con reemplazo
#       - devuelve: predicciones para cada fila de df_pred
boot_curve_fun <- function(data, indices) {
  d <- data[indices, ]
  
  # Reajustar modelo con la muestra bootstrap
  fit <- lm(
    ln_ingtot_h ~ age + age_sq +
      bin_male + bin_male:age + bin_male:age_sq +
      estrato1 + oficio + hoursWorkUsual + maxEducLevel,
    data = d
  )
  
  # Predicciones en la misma grilla df_pred
  predict(fit, newdata = df_pred)
}

# (4.2) Ejecutar bootstrap
set.seed(123)
boot_R <- 500  # usa 1000 o 2000 si quieres más precisión
boot_res <- boot::boot(data = df_clean, statistic = boot_curve_fun, R = boot_R)

# (4.3) Calcular intervalos percentiles por cada fila de df_pred
#       boot_res$t es una matriz de R x nrow(df_pred)
boot_mat <- boot_res$t

ci_mat <- t(apply(boot_mat, 2, quantile, probs = c(0.05, 0.95), na.rm = TRUE))

# (4.4) Añadimos estas bandas bootstrap a df_pred
df_pred <- df_pred %>%
  mutate(
    fit_boot = boot_res$t0,    # predicción original
    lwr_boot = ci_mat[,1],     # límite inferior 2.5%
    upr_boot = ci_mat[,2]      # límite superior 97.5%
  )

# =========================
# Grafica
# =========================

# (1) Extraer coeficientes del modelo original
coefs <- coef(model_age)

b_age  <- coefs["age"]
b_age2 <- coefs["age_sq"]

# Manejo robusto de las interacciones (pueden llamarse "bin_male:age" o "age:bin_male")
nm_int1 <- grep("^(bin_male:age|age:bin_male)$", names(coefs), value = TRUE)
nm_int2 <- grep("^(bin_male:age_sq|age_sq:bin_male)$", names(coefs), value = TRUE)

b_int1 <- if (length(nm_int1) == 1) coefs[nm_int1] else 0
b_int2 <- if (length(nm_int2) == 1) coefs[nm_int2] else 0

# (2) Calcular picos
peak_female <- -b_age / (2 * b_age2)
peak_male   <- -(b_age + b_int1) / (2 * (b_age2 + b_int2))

# (3) Gráfico con bandas bootstrap y líneas verticales
ggplot(df_pred, aes(x = age, y = fit_boot, color = sex, fill = sex)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr_boot, ymax = upr_boot), alpha = 0.2, color = NA) +
  geom_vline(xintercept = peak_female, linetype = "dashed", color = "red") +
  geom_vline(xintercept = peak_male,   linetype = "dashed", color = "blue") +
  labs(
    title = "Predicted log-salary by Age and Gender (Bootstrap 95% CI)",
    x = "Age",
    y = "Predicted log(salary)"
  ) +
  theme_minimal()

# Ejercicio 5 -------------------------------------------------------------






