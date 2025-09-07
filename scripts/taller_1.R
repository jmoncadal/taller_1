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

# Ejercicio 1.1 Web Scrap -------------------------------------------------------------
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
# Ejercicio 1.2 Exploración ; limpieza y imputación ---------------------------------------------
geih <- read_xlsx(paste0(wd_output, "/base_geih.xlsx"))
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
  filter(y_total_m_ha != 0)

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

# Histogram of ln_ingtot_h (not taking into account NAs)
x <- df$y_total_m_ha
hist(x[!is.na(x)],
     breaks = 40,
     main = "Histogram: y_total_m_ha",
     xlab  = "ln_ingtot_h")

x <- x[is.finite(x)]

#Buscamos si hay outliers y cuántos
pcts <- data.frame(
  percentile = 1:100,
  value = as.numeric(quantile(x, probs = (1:100)/100, na.rm = TRUE, names = FALSE, type = 7))
)
print(pcts, row.names = FALSE)
#Hay una variable muy alta en el percentil 1%
p99 <- quantile(df$y_total_m_ha[is.finite(df$y_total_m_ha)], 0.99, na.rm = TRUE, type = 7)

# ) La elminamos  > p99 (keep NAs as-is)
df <- df %>% filter(is.na(y_total_m_ha) | y_total_m_ha <= p99)


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
df$sizeFirm      <- as.factor(df$sizeFirm)
df$informal      <- as.factor(df$informal)
#rename some variables:

df <- df %>% rename(
  type_occup = relab,
  activity_time = p6240,
  second_job = p7040,
  activity_second_job = p7050,
  experience = p6426
)


# We check the amount of na's based on 
colSums(is.na(df[c("estrato1", "oficio","maxEducLevel", "type_occup", "activity_time", "second_job", "activity_second_job")]))

# second_job has a lot of NA's the same number as in oficio
table(df$second_job) # Valores muy bajos de sí, con muchos missings. No es una buena idea. bad controls?


# Ejercicio 1.3 Estadistícas descriptivas base limpia ------------------------------------------


# Ejercicio 3 -------------------------------------------------------------

# Creando las variables del modelo (2)
df <- df %>% mutate(
  ln_ingtot_h = log(y_total_m_ha)
)

df <- df %>% mutate(
  age_sq = age^2)


# Corriendo el modelo (2)
model2_3 <- lm(ln_ingtot_h ~ age+age_sq, data = df)
stargazer(model2_3, type = "text")

# Hallando los intervalos de confianza utilizando bootstrap.

ci_model2_3 <- function(data, index){

  model2_3 <- lm(ln_ingtot_h ~ age+age_sq, data = df, subset = index)
  coefs <- model2_3$coefficients
  
  b1 <- coefs[1]
  # b2 <- coefs[2]
  
}

boot(df, ci_model2_3, R = 1000) # Creo que está sacando el beta 0

# Propuesta bootstrap
ci_model2_3 <- function(data, index){
  model2_3 <- lm(ln_ingtot_h ~ age + age_sq, data = data, subset = index)
  return(coef(model2_3)[c("age", "age_sq")])
}

results <- boot(df, ci_model2_3, R = 1000)

boot.ci(results, type = "perc", index = 1)  # For age
boot.ci(results, type = "perc", index = 2)  # For age_sq



# Ejercicio 4 -------------------------------------------------------------

# Estimando los modelos
#Cambia
df$sex <- ifelse(df$sex == 1, 0, 1)
df <- rename(df, 'bin_female'='sex')
#Ejercicio 4.a
model3_4 <- lm(ln_ingtot_h ~ bin_female, data = df)
out_tex <- file.path(wd_views, "model3_4.tex")
stargazer(model3_4, type = 'text')
stargazer(model3_4, type = 'latex', out=out_tex)

#Ejercicio 4.b 
df <- df %>% mutate(
  experience_sq = experience^2
)
# Define all variables used in both models
vars_needed <- c("ln_ingtot_h", "bin_female", "age", "age_sq", "estrato1", 
                 "sizeFirm", "maxEducLevel", "experience")


# Filter out rows with any missing values in those variables
df_clean <- df %>% filter(if_all(all_of(vars_needed), ~ !is.na(.)))

# Now run the FWL steps
controles <- ~ age + age_sq + estrato1 + maxEducLevel + informal + experience + experience_sq + sizeFirm

y_tilde <- resid(lm(update(controles, ln_ingtot_h ~ .), data = df_clean))
d_tilde <- resid(lm(update(controles, bin_female ~ .), data = df_clean))

model4_fwl <- lm(y_tilde ~ 0 + d_tilde)
stargazer(model4_fwl, type = 'text')

fwl_boot <- function(data, indices) {
  df_sample <- data[indices, ]
  
  controles <- ~ age + age_sq + estrato1 + maxEducLevel + informal + experience + experience_sq + sizeFirm
  
  y_tilde <- resid(lm(update(controles, ln_ingtot_h ~ .), data = df_sample))
  d_tilde <- resid(lm(update(controles, bin_female ~ .), data = df_sample))
  
  coef(lm(y_tilde ~ 0 + d_tilde))[1]
}

library(boot)

set.seed(123) 
boot_results <- boot(data = df_clean, statistic = fwl_boot, R = 500)

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
library(dplyr)
library(ggplot2)
library(boot)


vars_needed <- c("ln_ingtot_h", "bin_female", "age", "age_sq", "estrato1", 
                 "oficio", "cuentaPropia", "maxEducLevel", "experience")

model_age <- lm(
  ln_ingtot_h ~ age + age_sq + 
    bin_female + bin_female:age + bin_female:age_sq +
    estrato1 + oficio + cuentaPropia + maxEducLevel + experience,
  data = df_clean
)

#converitr en factores
df_clean <- df_clean %>%
  mutate(
    estrato1     = as.factor(estrato1),
    oficio       = as.factor(oficio),
    cuentaPropia = as.factor(cuentaPropia),
    maxEducLevel = as.factor(maxEducLevel),
    sizeFirm = as.factor(sizeFirm)
  )

df_pred <- expand.grid(
  age          = seq(16, 85, by = 1),
  bin_female     = c(0, 1),
  estrato1     = levels(df$estrato1)[1],
  oficio       = levels(df$oficio)[1],
  cuentaPropia = levels(df$cuentaPropia)[1],
  maxEducLevel = levels(df$maxEducLevel)[1],
  experience   = mean(df$experience, na.rm = TRUE)
) %>%
  mutate(age_sq = age^2)  # agregamos el término cuadrático

# Función bootstrap:
#       - data: df_clean
#       - indices: remuestreo con reemplazo
#       - devuelve: predicciones para cada fila de df_pred
boot_curve_fun <- function(data, indices) {
  d <- data[indices, ]
  
  # Reajustar modelo con la muestra bootstrap
  fit <- lm(
    ln_ingtot_h ~ age + age_sq +
      bin_female + bin_female:age + bin_female:age_sq +
      estrato1 + oficio + maxEducLevel + cuentaPropia + experience,
    data = d
  )
  
  # Predicciones en la misma grilla df_pred
  predict(fit, newdata = df_pred)
}

# Ejecutar bootstrap
set.seed(111)
boot_res <- boot::boot(data = df_clean, statistic = boot_curve_fun, R = 1000)

# Calcular intervalos percentiles por cada fila de df_pred
# boot_res$t es una matriz de R x nrow(df_pred)
boot_mat <- boot_res$t

ci_mat <- t(apply(boot_mat, 2, quantile, probs = c(0.05, 0.95), na.rm = TRUE))

# (4.4) Añadimos estas bandas bootstrap a df_pred
df_pred <- df_pred %>%
  mutate(
    fit_boot = boot_res$t0,    # predicción original
    lwr_boot = ci_mat[,1],     # límite inferior 5%
    upr_boot = ci_mat[,2]      # límite superior 95%
  )

# =========================
# Grafica
# =========================

# (1) Extraer coeficientes del modelo original
coefs <- coef(model_age)

b_age  <- coefs["age"]
b_age2 <- coefs["age_sq"]

# Manejo robusto de las interacciones (pueden llamarse "bin_female:age" o "age:bin_female")
nm_int1 <- grep("^(bin_female:age|age:bin_female)$", names(coefs), value = TRUE)
nm_int2 <- grep("^(bin_female:age_sq|age_sq:bin_female)$", names(coefs), value = TRUE)

b_int1 <- if (length(nm_int1) == 1) coefs[nm_int1] else 0
b_int2 <- if (length(nm_int2) == 1) coefs[nm_int2] else 0

# (2) Calcular picos
peak_female <- -b_age / (2 * b_age2)
peak_male   <- -(b_age + b_int1) / (2 * (b_age2 + b_int2))


# (3) Gráfico con bandas bootstrap y líneas verticales
ggplot(df_pred, aes(x = age, y = fit_boot, color = factor(bin_female), fill = factor(bin_female))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr_boot, ymax = upr_boot, group = bin_female),
              alpha = 0.2, color = NA) +
  geom_vline(xintercept = peak_female, linetype = "dashed", color = "red") +
  geom_vline(xintercept = peak_male,   linetype = "dashed", color = "blue") +
  annotate("text", x = peak_female, y = max(df_pred$fit_boot, na.rm = TRUE),
           label = round(peak_female, 1), color = "red",
           size = 4, fontface = "bold", vjust = -0.5) +
  annotate("text", x = peak_male, y = max(df_pred$fit_boot, na.rm = TRUE),
           label = round(peak_male, 1), color = "blue",
           size = 4, fontface = "bold", vjust = -0.5) +
  labs(
    title = "Predicted log-salary by Age and Gender (Bootstrap 95% CI)",
    x = "Age",
    y = "Predicted log(salary)",
    color = "bin_female",
    fill  = "bin_female"
  ) +
  theme_minimal()

# Intento gráfica marly -------------------------------------------------------------

vars_needed <- c("ln_ingtot_h", "bin_female", "age", "age_sq", "sizeFirm", "estrato1", "maxEducLevel", "experience")

model_age <- lm(
  ln_ingtot_h ~ age + age_sq + 
    bin_female + bin_female:age + bin_female:age_sq +
    estrato1  + informal + maxEducLevel + poly(experience,degree = 2, raw = TRUE) + sizeFirm,
  data = df_clean
)


df_pred <- expand.grid(
  age          = seq(15, 85, by = 1),
  bin_female     = c(0, 1),
  estrato1     = levels(df$estrato1)[1],
  sizeFirm       = levels(df$sizeFirm)[1],
  informal = levels(df$informal)[1],
  maxEducLevel = levels(df$maxEducLevel)[1],
  experience   = mean(df$experience, na.rm = TRUE)
) %>%
  mutate(age_sq = age^2)  # agregamos el término cuadrático

# Función bootstrap:
#       - data: df_clean
#       - indices: remuestreo con reemplazo
#       - devuelve: predicciones para cada fila de df_pred
boot_curve_fun <- function(data, indices) {
  d <- data[indices, ]
  
  # Reajustar modelo con la muestra bootstrap
  fit <- lm(
    ln_ingtot_h ~ age + age_sq + 
      bin_female + bin_female:age + bin_female:age_sq +
      informal + estrato1 + maxEducLevel + poly(experience,degree = 2, raw = TRUE) + sizeFirm,
    data = d
  )
  
  # Predicciones en la misma grilla df_pred
  predict(fit, newdata = df_pred)
}

# Ejecutar bootstrap
set.seed(111)
boot_res <- boot(data = df_clean, statistic = boot_curve_fun, R = 1000)

# Calcular intervalos percentiles por cada fila de df_pred
# boot_res$t es una matriz de R x nrow(df_pred)
boot_mat <- boot_res$t

# 5) Matriz R x n_pred y percentiles por punto (fila de df_pred)
boot_mat <- boot_res$t
stopifnot(ncol(boot_mat) == nrow(df_pred))

probs <- c(0.05, 0.50, 0.95)  # 5%, mediana, 95%
ci_mat <- t(apply(boot_mat, 2, quantile, probs = probs, na.rm = TRUE))
colnames(ci_mat) <- c("p5", "p50", "p95")

ci_df <- bind_cols(df_pred, as.data.frame(ci_mat))

# 6) Gráfico: dos bandas (una por género) con su mediana
ci_df <- ci_df %>%
  mutate(genero = if_else(bin_female == 1, "Mujer", "Hombre"))

ggplot(ci_df, aes(x = age, y = p50, group = genero)) +
  geom_ribbon(aes(ymin = p5, ymax = p95, fill = genero), alpha = 0.2) +
  geom_line(aes(linetype = genero)) +
  labs(
    title = "Curvas de salario (log) por género con IC bootstrap (5%–95%)",
    x = "Edad",
    y = "ln(salario horario)"
  ) +
  theme_minimal()

# Punto 5 ----------------

set.seed(10101)

df_w <- df%>% filter(!is.na(df$ln_ingtot_h))

df_w <- df%>% filter(!is.infinite(df$ln_ingtot_h))

db_int = df_w%>% filter(ln_ingtot_h > 0)

# vamos a partir las bases de datos

inTrain <- createDataPartition(
  
  y = db_int$ln_ingtot_h,  ## the outcome data are needed
  
  p = .70, ## The percentage of training data
  
  list = FALSE
  
)

training <- db_int |> filter(row_number() %in% inTrain)

testing  <- db_int |> filter(!(row_number() %in% inTrain))

# modelo 1

model1_formula <- ln_ingtot_h ~ age+age_sq

model1 <- lm(model1_formula,
             data = training)

predictions <- predict(object = model1, newdata = testing)

score1a<- RMSE(pred = predictions, obs = testing$ln_ingtot_h )

score1a

# modelo 2 (hasta acá está bien)

model2_formula <- ln_ingtot_h ~ bin_male

model2 <- lm(model2_formula,
             
             data = training)

predictions <- predict(object = model2, newdata = testing)

score2a<- RMSE(pred = predictions, obs = testing$ln_ingtot_h )

score2a

# model 3

model3_formula <- ln_ingtot_h ~ age + age_sq + bin_male + bin_male:age + bin_male:age_sq + estrato1  + cuentaPropia + maxEducLevel + poly(experience,degree = 2, raw = TRUE) + sizeFirm

model3 <- lm(model3_formula,
             
             data = training)

training$maxEducLevel <- droplevels(factor(training$maxEducLevel))

testing$maxEducLevel  <- factor(testing$maxEducLevel, levels = levels(training$maxEducLevel))

bad <- is.na(testing$maxEducLevel)    # filas con niveles no vistos

predictions <- predict(model3, newdata = testing[!bad,])

score3a<- RMSE(pred = predictions, obs = testing$ln_ingtot_h )

score3a

# modelo 4 (alucinación 1/5)

model4_formula <- ln_ingtot_h ~ age + age_sq + bin_male + bin_male:age + bin_male:age_sq + estrato1  + cuentaPropia + maxEducLevel + poly(experience,degree = 2, raw = TRUE) + sizeFirm + oficio

model4 <- lm(model4_formula,
             
             data = training)

predictions <- predict(model4, newdata = testing[!bad, ])

score4a<- RMSE(pred = predictions, obs = testing$ln_ingtot_h )

score4a

# modelo 5 (alucinación 2/5)

model5_formula <- ln_ingtot_h ~ 
  bin_male +
  age + age_sq +
  poly(experience, 2, raw = TRUE) +
  hoursWorkUsual+
  maxEducLevel +
  formal +
  sizeFirm +
  estrato1 + oficio +
  bin_male:maxEducLevel + bin_male:formal

model5 <- lm(model5_formula,
             data = training)

predictions <- predict(model5, newdata = testing[!bad,])
score5a<- RMSE(pred = predictions, obs = testing$ln_ingtot_h )

score5a

# modelo 6 (alucinación 3/5)

model6_formula <- ln_ingtot_h ~
  bin_male +
  poly(age, 2, raw = TRUE) + poly(age, 2, raw = TRUE):bin_male +
  poly(experience, 4, raw = TRUE) + poly(experience, 3, raw = TRUE):bin_male + poly(experience, 3, raw = TRUE):formal +
  poly(hoursWorkUsual, 4, raw = TRUE) + poly(hoursWorkUsual, 7, raw = TRUE):bin_male +
  maxEducLevel + maxEducLevel:formal +
  formal + bin_male:formal +
  sizeFirm + bin_male:sizeFirm +
  estrato1 + oficio + 
  bin_male:maxEducLevel +  estrato1:maxEducLevel

model6 <- lm(model6_formula,
             
             data = training)

predictions <- predict(object = model6, newdata = testing[!bad, ])

score6a<- RMSE(pred = predictions, obs = testing$ln_ingtot_h )

score6a

# modelo 7 (alucinación 4/5)

model7_formula <- ln_ingtot_h ~ 
  bin_male +
  poly(age, 3, raw = TRUE) + poly(age, 3, raw = TRUE):bin_male +
  poly(experience, 3, raw = TRUE) + poly(experience, 3, raw = TRUE):bin_male + poly(experience, 3, raw = TRUE):formal +
  poly(hoursWorkUsual, 3, raw = TRUE) + poly(hoursWorkUsual, 3, raw = TRUE):bin_male + poly(hoursWorkUsual, 3, raw = TRUE):oficio +
  maxEducLevel + maxEducLevel+
  formal + bin_male:formal +
  sizeFirm + bin_male:sizeFirm +
  estrato1 + oficio + formal:oficio+
  bin_male:maxEducLevel +  estrato1:maxEducLevel

model7 <- lm(model7_formula, data = training)

predictions   <- predict(model7, newdata = testing[!bad, ])

score7a  <- RMSE(predictions, testing$ln_ingtot_h)

score7a

# modelo 8 (alucinación 5/5)

model8_formula <- ln_ingtot_h ~
  
  (
    bin_male + formal + sizeFirm +
      maxEducLevel + estrato1 + oficio +
      poly(age, 1, raw = TRUE) +
      poly(experience, 1, raw = TRUE) +
      poly(hoursWorkUsual, 1, raw = TRUE)
  )^2

model8 <- lm(model8_formula, data = training)

predictions   <- predict(model8, newdata = testing[!bad, ])

score8a  <- RMSE(predictions, testing$ln_ingtot_h)

score8a


# modelo 9 (alucinación 6/5)

model9_formula <- ln_ingtot_h ~ 
  (
    bin_male + formal + sizeFirm +
      maxEducLevel + estrato1 + oficio +
      poly(age, 3, raw = TRUE) +
      poly(experience, 3, raw = TRUE) +
      poly(hoursWorkUsual, 3, raw = TRUE)
  )^2

model9 <- lm(model9_formula, data = training)

predictions   <- predict(model9, newdata = testing[!bad, ])

score9a  <- RMSE(predictions, testing$ln_ingtot_h)

score9a

###########################

p_load(modelsummary)

models <- list(
  "Modelo 1" = model1,
  "Modelo 2" = model2,
  "Modelo 3" = model3,
  "Modelo 4" = model4,
  "Modelo 5" = model5,
  "Modelo 6" = model6,
  "Modelo 7" = model7,
  "Modelo 8" = model8,
  "Modelo 9" = model9
)

# --- Parámetro personalizado por modelo ---
param_personal <- c(
  "Modelo 1" = score1a,
  "Modelo 2" = score2a,
  "Modelo 3" = score3a,
  "Modelo 4" = score4a,
  "Modelo 5" = score5a,
  "Modelo 6" = score6a,
  "Modelo 7" = score7a,
  "Modelo 8" = score8a,
  "Modelo 9" = score9a
)

# Funciones auxiliares
rmse_in_sample <- function(model) sqrt(mean(residuals(model)^2, na.rm = TRUE))
k_params       <- function(model) length(coef(model))

# === 1) Calcular métricas ===
metrics_list <- lapply(models, function(mod) {
  sm <- summary(mod)
  c(
    "RMSE (in-sample)" = rmse_in_sample(mod),
    "R²"               = unname(sm$r.squared),
    "R² ajustado"      = unname(sm$adj.r.squared),
    "Observaciones"    = stats::nobs(mod),
    "N° parámetros"    = k_params(mod),
    "RMSE (out of sample)" = NA  # Placeholder, lo agregaremos luego
  )
})

# === 2) Convertir a matriz ===
metrics_mat <- do.call(rbind, metrics_list)  # <-- cambiamos a rbind en vez de cbind
rownames(metrics_mat) <- names(models)

# === 3) Insertar columna de parámetro personalizado ===
metrics_mat[, "RMSE (out of sample)"] <- param_personal[rownames(metrics_mat)]

# === 4) Pasar a data.frame ===
tabla_df <- data.frame(
  Modelo = rownames(metrics_mat),
  metrics_mat,
  row.names = NULL,
  check.names = FALSE
)

# === 5) Exportar tabla en LaTeX ===
latex_out <- datasummary_df(tabla_df, fmt = 3, output = "latex")
latex_out

#################################


# 2)
model_best <- lm(model6_formula, data = training, na.action = na.exclude)

pred <- predict(model_best, newdata = testing, se.fit = TRUE, na.action = na.pass)
y    <- testing$ln_ingtot_h

ok   <- !is.na(pred$fit) & !is.na(y)
yhat <- pred$fit[ok]
e    <- y[ok] - yhat

# sigma^2 (del entrenamiento) y leverage predictivo h0
sigma2 <- sum(residuals(model_best)^2) / df.residual(model_best)
h0     <- (pred$se.fit[ok]^2) / sigma2                      # se.fit es del "mean"; h0 = se.fit^2 / sigma^2
se_pred <- sqrt(sigma2 * (1 + h0))                          # error estándar predictivo
z       <- e / se_pred

# Intervalos de predicción 95% y flag de fuera de PI
lw <- yhat - 1.96 * se_pred
up <- yhat + 1.96 * se_pred
outside_PI <- (y[ok] < lw) | (y[ok] > up)

# 3) Tabla de triage (ordena por |z|)
triage <- testing[ok, , drop = FALSE] |>
  mutate(
    .row      = which(ok)[seq_len(sum(ok))],
    y         = y[ok],
    yhat      = yhat,
    e         = e,
    abs_e     = abs(e),
    z         = z,
    h0        = h0,
    PI_low    = lw,
    PI_up     = up,
    outside_PI = outside_PI
  ) |>
  arrange(desc(abs(z)))

# (Opcional) tasa observada vs esperada
rate_out <- mean(triage$outside_PI, na.rm = TRUE)
message(sprintf("Cobertura 95%%: fuera de PI observados = %.2f%% (esperado ~5%%).", 100 * rate_out))

# ================== 4) GRÁFICAS ==================

theme_set(theme_minimal(base_size = 12))

# A) Histograma + densidad de errores con colas destacadas
p_hist <- ggplot(triage, aes(x = e)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.7) +
  geom_density(linewidth = 0.8) +
  labs(title = "Distribución de errores de predicción (test)",
       x = "e = y - ŷ", y = "Densidad", fill = "Fuera PI 95%") +
  guides(fill = guide_legend(override.aes = list(color = NA)))

# B) QQ-plot de z (estandarizados predictivos)
p_qq <- ggplot(triage, aes(sample = z)) +
  stat_qq(alpha = 0.5) +
  stat_qq_line() +
  labs(title = "QQ-plot de errores estandarizados (z)",
       x = "Cuantiles teóricos", y = "Cuantiles de z")

# C) Residual vs yhat (búsqueda de no linealidades/heterocedasticidad)
p_res_v_yhat <- ggplot(triage, aes(x = yhat, y = e)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Residuales vs ŷ",
       x = "ŷ", y = "e")

# D) |z| vs h0 (¿errores grandes donde ya había alta varianza?)
p_absz_h0 <- ggplot(triage, aes(x = h0, y = abs(z))) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "|z| vs leverage predictivo (h0)",
       x = "h0", y = "|z|")

# --- E) Box/Violin por subgrupos: OFICIO (top 12) ---
if ("oficio" %in% names(triage)) {
  library(forcats)
  
  # Asegura que 'oficio' sea factor y sin niveles vacíos
  triage <- triage |>
    mutate(oficio = fct_drop(as.factor(oficio)))
  
  # Saca top 12 por frecuencia y CONVIERTE A CHARACTER
  top_oficios <- triage |>
    count(oficio, sort = TRUE, name = "n") |>
    slice_head(n = 12) |>
    pull(oficio) |>
    as.character()
  
  # Mantén solo esos niveles, el resto a "Otros"
  triage <- triage |>
    mutate(oficio_top = fct_other(oficio,
                                  keep = top_oficios,
                                  other_level = "Otros"))
  
  # Grafica sólo los top (excluye "Otros" para que no sature)
  p_box_oficio <- triage |>
    filter(oficio_top != "Otros") |>
    ggplot(aes(x = fct_reorder(oficio_top, e, .fun = median),
               y = e)) +
    geom_violin(fill = "grey85", alpha = 0.7, trim = FALSE) +
    geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
    coord_flip() +
    labs(title = "Errores por oficio (top 12)",
         x = "oficio", y = "e")
}

# F) Cobertura de PI por deciles de ŷ (calibración local)
calib <- triage |>
  mutate(decile = ntile(yhat, 10)) |>
  group_by(decile) |>
  summarise(
    n = n(),
    share_out = mean(outside_PI, na.rm = TRUE),
    .groups = "drop"
  )

p_calib <- ggplot(calib, aes(x = decile, y = share_out)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Cobertura local: proporción fuera del PI por decil de ŷ",
       x = "Decil de ŷ (test)", y = "% fuera del PI (observado)")

#################################

#LOOCV del modelo 5

full_model <- lm(model5_formula,
                 
                 data = db_int)

X<- model.matrix(full_model)

y <- model.response(model.frame(full_model))

beta_hat <- full_model$coefficients

## Calculate the inverse of  (X'X), call it G_inv

G_inv<- solve(t(X)%*%X)

## and 1/1-hi

vec<- 1/(1-hatvalues(full_model))

N <- nrow(X)  # Number of observations

LOO <- numeric(N)  # To store the errors

# Loop over each observation

for (i in 1:N) {
  
  # get the new beta
  
  new_beta<- beta_hat  - vec[i] * G_inv %*% as.vector(X[i, ]) * full_model$residuals[i]
  
  ## get the new error
  
  new_error<- (y[i]- (X[i, ] %*% new_beta))^2
  
  LOO[i]<-  new_error
  
}

looCV_error_model5 <- mean(LOO,na.rm = TRUE)

sqrt(looCV_error_model5)

#LOOCV del modelo 6

ctrl <- trainControl(
  method = "LOOCV", verboseIter = TRUE) ## input the method Leave One Out Cross Validation

vars <- all.vars(model6_formula)      # variables de la fórmula

db_cv <- db_int |>
  mutate(across(where(is.character), ~na_if(.x, ""))) |>  # convierte "" a NA
  filter(!is.na(ln_ingtot_h)) |>
  drop_na(all_of(vars)) |>
  mutate(across(where(is.factor), droplevels))            # limpia niveles vacíos

loocv_modelo6 <- train(
  model6_formula,
  data = db_cv,
  method = "lm",
  trControl = ctrl
)
loocv_modelo6
