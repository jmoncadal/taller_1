# Formateando y estableciendo preferencias --------------------------------

rm(list = ls())
set.seed(07092025)
options("scipen"=100, "digits"=4)

# Cargando paquetes -------------------------------------------------------

library(pacman)
p_load(tidyverse, rvest, writexl, readxl,
       gt, gtsummary, caret, boot, stargazer,
       skimr)
# source(paste0(wd_main, wd_code, "/aux_functions.R")) Descomentar esto luego

# Estableciendo rutas -----------------------------------------------------

wd_main <- "C:/Users/Juan/OneDrive - Universidad de los andes/Escritorio/Universidad/Posgrado/1. Primer Semestre/Big Data y Machine Learning/Trabajos/taller_1"
wd_code <- "/scripts"
wd_output <- "/stores"
wd_views <- "/views"

# Definiciones necesarias -------------------------------------------------

geih <- data.frame()
source(paste0(wd_main, wd_code, "/aux_functions_v2.R"))

# Ejercicio 1. Scrapeo de datos -------------------------------------------
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

geih <- read_xlsx(paste0(wd_main, wd_output, "/base_geih.xlsx"))

# Ejercicio 2. Limpieza de datos ------------------------------------------

# Limpiando las variables de interés.

geih_clean <- geih %>% 
  mutate(estrato1 = as.factor(estrato1),
         oficio = as.factor(oficio),
         maxEducLevel = as.factor(maxEducLevel),
         relab = as.factor(relab),
         p6240 = as.factor(p6240),
         p7040 = as.factor(p7040),
         p7050 = as.factor(p7050),
         age_sq = age^2,
         ln_ingtot_h = log(y_total_m_ha+1e-10)) %>% 
  rename(type_occup = relab,
         activity_time = p6240,
         second_job = p7040,
         activity_second_job = p7050,
         experience = p6426,
         bin_male = sex,
         bin_selfemp = cuentaPropia) %>% 
  filter(age > 18,
         dsi == 0,
         age <= 82)

# Revisamos la cantidad de missing values de la base.

geih_miss <- skim(geih_clean) %>%
  select(skim_variable, n_missing) %>% 
  mutate(perc_missing = n_missing/nrow(geih)) %>% 
  arrange(-n_missing)

# Estadísticas descriptivas

# Gráficos

# Concentración del salario por grupo sexo

ggplot(geih_clean, aes(as.factor(bin_male), ln_ingtot_h)) +
  geom_boxplot(alpha = 0.7, width = 0.6, color = "black", outlier.colour = "blue", outlier.alpha = 0.6) +
  labs(x = "Sexo", y = "Ingreso total por hora (log)") +
  scale_x_discrete(labels = c("0" = "Mujer", "1" = "Hombre")) +
  theme_minimal()

ggsave(paste0(wd_main, wd_views, "/salario_sexo.png"))

# Concentración del salario por grupo etario

geih_clean <- geih_clean %>%
  filter(age <= 82) %>% 
  mutate(age_group = cut(age, breaks = seq(15, 80, by = 5), right = FALSE)) %>% 
  drop_na(age, ln_ingtot_h)

ggplot(geih_clean, aes(age_group, ln_ingtot_h)) +
  geom_boxplot(alpha = 0.7, width = 0.6, color = "black",
               outlier.colour = "blue", outlier.alpha = 0.6) +
  labs(x = "Grupo de edad (años)", y = "Ingreso total por hora (log)") +
  theme_minimal()

# Concentración del salario por grupo educativo

ggplot(geih_clean, aes(maxEducLevel, ln_ingtot_h)) +
  geom_boxplot(alpha = 0.7, width = 0.6, color = "black",
               outlier.colour = "blue", outlier.alpha = 0.6) +
  scale_x_discrete(labels = c("1" = "Ninguno", "2" = "Pre-escolar",
                              "3" = "Primaria incomp.", "4" = "Primaria comp.",
                              "5" = "Secundaria incomp.", "6" = "Secundaria comp.",
                              "7"  = "Terciaria")) +
  labs(x = "Máximo nivel educativo", y = "Ingreso total por hora (log)") +
  theme_minimal()

ggsave(paste0(wd_main, wd_views, "/salario_educacion.png"))

# Concentración de cuenta propia

ggplot(geih_clean, aes(as.factor(bin_selfemp), ln_ingtot_h)) +
  geom_boxplot(alpha = 0.7, width = 0.6, color = "black", outlier.colour = "blue", outlier.alpha = 0.6) +
  labs(x = "Sexo", y = "Ingreso total por hora (log)") +
  scale_x_discrete(labels = c("0" = "No Cuenta Propia", "1" = "Cuenta Propia")) +
  theme_minimal()

ggsave(paste0(wd_main, wd_views, "/salario_cuenta_propa.png"))


# Ejercicio 3. Age-wage profile -------------------------------------------

# Tabla de regresión

model_3 <- lm(ln_ingtot_h ~ age + age_sq, data = geih_clean)
i1 <- coef(model_3)
stargazer(model_3, type = "text")

# Desempeño en la muestra

eta_fn <- function(data, index){
  
    d <- data[index, , drop = FALSE]
    m <- lm(ln_ingtot_h ~ age + age_sq, data = d)
    B <- coef(m)

    peak_age <- - B["age"] / (2 * B["age_sq"])
    return(as.numeric(peak_age))
}

# Graficando perfil edad-salario

ic_boot <- boot(data = geih_clean, statistic = eta_fn, R = 1000)
peak_age <- as.numeric(ic_boot$t0)

ci_bca <- boot.ci(ic_boot, type = "bca")
low <- ci_bca$bca[4]
high <- ci_bca$bca[5]

age_grid <- data.frame(age = seq(floor(min(geih_clean$age)),
                                 ceiling(max(geih_clean$age)),
                                 by = 1)) %>% 
  mutate(age_sq = age^2)

pred_df <- cbind(age_grid, as.data.frame(predict(model_3, newdata = age_grid,
                                                 se.fit = TRUE))) %>% 
  mutate(lwr = fit - 1.96*se.fit,
         upr = fit + 1.96*se.fit)

shade_df <- data.frame(xmin = low, xmax = high, ymin = -Inf, ymax = Inf)

ggplot() +
  geom_point(data = geih_clean, aes(age, ln_ingtot_h), alpha = 0.2, size = 0.8) +
  geom_rect(data = na.omit(shade_df),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, alpha = 0.2) +
  geom_ribbon(data = pred_df, aes(age, ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") +
  geom_line  (data = pred_df, aes(age, y = fit), linewidth = 1) +
  labs(x = "Edad", y = "Ingresos por hora (log)") +
  theme_minimal()

# Ejercicio 4 -------------------------------------------------------------

# Estimando los modelos
#Cambia
geih_clean$bin_male <- ifelse(geih_clean$bin_male == 1, 0, 1)
geih_clean <- rename(geih_clean, 'bin_female'='bin_male')

#Ejercicio 4.a
model3_4 <- lm(ln_ingtot_h ~ bin_female, data = geih_clean)
stargazer(model3_4, type = 'text')

#Ejercicio 4.b 
geih_clean <- geih_clean %>% mutate(
  experience_sq = experience^2)

vars_needed <- c("ln_ingtot_h", "bin_female", "age", "age_sq", "estrato1", 
                 "sizeFirm", "maxEducLevel", "experience")

# Filter out rows with any missing values in those variables
geih_clean <- geih_clean %>% filter(if_all(all_of(vars_needed), ~ !is.na(.)))

# Now run the FWL steps
controles <- ~ age + age_sq + estrato1 + maxEducLevel + bin_selfemp + experience + experience_sq + sizeFirm

y_tilde <- resid(lm(update(controles, ln_ingtot_h ~ .), data = geih_clean))
d_tilde <- resid(lm(update(controles, bin_female ~ .), data = geih_clean))

model4_fwl <- lm(y_tilde ~ 0 + d_tilde)
stargazer(model4_fwl, type = 'text')

fwl_boot <- function(data, indices) {
  df_sample <- data[indices, ]
  
  controles <- ~ age + age_sq + estrato1 + maxEducLevel + informal + experience + experience_sq + sizeFirm
  
  y_tilde <- resid(lm(update(controles, ln_ingtot_h ~ .), data = df_sample))
  d_tilde <- resid(lm(update(controles, bin_female ~ .), data = df_sample))
  
  coef(lm(y_tilde ~ 0 + d_tilde))[1]
}
boot_results <- boot(data = geih_clean, statistic = fwl_boot, R = 500)

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
vars_needed <- c("ln_ingtot_h", "bin_female", "age", "age_sq", "estrato1", 
                 "oficio", "cuentaPropia", "maxEducLevel", "experience")

model_age <- lm(
  ln_ingtot_h ~ age + age_sq + 
    bin_female + bin_female:age + bin_female:age_sq +
    estrato1 + oficio + bin_selfemp + maxEducLevel + experience,
  data = geih_clean)

#converitr en factores

df_pred <- expand.grid(
  age          = seq(18, 85, by = 1),
  bin_female   = c(0, 1),
  estrato1     = levels(geih_clean$estrato1)[1],
  oficio       = levels(geih_clean$oficio)[1],
  cuentaPropia = levels(geih_clean$bin_selfemp)[1],
  maxEducLevel = levels(geih_clean$maxEducLevel)[1],
  experience   = mean(geih_clean$experience, na.rm = TRUE)) %>%
  mutate(age_sq = geih_clean$age^2)  # agregamos el término cuadrático

boot_curve_fun <- function(data, indices) {
  d <- data[indices, ]
  
  # Reajustar modelo con la muestra bootstrap
  fit <- lm(
    ln_ingtot_h ~ age + age_sq +
      bin_female + bin_female:age + bin_female:age_sq +
      estrato1 + oficio + maxEducLevel + cuentaPropia + experience, data = d)
  
  # Predicciones en la misma grilla df_pred
  predict(fit, newdata = df_pred)
}

# Ejecutar bootstrap

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

