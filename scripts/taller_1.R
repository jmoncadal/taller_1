# Formateando y estableciendo preferencias --------------------------------

rm(list = ls())
set.seed(07092025)
options("scipen"=100, "digits"=4)

# Cargando paquetes -------------------------------------------------------

library(pacman)
p_load(tidyverse, rvest, writexl, readxl,
       gt, gtsummary, caret, boot, stargazer)

# Estableciendo rutas -----------------------------------------------------

wd_main <- "taller_1"
wd_code <- "scripts"
wd_output <- "stores"
wd_views <- "views"


# Definiciones necesarias -------------------------------------------------

geih <- data.frame()

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
  Sys.sleep(1)
}

geih[1] <- NULL

write_xlsx(geih, paste0(wd_output, "/base_geih.xlsx"))

geih <- read_xlsx(paste0(wd_output, "/base_geih.xlsx"))

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

ggsave(paste0(wd_views, "/salario_educacion.png"))

# Concentración de cuenta propia

ggplot(geih_clean, aes(as.factor(bin_selfemp), ln_ingtot_h)) +
  geom_boxplot(alpha = 0.7, width = 0.6, color = "black", outlier.colour = "blue", outlier.alpha = 0.6) +
  labs(x = "Sexo", y = "Ingreso total por hora (log)") +
  scale_x_discrete(labels = c("0" = "No Cuenta Propia", "1" = "Cuenta Propia")) +
  theme_minimal()

ggsave(paste0(wd_views, "/salario_cuenta_propa.png"))


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
# Cambiamos nombre
df <- geih_clean
# Estimando los modelos
#Cambia
df$bin_male <- ifelse(df$bin_male == 1, 0, 1)
df <- rename(df, 'bin_female'='bin_male')
df <- rename(df,'cuentaPropia' = 'bin_selfemp')
#Ejercicio 4.a
model3_4 <- lm(ln_ingtot_h ~ bin_female, data = df)
stargazer(model3_4, type = 'text')

#Ejercicio 4.b 
df <- df %>% mutate(
  experience_sq = experience^2
)
# Define all variables used in both models
vars_needed <- c("ln_ingtot_h", "bin_female", "age", "age_sq", "estrato1", "cuentaPropia",
                 "sizeFirm", "maxEducLevel", "experience", "experience_sq")


# Filter out rows with any missing values in those variables
df_clean <- df %>% filter(if_all(all_of(vars_needed), ~ !is.na(.)))

# Now run the FWL steps
controles <- ~ age + age_sq + estrato1 + maxEducLevel + experience + experience_sq + cuentaPropia+  sizeFirm

y_tilde <- resid(lm(update(controles, ln_ingtot_h ~ .), data = df_clean))
d_tilde <- resid(lm(update(controles, bin_female ~ .), data = df_clean))

model4_fwl <- lm(y_tilde ~ 0 + d_tilde)
stargazer(model4_fwl, type = 'text')

stargazer(model3_4, model4_fwl, type = 'latex')
out_tex <- file.path(wd_views, "modelos_p4.tex")

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

# Gráfica 4c
library(car)
model <- lm(
  ln_ingtot_h ~ age + age_sq + bin_female +
    bin_female:age + bin_female:age_sq +
    estrato1 + cuentaPropia + maxEducLevel +
    poly(experience, degree = 2, raw = TRUE) + sizeFirm,
  data = df, na.action = na.omit
)

# Peak ages via delta method (names match your coef list) ---
# Male:   -age / (2*age_sq)
# Female: -(age + age:bin_female) / (2*(age_sq + age_sq:bin_female))
dm_male <- deltaMethod(model, "- age / (2*age_sq)")
dm_female <- deltaMethod(
  model,
  "- (age + `age:bin_female`) / (2*(age_sq + `age_sq:bin_female`))"
)

peaks_tbl <- tibble(
  sex  = c("Male","Female"),
  est  = c(dm_male$Estimate, dm_female$Estimate),
  se   = c(dm_male$SE,       dm_female$SE)
) |>
  mutate(lwr = est - 1.96*se, upr = est + 1.96*se)

print(peaks_tbl)

# Ceteris paribus prediction grid: only age varies ---
age_grid <- 18:82
xl <- model$xlevels  # factor levels used by lm

fac_ref <- function(var) factor(xl[[var]][1], levels = xl[[var]])
num_ref <- function(v) if (all(is.na(v))) NA_real_ else median(v, na.rm = TRUE)

newdat_base <- expand.grid(
  age        = age_grid,
  bin_female = c(0, 1)
) |>
  mutate(
    age_sq       = age^2,
    
    # Hold controls fixed at reference/typical values:
    estrato1     = if ("estrato1"     %in% names(xl)) fac_ref("estrato1")     else factor(df$estrato1[1]),
    maxEducLevel = if ("maxEducLevel" %in% names(xl)) fac_ref("maxEducLevel") else factor(df$maxEducLevel[1]),
    sizeFirm     = if ("sizeFirm"     %in% names(xl)) fac_ref("sizeFirm")     else num_ref(df$sizeFirm),
    cuentaPropia = if ("cuentaPropia" %in% names(xl)) fac_ref("cuentaPropia") else 0,   # fix at 0 (baseline)
    experience   = mean(df$experience, na.rm = TRUE)
  ) |>
  as.data.frame()

# Bootstrap ribbons (refit on raw rows used in model) ---
used_rows <- if (!is.null(model$na.action)) {
  setdiff(seq_len(nrow(df)), as.integer(model$na.action))
} else seq_len(nrow(df))
df_used <- df[used_rows, , drop = FALSE]

B <- 300  # increase for smoother bands if you like

boot_preds <- replicate(B, {
  idx <- sample(seq_len(nrow(df_used)), replace = TRUE)
  m_b <- lm(
    ln_ingtot_h ~ age + age_sq + bin_female +
      bin_female:age + bin_female:age_sq +
      estrato1 + cuentaPropia + maxEducLevel +
      poly(experience, degree = 2, raw = TRUE) + sizeFirm,
    data = df_used[idx, , drop = FALSE]
  )
  predict(m_b, newdata = newdat_base)
}, simplify = TRUE)

pred_summary <- newdat_base |>
  mutate(
    pred_mean = rowMeans(boot_preds),
    pred_lwr  = apply(boot_preds, 1, quantile, probs = 0.025),
    pred_upr  = apply(boot_preds, 1, quantile, probs = 0.975),
    sex_lbl   = if_else(bin_female == 1, "Female", "Male")
  )

# Prep peak overlays ---
peak_bands <- peaks_tbl |>
  transmute(sex_lbl = sex, x0 = lwr, x1 = upr)

peak_lines <- peaks_tbl |>
  transmute(sex_lbl = sex, x = est)

# Renombrar etiquetas de sexo a español
pred_summary <- pred_summary |>
  mutate(sexo = if_else(sex_lbl == "Male", "Hombres", "Mujeres"))

peak_bands <- peak_bands |>
  mutate(sexo = if_else(sex_lbl == "Male", "Hombres", "Mujeres"))

peak_lines <- peak_lines |>
  mutate(sexo = if_else(sex_lbl == "Male", "Hombres", "Mujeres"))

# Gráfico en un solo panel
p <- ggplot(pred_summary, aes(x = age, y = pred_mean, color = sexo, fill = sexo)) +
  geom_rect(data = peak_bands,
            aes(xmin = x0, xmax = x1, ymin = -Inf, ymax = Inf, fill = sexo),
            inherit.aes = FALSE, alpha = 0.1) +
  geom_ribbon(aes(ymin = pred_lwr, ymax = pred_upr), alpha = 0.2, colour = NA) +
  geom_line(size = 1) +
  geom_vline(data = peak_lines, aes(xintercept = x, color = sexo),
             linetype = "dashed") +
  labs(
    title = "Ingreso laboral horario (log) por edad y sexo",
    subtitle = "Ceteris paribus: Solo varía la edad.\nLínea discontinua = pico; banda = IC 95%",
    x = "Edad",
    y = "Ingreso horario predicho (log)",
    color = "Sexo", fill = "Sexo"
  ) +
  theme_minimal(base_size = 12)
print(p)

# Guardar con salto de línea en el subtítulo
ggsave(
  filename = paste0(wd_views, "/salario_por_sexo_edad.png"),
  plot = p,
  width = 7, height = 5, dpi = 300
)


# Punto 5 ----------------


set.seed(10101)

df_age <- df |> 
  dplyr::filter(age >= 18, age <= 82)

df_w <- df_age |>
  dplyr::filter(!is.na(ln_ingtot_h), is.finite(ln_ingtot_h))

db_int <- df_w |>
  dplyr::filter(ln_ingtot_h > 0)

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

# modelo 2

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

#Nivelar factores tanto en training como en testing
training$oficio <- droplevels(factor(training$oficio))
testing$oficio  <- factor(testing$oficio, levels = levels(training$oficio))
bad <- is.na(testing$oficio)    # filas con niveles no vistos

predictions <- predict(model3, newdata = testing[!bad,])
score3a<- RMSE(pred = predictions, obs = testing$ln_ingtot_h )
score3a

# modelo 4 (especificación adicional 1/5)

model4_formula <- ln_ingtot_h ~ age + age_sq + bin_male + bin_male:age + bin_male:age_sq + estrato1  + cuentaPropia + maxEducLevel + poly(experience,degree = 2, raw = TRUE) + sizeFirm + oficio

model4 <- lm(model4_formula,
             data = training)

predictions <- predict(model4, newdata = testing[!bad, ])
score4a<- RMSE(pred = predictions, obs = testing$ln_ingtot_h )
score4a

# modelo 5 (especificación adicional 2/5)

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

# modelo 6 (especificación adicional 3/5)

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

# modelo 7 (especificación adicional 4/5)

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

# modelo 8 (especificación adicional 5/5)

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

###########################
#TABLA MODELOS

p_load(modelsummary)

models <- list(
  "Modelo 1" = model1,
  "Modelo 2" = model2,
  "Modelo 3" = model3,
  "Modelo 4" = model4,
  "Modelo 5" = model5,
  "Modelo 6" = model6,
  "Modelo 7" = model7,
  "Modelo 8" = model8
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
  "Modelo 8" = score8a
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

# Generar la salida LaTeX como texto plano
latex_out <- datasummary_df(tabla_df, fmt = 3, output = "latex") |> as.character()
ruta_salida <- "views/tabla_modelos.tex"

# Guardar el archivo .tex
writeLines(latex_out, ruta_salida)

cat("Archivo LaTeX guardado en:", ruta_salida)

#################################
# EJERICIO OUTLIERS

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


# Asegura alineación 1 a 1
obs_ok <- testing$ln_ingtot_h[!bad]

# Error de predicción
err <- predictions - obs_ok

# Data frame y violín
df_err <- tibble(error = err)

ggplot(df_err, aes(x = "", y = error)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.alpha = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = NULL, y = "Error de predicción"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

library(dplyr)
library(ggplot2)
library(caret)

err <- predictions - obs_ok

# Marco para graficar
df_err <- tibble(
  x     = "Errores",        # categoría única para el eje X
  error = err,
  id    = which(!bad)[seq_along(predictions)]  # índice en testing
)

# Umbrales para “colas”
qs <- quantile(df_err$error, c(.025, .50, .975), na.rm = TRUE)
df_err <- df_err %>%
  mutate(
    zona = case_when(
      error <= qs[1] ~ "Cola inferior (2.5%)",
      error >= qs[3] ~ "Cola superior (2.5%)",
      TRUE           ~ "Centro"
    ),
    zona = factor(zona, levels = c("Cola inferior (2.5%)","Centro","Cola superior (2.5%)"))
  )

df_pts <- df_err %>% slice_sample(n = 5000)  

distr <- ggplot(df_err, aes(x = x, y = error)) +
  # violín
  geom_violin(trim = FALSE, width = 0.9, alpha = 0.15) +
  # caja 
  geom_boxplot(width = 0.12, outlier.alpha = 0) +
  # puntos: colorea por zona (colas/centro)
  geom_jitter(
    data  = df_pts,
    aes(color = zona),
    width = 0.08, height = 0, size = 1, alpha = 0.35
  ) +
  # línea en 0 (sesgo)
  geom_hline(yintercept = 0, linetype = "dashed") +
  # mediana como rombo
  stat_summary(fun = median, geom = "point", shape = 23, size = 2.5, fill = "white") +
  # marcas de los percentiles de cola
  geom_segment(aes(x = 0.72, xend = 1.28, y = qs[1], yend = qs[1]), linewidth = 0.3) +
  geom_segment(aes(x = 0.72, xend = 1.28, y = qs[3], yend = qs[3]), linewidth = 0.3) +
  labs(
    subtitle = sprintf("Mediana = %.3f | P2.5 = %.3f | P97.5 = %.3f", qs[2], qs[1], qs[3]),
    x = NULL, y = "Error de predicción (ŷ − y)", color = "Zona"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
distr

ggsave("distr_errors.pdf", plot = distr, width = 10, height = 6, dpi = 300)

#########################
#LOOCV del modelo 5

full_model <- lm(model5_formula,
                 data = db_int)

X<- model.matrix(full_model)
y <- model.response(model.frame(full_model))
beta_hat <- full_model$coefficients

# (X'X)^-1

G_inv<- solve(t(X)%*%X)

# 1/1-hi

vec<- 1/(1-hatvalues(full_model))
N <- nrow(X)  # Number of observations
LOO <- numeric(N)  # To store the errors

# Loop over each observation

for (i in 1:N) {
  
  # new beta
  
  new_beta<- beta_hat  - vec[i] * G_inv %*% as.vector(X[i, ]) * full_model$residuals[i]
  
  # new error
  
  new_error<- (y[i]- (X[i, ] %*% new_beta))^2
  LOO[i]<-  new_error
  
}

looCV_error_model5 <- mean(LOO,na.rm = TRUE)
sqrt(looCV_error_model5)

## LOOCV del modelo 6

ctrl <- trainControl(
  method = "LOOCV", verboseIter = TRUE) ## input the method Leave One Out Cross Validation

vars <- all.vars(model6_formula)      # variables de la fórmula

# Nivela todas las observaciones (en este caso solo es una observación)
db_cv <- db_int |>
  mutate(across(where(is.character), ~na_if(.x, ""))) |>  
  filter(!is.na(ln_ingtot_h)) |>
  drop_na(all_of(vars)) |>
  mutate(across(where(is.factor), droplevels))          

loocv_modelo6 <- train(
  model6_formula,
  data = db_cv,
  method = "lm",
  trControl = ctrl
)
loocv_modelo6

############################
# Tabla

# LOOCV
loocv <- c(
  "Modelo 5" = sqrt(looCV_error_model5), 
  "Modelo 6" = loocv_modelo6$results$RMSE
)

# Scores de validación 30/70
rmse_7030 <- c(
  "Modelo 5" = score5a,   
  "Modelo 6" = score6a    
)

# ---- 2) Construye la tabla a partir de vectores nombrados ----
metrics <- list(
  `LOOCV (RMSE)` = loocv,
  `RMSE (30/70)` = rmse_7030)

build_table <- function(metrics_list) {
  models <- unique(unlist(lapply(metrics_list, names)))
  df <- data.frame(Modelo = models, check.names = FALSE)
  for (nm in names(metrics_list)) {
    v <- metrics_list[[nm]]
    df[[nm]] <- as.numeric(v[df$Modelo])
  }
  rownames(df) <- NULL
  df
}

tab <- build_table(metrics)

# Redondeo
num_cols <- which(sapply(tab, is.numeric))
tab[num_cols] <- lapply(tab[num_cols], function(x) round(x, 3))


# ---- 3) Exporta a LaTeX con kableExtra ----
p_load(knitr)
p_load(kableExtra)

cols_7030 <- grep("30/70", names(tab))
header_vec <- c(" " = 1)
if (length(cols_7030) > 0) header_vec <- c(header_vec, "Validación 30/70" = length(cols_7030))
if (sum(!(seq_along(tab) %in% c(1, cols_7030))) > 0) {
  header_vec <- c(" " = 1, "LOOCV" = 1, "Validación 30/70" = length(cols_7030))
}

k <- kable(
  tab,
  format = "latex",
  booktabs = TRUE,
  align = c("l", rep("c", ncol(tab)-1)),
  caption = "Comparativo de desempeño: LOOCV vs validación 30/70"
) |>
  kable_styling(
    latex_options = c("hold_position", "striped"),
    full_width = FALSE
  ) |>
  column_spec(1, bold = TRUE)

# Aplica encabezado agrupado si corresponde
if (length(header_vec) > 1) {
  k <- k |> add_header_above(header_vec)
}

# Guardar en un .tex listo para \input{} en tu documento
outfile <- "views/tabla_desempeno_modelos.tex"
cat(k, file = outfile)

message("Tabla exportada a: ", outfile)

