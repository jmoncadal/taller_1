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
  
  y_tilde <- resid(lm(update(controles, ln_ingtot ~ .), data = df_sample))
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
# 2) FWL setup
#    X1 (vars of interest) = bin_male, age, age_sq, bin_male:age, bin_male:age_sq
#    X2 (controls)         = estrato1 + oficio + hoursWorkUsual + maxEducLevel
#    Note: intercept is NOT identified by FWL; we get it from full OLS.
# =========================
# Design matrices (explicit columns, no intercept)
Z <- model.matrix(~ estrato1 + oficio + hoursWorkUsual + maxEducLevel - 1, data = df_clean)
X <- model.matrix(~ bin_male * (age + age_sq) - 1, data = df_clean)  # gives columns:
# "bin_male" "age" "age_sq" "bin_male:age" "bin_male:age_sq"
y <- df_clean$ln_ingtot_h

# Residualize y and each column of X on Z (FWL steps 1 & 2)
y_tilde <- lm(y ~ Z - 1)$residuals
X_tilde <- as.data.frame(X)
for (nm in names(X_tilde)) {
  X_tilde[[nm]] <- lm(X_tilde[[nm]] ~ Z - 1)$residuals
}

# FWL regression (step 3): coefficients equal the full-OLS β for these columns
m_fwl <- lm(y_tilde ~ . - 1, data = X_tilde)
beta  <- coef(m_fwl)
# names(beta): "bin_male","age","age_sq","bin_male:age","bin_male:age_sq"

# Intercept from full model (needed to predict levels of ln(income))
m_full <- lm(ln_ingtot_h ~ bin_male * (age + age_sq) + estrato1 + oficio + hoursWorkUsual + maxEducLevel,
             data = df_clean)
alpha  <- coef(m_full)[["(Intercept)"]]

# =========================
# 3) Deterministic predictions (ages 6..65) in ln(income)
#    Female curve:   α + β_age*a + β_age2*a^2
#    Male curve:     α + β_male + (β_age+β_ma)*a + (β_age2+β_ma2)*a^2
#    Peaks:
#      female_peak = -β_age / (2*β_age2)
#      male_peak   = -(β_age+β_ma) / (2*(β_age2+β_ma2))
# =========================
ages  <- 15:85
b0    <- alpha
b_m   <- beta[["bin_male"]]
b_a   <- beta[["age"]]
b_a2  <- beta[["age_sq"]]
b_ma  <- beta[["bin_male:age"]]
b_ma2 <- beta[["bin_male:age_sq"]]

# Predicted ln(income)
pred_female <- b0 + b_a*ages + b_a2*(ages^2)
pred_male   <- b0 + b_m + (b_a + b_ma)*ages + (b_a2 + b_ma2)*(ages^2)

# Peak ages (sex-specific thanks to interactions)
peak_female <- - b_a / (2*b_a2)
peak_male   <- - (b_a + b_ma) / (2*(b_a2 + b_ma2))

# =========================
# 4) Bootstrap CIs that respect "sum of betas"
#    - Resample rows
#    - Recompute FWL β's + intercept
#    - Form ln-predictions by sex for 6..65
#    - Compute sex-specific peaks
# =========================
boot_fun <- function(data, idx){
  d <- data[idx, ]
  
  # Safeguard types on each resample
  Z <- model.matrix(~ estrato1 + oficio + hoursWorkUsual + maxEducLevel - 1, data = d)
  X <- model.matrix(~ bin_male * (age + age_sq) - 1, data = d)
  y <- d$ln_ingtot_h
  
  y_t <- lm(y ~ Z - 1)$residuals
  X_t <- as.data.frame(X)
  for (nm in names(X_t)) X_t[[nm]] <- lm(X_t[[nm]] ~ Z - 1)$residuals
  
  b <- coef(lm(y_t ~ . - 1, data = X_t))
  # ensure we have all names (rare collinearity fallback)
  need <- c("bin_male","age","age_sq","bin_male:age","bin_male:age_sq")
  b <- setNames(b[need], need)
  
  # Intercept from full model on the resample
  a <- coef(lm(ln_ingtot_h ~ bin_male * (age + age_sq) +
                 estrato1 + oficio + hoursWorkUsual + maxEducLevel, data = d))[["(Intercept)"]]
  
  # Predictions (ln)
  ages <- 6:65
  pf <- a + b[["age"]]*ages + b[["age_sq"]]*(ages^2)
  pm <- a + b[["bin_male"]] + (b[["age"]]+b[["bin_male:age"]])*ages +
    (b[["age_sq"]]+b[["bin_male:age_sq"]])*(ages^2)
  
  # Peaks (sex-specific)
  pf_peak <- - b[["age"]] / (2*b[["age_sq"]])
  pm_peak <- - (b[["age"]]+b[["bin_male:age"]]) / (2*(b[["age_sq"]]+b[["bin_male:age_sq"]]))
  
  c(pf, pm, pf_peak, pm_peak)
}

set.seed(123)
boot_res <- boot(df_clean, boot_fun, R = 1000)

# Unpack bootstrap draws
nAges <- length(ages)
bm    <- boot_res$t                             # B x (2*nAges + 2)
bf    <- bm[, 1:nAges]                          # female ln preds
bmle  <- bm[, (nAges+1):(2*nAges)]              # male   ln preds
bpf   <- bm[, 2*nAges + 1]                      # female peak
bpm   <- bm[, 2*nAges + 2]                      # male   peak

# Pointwise 95% CIs for ln-preds by age and sex
ci_female <- apply(bf,   2, quantile, c(.025,.975))
ci_male   <- apply(bmle, 2, quantile, c(.025,.975))

# 95% CIs for peaks
ci_peak_f <- quantile(bpf, c(.025,.975))
ci_peak_m <- quantile(bpm, c(.025,.975))

# =========================
# 5) Plot (simple): ln(income) only
#    - Lines for predicted ln(income)
#    - Shaded ribbons = 95% CI
#    - Two dashed vertical lines at peaks (female & male)
# =========================
df_plot <- bind_rows(
  tibble(age = ages, sex = "Female",
         lny = pred_female,
         lny_lo = ci_female[1,], lny_hi = ci_female[2,]),
  tibble(age = ages, sex = "Male",
         lny = pred_male,
         lny_lo = ci_male[1,],   lny_hi = ci_male[2,])
)

p <- ggplot(df_plot, aes(x = age, y = lny, color = sex, fill = sex)) +
  geom_ribbon(aes(ymin = lny_lo, ymax = lny_hi), alpha = 0.20, color = NA) +
  geom_line(size = 1.05) +
  geom_vline(xintercept = peak_female, linetype = "dashed") +
  geom_vline(xintercept = peak_male,   linetype = "dashed") +
  labs(
    title = "Predicted Age–Wage Profile (ln income) with 95% CIs",
    subtitle = sprintf("Female peak: %.1f (95%% CI: %.1f–%.1f) | Male peak: %.1f (95%% CI: %.1f–%.1f)",
                       peak_female, ci_peak_f[1], ci_peak_f[2],
                       peak_male,   ci_peak_m[1], ci_peak_m[2]),
    x = "Age", y = "ln(income)", color = "Sex", fill = "Sex",
    caption = "Notes: FWL for β’s; intercept from full OLS. CIs via bootstrap over the sum of betas."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p)

# peak ages plot 

# Predicciones con intervalos de confianza
coefs_model5 <- coef(model5)
b1 <- coefs_model5["age"]         
b2 <- coefs_model5["age_sq"]       
b4 <- coefs_model5["age:bin_male"] 
b5 <- coefs_model5["age_sq:bin_male"] 

peak_f <- -b1 / (2*b2)                    # Para mujeres
peak_m <- -(b1 + b4) / (2*(b2 + b5))      # Para hombres


pred <- predict(model5, newdata = geih, interval = "confidence")
geih <- geih %>%
  mutate(
    fit = pred[, "fit"],
    lwr = pred[, "lwr"],
    upr = pred[, "upr"],
    sex = ifelse(bin_male == 1, "Male", "Female")
  )

ggplot(geih, aes(x = age, y = fit, color = sex, fill = sex)) +
  geom_line(size = 1) +  # predicted line
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) + # shaded CI
  labs(
    title = "Predicted log-salary by Age and Sex with 95% Confidence Areas",
    x = "Age", y = "Predicted log(salary)"
  ) +
  theme_minimal()


# Ejercicio 5 -------------------------------------------------------------






