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

geih <- read_xlsx( paste0(wd_output, "/base_geih.xlsx"))
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

Explorando los datos v1
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

df <- select(df, -dominio, -depto, -fex_dpto)

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

model3 <- lm(ln_ingtot_h ~ bin_male, data = geih)
out_tex <- file.path(wd_views, "model3.tex")
stargazer(model3, type = 'text')
stargazer(model3, type = 'latex', out=out_tex)

# Modelo con controles FWL
controles <- ~ age + clase + estrato1 + oficio + hoursWorkUsual + p7090 +maxEducLevel
y_tilde <- resid(lm(update(controles, log_salary_m ~ .), data = geih))
d_tilde <- resid(lm(update(controles, bin_male ~ .), data = geih))

model4_fwl <- lm(y_tilde ~ 0 + d_tilde)
summary(model4_fwl)
coef_fwl <- coef(model4_fwl[1])
se_fwl <- sqrt(vcov(model4_fwl)[1,1])

# Propuesta v2 FWL ------------------

library(fixest)

feols(ln_ingtot ~ bin_male | age + age_sq + clase + estrato1 + oficio + hoursWorkUsual + maxEducLevel, data = df)

# Controles y Bootstrap ------------------


# Modelo con controles FWL y Bootstrap
f_boot_fwl <- function(data, idx){
  d <- data[idx, ]                                                #Remuestreo con reemplazo
  resid_y <- resid(lm(update(controles, log_salary_m ~ .), data = d))
  resid_d <- resid(lm(update(controles, bin_male     ~ .), data = d))
  coef(lm(resid_y ~ 0 + resid_d))[1]
}

set.seed(1213)
coef_boot  <- boot(geih, f_boot_fwl, R = 1000)
se_boot <- sd(coef_boot$t)

# Comparación
model4_ols <- lm(log_salary_m ~ bin_male + age + clase + estrato1 + oficio + hoursWorkUsual + p7090 +maxEducLevel, data = geih)
stargazer(model3, model4_ols, type='text')

# peak ages plot 
geih <- geih %>% mutate(age_sq = age^2)

model5 <- lm(log_salary_m ~ age + age_sq + bin_male + bin_male:age + bin_male:age_sq + clase + estrato1 + oficio + hoursWorkUsual + p7090 +maxEducLevel, data = geih)
summary(model5)

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



# 0) Make quadratic and (re)fit model
geih <- geih %>% mutate(age_sq = age^2)
model <- lm(
  log_salary_m ~ age * bin_male + age_sq * bin_male +
    clase + estrato1 + oficio + hoursWorkUsual + p7090 + maxEducLevel,
  data = geih
)

# 1) What variables does the model need?
need <- setdiff(all.vars(terms(model)), c("log_salary_m"))
need
# [should print: "age" "bin_male" "age_sq" "clase" "estrato1" "oficio" "hoursWorkUsual" "p7090" "maxEducLevel"]

# 2) Typical values for other regressors
mode_of <- function(x) { ux <- unique(x); ux[which.max(tabulate(match(x, ux)))] }

typicals <- geih %>%
  summarise(
    across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
    across(where(is.factor),  ~mode_of(.x))
  )

# keep only the columns that appear in the model (except age/bin_male/age_sq)
other_vars <- setdiff(need, c("age","bin_male","age_sq"))
typicals <- typicals %>% select(any_of(other_vars))

# 3) Build prediction grid and ensure correct types
ages <- 18:65
grid <- tidyr::crossing(age = ages, bin_male = c(0,1)) %>%
  mutate(age_sq = age^2)

newdat <- bind_cols(grid, typicals[rep(1, nrow(grid)), , drop = FALSE])

# Make factor columns in newdat match geih's factor levels
factor_vars <- names(Filter(is.factor, geih))
newdat <- newdat %>%
  mutate(across(all_of(intersect(factor_vars, names(newdat))),
                ~ factor(.x, levels = levels(geih[[cur_column()]]))))

# 4) Sanity check: does newdat have what the model needs?
setdiff(need, names(newdat))   # should be character(0)
str(newdat[, need])

# 5) Predict with CIs and plot
pred <- predict(model, newdata = newdat, interval = "confidence")
plotdat <- cbind(newdat, pred) %>%
  mutate(sex = ifelse(bin_male == 1, "Male", "Female"))

ggplot(plotdat, aes(x = age, y = fit, color = sex, fill = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.18, color = NA) +
  labs(title = "Predicted log-salary by Age and Sex (95% CI)",
       x = "Age", y = "Predicted log(salary)") +
  theme_minimal()


# Ejercicio 5 -------------------------------------------------------------






