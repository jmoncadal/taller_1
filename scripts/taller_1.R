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

wd_main <- "/Users/marianacorrea/Desktop/PEG/Big data/Taller 1/Sin título/taller_1"
wd_code <- "/scripts"
wd_output <- "/stores"
wd_views <- "/views"

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

geih <- read_xlsx( paste0(wd_main, wd_output, "/base_geih.xlsx"))

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

# Crreo que podemos hacer ejercicios más complejos de imputación más complejo y 
# con una limpieza de datos más profunda
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
geih <- rename(geih, 'bin_male'='sex')

model3 <- lm(log_salary_m ~ bin_male, data = geih)
out_tex <- file.path(wd_main, wd_views, "model3.tex")
stargazer(model3, type = 'latex', out=out_tex)


# Modelo con controles FWL
controles <- ~ age + clase + estrato1 + oficio + hoursWorkUsual + p7090 +maxEducLevel
y_tilde <- resid(lm(update(controles, log_salary_m ~ .), data = geih))
d_tilde <- resid(lm(update(controles, bin_male ~ .), data = geih))

model4_fwl <- lm(y_tilde ~ 0 + d_tilde)
summary(model4_fwl)
coef_fwl <- coef(model4_fwl[1])
se_fwl <- sqrt(vcov(model4_fwl)[1,1])

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

# Modelo interactuando edad y género # metemos edad al cuadrado?
model5 <- lm(log_salary_m ~ age + bin_male + bin_male:age + clase + estrato1 + oficio + hoursWorkUsual + p7090 +maxEducLevel, data = geih)
summary(model5)

# "peak-ages" e intervalos de confianza


geih <- geih %>% mutate(age_sq = age^2)

model <- lm(
  log_salary_m ~ age * bin_male + age_sq * bin_male +
    clase + estrato1 + oficio + hoursWorkUsual + p7090 + maxEducLevel,
  data = geih
)

# Predicciones con intervalos de confianza
pred <- predict(model, newdata = geih, interval = "confidence")
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






