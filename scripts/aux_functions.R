
# Histograma de datos -----------------------------------------------------

histogram <- function(data, column, filename){

  col <- data[[column]]
  mean_val <- round(mean(col, na.rm = TRUE), 0)
  median_val <- round(median(col, na.rm = TRUE), 0)
  
  x1_text <- round(max(col, na.rm = TRUE)*0.9, 0)
  y1_text <- max(table(cut(col, 30)))
  
  x2_text <- round(max(col, na.rm = TRUE)*0.9, 0)
  y2_text <- max(table(cut(col, 30)))*0.95
  
  p <- ggplot(geih, aes(x = .data[[column]])) +
          geom_histogram(bins = 100, color = "grey20", alpha = 0.7) +
          geom_vline(aes(xintercept = mean_val),
                     color = 'red', linetype = 'dashed', size = 1) +
          geom_vline(aes(xintercept = median_val),
                     color = 'blue', linetype = 'dashed', size = 1) +
          annotate("text", x = x1_text, y = y1_text,
                   label = paste0("Mean: ", mean_val)) +
          annotate("text", x = x2_text, y = y2_text,
                   label = paste0("Median: ", median_val)) +
          labs(x = "Average salary", y = "Frequency")
  
  ggsave(filename = paste0(wd_main, wd_graphics, "/", filename, ".png"),
         plot = p, width = 8, height = 6)
}

# Box-plot datos

boxplot <- function(data, column, filename, labels){
  
  col <- data[[column]]
  x_label <- labels[1]
  y_label <- labels[2]
  
  p <- ggplot(data, aes(x = factor(col), y = y_salary_m, fill = factor(col))) +
          geom_boxplot(alpha = 0.7) +
          labs(x = x_label, y = y_label) +
          theme(legend.position = "none")
        
  ggsave(filename = paste0(wd_main, wd_graphics, "/", filename, ".png"),
         plot = p, width = 8, height = 6)
}

# Estimar el error por boostrap -------------------------------------------

ci_model2 <- function(data, index){
  
  model2 <- lm(log_salary_m ~ age+age_sq, data = geih, subset = index)
  coefs <- model2$coefficients
  
  b1 <- coefs[1]
}