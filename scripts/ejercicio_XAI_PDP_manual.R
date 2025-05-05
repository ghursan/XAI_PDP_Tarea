library(randomForest)
library(ggplot2)
library(dplyr)
library(readr)

# EJERCICIO 1

bike_data <- read_csv("data/day.csv")

# Modelo
set.seed(123)
bike_model <- randomForest(cnt ~ instant + temp + hum + windspeed, data = bike_data, ntree = 300)

# Función para generar PDP unidimensional
generar_pdp_1d <- function(df, model, var, fixed_vars, n = 100) {
  var_seq <- seq(min(df[[var]]), max(df[[var]]), length.out = n)
  grid <- data.frame(matrix(nrow = n, ncol = length(fixed_vars)))
  colnames(grid) <- fixed_vars
  
  for (fv in fixed_vars) {
    if (fv == var) {
      grid[[fv]] <- var_seq
    } else {
      grid[[fv]] <- mean(df[[fv]])
    }
  }
  
  grid$cnt_pred <- predict(model, newdata = grid)
  
  ggplot(grid, aes_string(x = var, y = "cnt_pred")) +
    geom_line(color = "blue") +
    labs(title = paste("PDP manual: cnt vs", var),
         x = var, y = "cnt predicho") +
    theme_minimal()
}

# Variables
variables <- c("instant", "temp", "hum", "windspeed")
for (v in variables) {
  p <- generar_pdp_1d(bike_data, bike_model, v, variables)
  ggsave(paste0("output/graficos/pdp_one_", v, ".png"), p)
}

# EJERCICIO 2

set.seed(42)
bike_sample <- bike_data %>%
  select(cnt, temp, hum, instant, windspeed) %>%
  sample_n(200)

rf_model_2d <- randomForest(cnt ~ temp + hum + instant + windspeed, data = bike_sample, ntree = 300)

temp_seq <- seq(min(bike_sample$temp), max(bike_sample$temp), length.out = 50)
hum_seq <- seq(min(bike_sample$hum), max(bike_sample$hum), length.out = 50)
grid <- expand.grid(temp = temp_seq, hum = hum_seq)
grid$instant <- mean(bike_sample$instant)
grid$windspeed <- mean(bike_sample$windspeed)

grid$cnt_pred <- predict(rf_model_2d, newdata = grid)

p2d <- ggplot(grid, aes(x = temp, y = hum, fill = cnt_pred)) +
  geom_tile(width = 0.01, height = 0.01) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "PDP 2D manual: temp vs hum",
       x = "Temperatura", y = "Humedad", fill = "cnt predicho") +
  theme_minimal()

ggsave("output/graficos/pdp_bi_temp_hum.png", p2d)

# EJERCICIO 3 

house_data <- read_csv("data/kc_house_data.csv")

vars_casas <- c("bedrooms", "bathrooms", "sqft_living", "sqft_lot", "floors", "yr_built")

house_sample <- house_data %>%
  select(price, all_of(vars_casas)) %>%
  sample_n(1000)

house_model <- randomForest(price ~ ., data = house_sample, ntree = 300)

for (v in c("bedrooms", "bathrooms", "sqft_living", "floors")) {
  p <- generar_pdp_1d(house_sample, house_model, v, vars_casas)
  ggsave(paste0("output/graficos/pdp_price_", v, ".png"), p)
}

