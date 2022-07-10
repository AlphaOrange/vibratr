source("R/vibratr.R")


##### Test of Vibrance functions

library(ggformula)

plotVibrance <- function(vib) {
  gf_function_2d(fun = function(x, y) vib(0, x, y),
                 xlim = c(0, 1), ylim = c(0, 1)) %>%
    gf_refine(scale_fill_viridis_c())
}

vibrance_HSL_0 <- function(H, S, L) {
  S * (1 - abs(2 * L - 1))
}

vibrance_HSL_1 <- function(H, S, L) {
  1 - (abs(L - 0.5) + abs(S - 1)) / 1.5
}
plotVibrance(vibrance_HSL_1)

vibrance_HSL_2 <- function(H, S, L) {
  S * (1 - abs(2 * L - 1))
}
plotVibrance(vibrance_HSL_2)

vibrance_HSL_3 <- function(H, S, L) {
  axisL <- (1 - 2 * abs(L - 0.5)) ^ 0.33
  axisS <- S ^ 0.33
  (axisL * axisS) ^ 1.5
}
plotVibrance(vibrance_HSL_3)

vibrance_HSL_3a <- function(H, S, L, bw = TRUE) {
  axisL <- (1 - 2 * abs(L - 0.5)) ^ 0.33
  axisS <- S ^ 0.33
  vibrance <- (axisL * axisS) ^ 1.5
  if (bw) {
    light_vibrance <- 1 - (1 -   L ^ 12) * (1 - S ^ 2)
    dark_vibrance  <- 1 - (1 - ((1 - L) ^ 12)) * (1 - S ^ 2)
    pmax(light_vibrance, dark_vibrance, vibrance)
  } else {
    vibrance
  }
}
plotVibrance(vibrance_HSL_3a)


vibrance_HSL_4 <- function(H, S, L) {
  x <- 3.5 * (0.5 - abs(L - 0.5)) +
       1.0 * S +
       1.0 * (1 - abs(L - 0.5)) * S
  x / 5.5
}
plotVibrance(vibrance_HSL_4)


### Categorise by hand

library(magrittr)
library(dplyr)

z <- 200
data <- data.frame(R = runif(z), G = runif(z), B = runif(z)) %>%
  mutate(col = rgb(R, G, B)) %>%
  mutate(customVib = NA)

for (i in 1:z) {
  barplot(1, col = data$col[i])
  data$customVib[i] <- readline("Vibrance [0-10]: ")
}

data <- data %>% select(R, G, B) %>% rgb2hsl %>% cbind(data, .)

data <- data %>% filter(customVib != "")
pl_data <- data
pl_data <- data %>% filter(H > 300 | H < 60)
pl_data <- data %>% filter(H > 60 & H < 180)
pl_data <- data %>% filter(H > 180 & H < 300)
plot_ly(pl_data, x = ~S, y = ~L, text = ~col,
        type = 'scatter', mode = 'markers',
        marker = list(size = 25, opacity = 1, color = ~customVib,
                      line = list(color = "#FFFFFF"))) %>%
  layout(title = 'Colors',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))
