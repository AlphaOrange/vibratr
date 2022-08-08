library(imager)
library(dplyr)

# Transform RGB data.frame to HSL data.frame
rgb2hsl <- function(df_rgb) {
  df_rgb %>%
    mutate(x = 1:nrow(.), y = 1, z = 1) %>%
    pivot_longer(c(R, G, B), names_to = "cc") %>%
    mutate(cc = ifelse(cc == "R", 1, ifelse(cc == "G", 2, 3))) %>%
    select(x, y, z, cc, value) %>%
    as.cimg(dims = c(max(.$x), 1, 1, 3)) %>%
    RGBtoHSL %>%
    as.data.frame %>%
    pivot_wider(id_cols = x, names_from = cc, values_from = value) %>%
    setNames(c("X", "H", "S", "L")) %>%
    select(H, S, L)
}

# Transform HSL data.frame to RGB data.frame
hsl2rgb <- function(df_hsl) {
  df_hsl %>%
    mutate(x = 1:nrow(.), y = 1, z = 1) %>%
    pivot_longer(c(H, S, L), names_to = "cc") %>%
    mutate(cc = ifelse(cc == "H", 1, ifelse(cc == "S", 2, 3))) %>%
    select(x, y, z, cc, value) %>%
    as.cimg(dims = c(max(.$x), 1, 1, 3)) %>%
    HSLtoRGB %>%
    as.data.frame %>%
    pivot_wider(id_cols = x, names_from = cc, values_from = value) %>%
    setNames(c("X", "R", "G", "B")) %>%
    select(R, G, B)
}

# Calculate Perceived Luminance (from RGB)
percLum <- function(df_rgb) {
  sqrt(0.299 * df_rgb$R ^ 2 + 0.587 * df_rgb$G ^ 2 + 0.114 * df_rgb$B ^ 2)
}

# Hue (0 - 360) to Polar Coordinates
hue2polar <- function(Hue) {
  H1 <- sin(Hue / 360 * 2 * pi) / 2
  H2 <- cos(Hue / 360 * 2 * pi) / 2
  data.frame(H1, H2)
}

# Polar Coordinates to Hue (0 - 360)
polar2hue <- function(polar) {
  polar %>%
    mutate(arcH1 = asin(polar$H1),
           arcH2 = acos(polar$H2) / 2 / pi * 360) %>%
    mutate(Hue = ifelse(arcH1 >= 0, arcH2, 360 - arcH2)) %>%
    select(Hue)
}
