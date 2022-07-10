library(imager)
library(dplyr)
library(tidyr)
library(plotly)

# Things TODO:
# Perceived Luminance (blue should no longer rank higher as green)
#   https://stackoverflow.com/questions/596216/formula-to-determine-perceived-brightness-of-rgb-color
# Free clustering (no more ten clusters of black)
# Palette generation
# Better definition of vibrance
# problem in clustering: different hues of near-black are far distant to each other
#   => no clean black or white clusters
# from each cluster take 90%-quantile most vibrant as representative
# generate_palette should return harmonic set of differents hues
#   or find corresponding color in actual image to avoid smearing



# Calculate vibrance from HSL
vibrance_HSL <- function(H, S, L, bw = FALSE) {
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


# Polar coordinates to arcus in 360 degrees
depolar <- function(H1, H2) {
  arcH1 <- asin(H1)
  arcH2 <- acos(H2) / 2 / pi * 360
  ifelse(arcH1 >= 0, arcH2, 360 - arcH2)
}


# Generate palette from image
.vibrance <- function(path, rescale = 50000, bw = FALSE) {

  # load image and rescale
  img_rgb <- load.image(path)
  scale   <- rescale / (dim(img_rgb)[1] * dim(img_rgb)[2])
  img_rgb <- imresize(img_rgb, scale = scale)

  # transform into data.frame
  df_rgb <- as.data.frame(img_rgb) %>%
    pivot_wider(id_cols = c(x, y),
                names_from = cc,
                values_from = value) %>%
    setNames(c("X", "Y", "R", "G", "B"))

  # hsl conversion
  img_hsl <- RGBtoHSL(img_rgb)
  df_hsl <- as.data.frame(img_hsl) %>%
    pivot_wider(id_cols = c(x, y),
                names_from = cc,
                values_from = value) %>%
    setNames(c("X", "Y", "H", "S", "L"))

  df_total <- left_join(df_rgb, df_hsl)

  # polar coordinates for hue
  df_total$H1 <- sin(df_total$H / 360 * 2 * pi) / 2
  df_total$H2 <- cos(df_total$H / 360 * 2 * pi) / 2

  # 20 Cluster
  df_cluster <- df_total %>% select(H1, H2, S, L)
  km <- kmeans(df_cluster, centers = 20)

  # Remove tiny clusters
  min_size <- nrow(df_total) / 1000

  # Bind RGB and Vibrance to centers
  results <- cbind(km$centers, Size = km$size) %>%
    as.data.frame %>%
    filter(Size >= min_size) %>%
    mutate(H = depolar(H1 * 2, H2 * 2)) %>%
    select(H, S, L, Size)
  results <- results %>%
    cbind(hsl2rgb(select(results, H, S, L))) %>%
    mutate(Vibrance = vibrance_HSL(H, S, L, bw)) %>%
    arrange(desc(Vibrance))
  results$RGB <- apply(results, 1, function(row) {
    rgb(row[5], row[6], row[7])
  })

  # Reduce number of clusters
  results_RGB <- results %>% select(R, G, B)
  dist_mat <- dist(results_RGB)
  clusters <- hclust(dist_mat, method = 'complete')
  cut <- max(sum(clusters$height > 0.25) + 1, 6) # min clusters: 6
  clusters <- cutree(clusters, k = cut)
  results <- cbind(results, Cluster = clusters) %>%
    group_by(Cluster) %>%
    arrange(desc(Size)) %>%
    mutate(Size = sum(Size)) %>%
    filter(row_number() == 1) %>%
    ungroup

  # Classify for luminance and vibrance
  results <- results %>%
    mutate(Vib_class = ifelse(Vibrance > 0.6, "vibrant",
                       ifelse(Vibrance > 0.4, "muted",
                                              "grey"))) %>%
    mutate(Lum_class = ifelse(L > 0.5, "light", "dark"))

  results

}

# TODO deconstruct def
generate_palette <- function(path, n = 3) {
  .vibrance(path) %>%
    arrange(desc(Vibrance)) %>%
    pull(RGB) %>%
    `[`(1:n)
}
