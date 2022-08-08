library(imager)
library(dplyr)
library(tidyr)
library(plotly)

source("helper.R")

# Things TODO:
# Perceived Luminance (blue should no longer rank higher than green)
#   https://stackoverflow.com/questions/596216/formula-to-determine-perceived-brightness-of-rgb-color
# Free clustering (no more ten clusters of black)
# Palette generation
# Better definition of vibrance
# problem in clustering: different hues of near-black are far distant to each other
#   => no clean black or white clusters
# from each cluster take 90%-quantile most vibrant as representative
# generate_palette should return harmonic set of differents hues
#   or find corresponding color in actual image to avoid smearing
# resize image by random pixel picking



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
  df_total <- df_total %>%
    hue2polar %>%
    bind_cols(df_total, .)

  # 20 Cluster
  df_cluster <- df_total %>% select(H1, H2, S, L)
  km <- kmeans(df_cluster, centers = 20)

  # Remove tiny clusters
  min_size <- nrow(df_total) / 1000

  # Bind RGB and Vibrance to centers
  results <- cbind(km$centers, Size = km$size) %>%
    as.data.frame %>%
    filter(Size >= min_size) %>%
    mutate(H = polar2hue(H1 * 2, H2 * 2)) %>%
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
