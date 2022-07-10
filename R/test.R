# TEST FUNCTIONS #

source("R/vibratr.R")

path <- "test4.jpg"

# Simple 3-color palette
cols <- generate_palette(path)
barplot(rep(1, 3), col = cols)

# Color decoding
pal <- .vibrance(path, bw = FALSE) %>%
  mutate(bubble = sqrt(Size / max(Size)) * 128)
plot_ly(pal, x = ~Vibrance, y = ~L, text = ~RGB,
        type = 'scatter', mode = 'markers',
        marker = list(size = ~bubble, opacity = 1, color = ~RGB,
                      line = list(color = "#FFFFFF"))) %>%
  layout(title = 'Dominant colors',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

# R/G/B pure score too dark
# cluster centers are not pure enough



library(cluster)

results <- cbind(Cluster = 1:nrow(results), results)
resRGB <- results %>% select(R, G, B)
dist_mat <- dist(resRGB)
hclust_avg <- hclust(dist_mat, method = 'complete')
plot(hclust_avg)
cutno <- sum(hclust_avg$height > 0.25) + 1
cuts <- cutree(hclust_avg, k = cutno)
cbind(results, FinalCluster = cuts)



clusters <- agnes(resRGB)
plot(clusters)

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(resRGB, FUN = hcut, nstart = 25, K.max = 20, B = 50)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)
