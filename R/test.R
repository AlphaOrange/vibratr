# TEST FUNCTIONS #

source("R/vibratr.R")

path <- "test1.jpg"

pal <- .vibrance(path, bw = TRUE) %>%
  mutate(bubble = 28 + Size / max(Size) * 100)
plot_ly(pal, x = ~Vibrance, y = ~L, text = ~RGB,
        type = 'scatter', mode = 'markers',
        marker = list(size = ~bubble, opacity = 1, color = ~RGB,
                      line = list(color = "#FFFFFF"))) %>%
  layout(title = 'Dominant colors',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

# R/G/B pure score too dark
# cluster centers are not pure enough
