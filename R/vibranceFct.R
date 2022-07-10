##### Test of Vibrance functions

library(ggformula)

plotVibrance <- function(vib) {
  gf_function_2d(fun = function(x, y) vib(0, x, y),
                 xlim = c(0, 1), ylim = c(0, 1)) %>%
    gf_refine(scale_fill_viridis_c())
}

vibrance_HSL_1 <- function(H, S, L) {
  1 - (abs(L - 0.5) + abs(S - 1)) / 1.5
}

vibrance_HSL_2 <- function(H, S, L) {
  S * (1 - abs(2 * L - 1))
}

plotVibrance(vibrance_HSL_1)
plotVibrance(vibrance_HSL_2)
