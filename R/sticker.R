# Preamble ----------------------------------------------------------------

# Dependencies
if (!require('pacman')) install.packages('pacman')
pacman::p_load(
      tidyverse,
      hexSticker
    )

library(ggplot2)
p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() + theme_transparent()
sticker(subplot = "./man/figures/LCSC_logo.png",
        s_x	= 1,
        s_y	= 0.7,
        s_width = 117/128 * 0.4,
        s_height = 0.4,
        package = "quality_control",
        p_size = 18,
        p_color = "azure4",
        h_fill = "aquamarine",
        h_color = "azure4",
        spotlight = TRUE,
        l_x	= 1,
        l_y = 1.25,
        l_width = 6,
        l_alpha = 0.8,
        filename = "./man/figures/badge.png")
