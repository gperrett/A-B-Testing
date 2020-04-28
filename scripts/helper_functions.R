library(tidyverse)

# set seed
set.seed(44)

# set scientific notation options 
options(scipen = 100, digits = 4)

# set a default theme
theme_set(theme_minimal())

# wrap ggplot() with a new default color palette
ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette = "Dark2")

# set wrapper around saving plots so size and type is consistent
save_plot <- function(plot = ggplot2::last_plot(), name, type = "svg", height = 4, width = 6.5){
  ggsave(plot = plot,
         paste0('figures/', name, '.', type),
         device = type,
         height = height,
         width = width)
}
