library(tidyverse)

# set seed
set.seed(44)

# set scientific notation options 
options(scipen = 100, digits = 4)

# set a default theme
theme_set(theme_minimal())

# wrap ggplot() with a new default color palette
ggplot <- function(...){ ggplot2::ggplot(...) + 
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Set1")
}

# set wrapper around saving plots so size and type is consistent
save_plot <- function(name, plot = ggplot2::last_plot(), type = c("png", "svg"), height = 4, width = 6.5){
  # function saves ggplots with standardized sizes
  # if more than one type is provided, then plot is saved once per each type
  
  invisible(
    map(type, function(x) {
      ggplot2::ggsave(
       filename = paste0('figures/', name, '.', x),
       plot = plot,
       device = x,
        height = height,
       width = width
    )
  })
  )
}
