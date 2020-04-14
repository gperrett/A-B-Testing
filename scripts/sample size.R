library(tidyverse)
library(here)

rdir <- here()

# set scintific notation options 
options(scipen=100, digits=4)

# set number of simulations to 10,000
simulations <- 10000

# set alpha of a/b checks to .05
alpha <- .05


sample.size.falacy <- function(mu, effect) {

sample.size <- seq(0, 50000, by = 5000) 
sample.size[1] <- 100

  
# create a matrix to store p values 
p.mat <- matrix(NA, nrow = simulations, ncol = length(sample.size))
# add col names to represent checks 1:10
colnames(p.mat) <- paste0(rep("size_", length(sample.size)), sample.size)

for (k in seq_along(sample.size)) {
  # set sample size
  N <- sample.size[k]

  for(i in 1:simulations) {
    A <- rnorm(N, mu, 15)
    B <- rnorm(N, effect , 15)
      p.mat[i,k] <-t.test(A,B, var.equal=TRUE)$p.value 
    }
}
return(p.mat)
}

no.difference <- sample.size.falacy(120, 120)
one.tenth <- sample.size.falacy(120, 120.1)
quarter.secound <- sample.size.falacy(120, 120.25)
half.secound <- sample.size.falacy(120, 120.5)
full.secound <- sample.size.falacy(120, 121)


one.tenth.plot <- as_tibble(one.tenth) %>% 
  pivot_longer(cols = c(1:ncol(one.tenth))) %>% 
  mutate(`Effect Size` = ".1 secounds")

half.secound.plot <- as_tibble(half.secound) %>% 
  pivot_longer(cols = c(1:ncol(one.tenth))) %>% 
  mutate(`Effect Size` = ".5 secounds")

quarter.secound.plot <- as_tibble(quarter.secound ) %>% 
  pivot_longer(cols = c(1:ncol(quarter.secound))) %>% 
  mutate(`Effect Size` = ".25 secounds")

full.secound.plot <- as_tibble(full.secound) %>% 
  pivot_longer(cols = c(1:ncol(quarter.secound))) %>% 
  mutate(`Effect Size` = "1.0 secound")

no.differnece.plot <- as_tibble(no.difference ) %>% 
  pivot_longer(cols = c(1:ncol(quarter.secound))) %>% 
  mutate(`Effect Size` = "No difference")

effect.size.plot <- rbind(one.tenth.plot, 
                          half.secound.plot, 
                          quarter.secound.plot, 
                          full.secound.plot)

plot.1 <- effect.size.plot %>% group_by(name, `Effect Size`) %>% 
  count(sig = value <.05) %>% 
  filter(sig == T)%>% 
  mutate(size = as.integer(substring(name, first = 6))) %>% 
  arrange(size) %>% 
  mutate(prop = n/10000) %>% mutate(sample = paste("N = ", size))


ggplot(plot.1, aes(size, prop, col = `Effect Size`)) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal() + 
  labs(title = "How often were A and B found to be differnet?", 
       y = 'Percent of simulations showing difference', 
       x = "Sample Size")

effect_comp <-
  rbind(
    tibble(
      A = rnorm(100000, 120, 15),
      B = rnorm(100000, 120.1, 15),
      effect = rep(".1% shift", 100000)
    ),
    tibble(
      A = rnorm(100000, 120, 15),
      B = rnorm(100000, 120.25, 15),
      effect = rep(".25 secounds", 100000)
    ),
    tibble(
      A = rnorm(100000, 120, 15),
      B = rnorm(100000, 120.5, 15),
      effect = rep(".5 secounds", 100000)
    ),
    tibble(
      A = rnorm(100000, 120, 15),
      B = rnorm(100000, 121, 15),
      effect = rep("1 secound", 100000)
    ),
    tibble(
      A = rnorm(100000, 120, 15),
      B = rnorm(100000, 132, 15),
      effect = rep("10% shift", 100000)
    )
  ) %>% 
  pivot_longer(cols = c(1:2))


ggplot(effect_comp, aes(value, col = name)) + 
  geom_density() + 
  theme_minimal() + 
  facet_wrap(~effect, scales = "free")
