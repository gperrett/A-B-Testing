library(tidyverse)
library(parallel)
library(here)

rdir <- here()

# set number of cores available for parallel processing
cpu.cores <- detectCores()

# set scintific notation options 
options(scipen = 100, digits = 4)

# set alpha of a/b checks to .05
alpha <- 0.05


# sample size illustration ------------------------------------------------

sample.size.falacy <- function(mu, effect, simulations = 10000) {
  # function conducts ttest of two normal distributions 
  #    with mu and effect size as their respective means
  # returns dataframe with pvalue for each sample size
  
  # set sequence of sample sizes to run
  sample.size <- seq(0, 50000, by = 5000)
  sample.size[1] <- 100
  sample.size <- rep(sample.size, simulations)
  
  # create two random normals, conduct t-test, and record p value
  p.mat <- mclapply(sample.size, mc.cores = cpu.cores, function(N){
    A <- rnorm(N, mu, 15)
    B <- rnorm(N, effect , 15)
    p.val <- t.test(A, B, var.equal = TRUE)$p.value
    return(p.val)
  }) %>% 
    unlist() %>% 
    cbind(sample.size) %>% 
    as_tibble() %>% 
    rename(pval = '.')
  
  return(p.mat)
}

# run the simulations at different effect sizes
no.difference <- sample.size.falacy(120, 120) %>% mutate(`Effect Size` = "No difference")
one.tenth <- sample.size.falacy(120, 120.1) %>% mutate(`Effect Size` = "0.1 secounds")
quarter.secound <- sample.size.falacy(120, 120.25) %>% mutate(`Effect Size` = "0.25 secounds")
half.secound <- sample.size.falacy(120, 120.5) %>% mutate(`Effect Size` = "0.5 secounds")
full.secound <- sample.size.falacy(120, 121) %>% mutate(`Effect Size` = "1 secounds")

# combine results into one dataframe and calculate proportion of values less than alpha
effect.size.plot <- rbind(one.tenth, 
                          quarter.secound, 
                          half.secound, 
                          full.secound) %>% 
  group_by(sample.size, `Effect Size`) %>% 
  count(sig = pval < alpha) %>% 
  filter(sig == T) %>% 
  arrange(sample.size) %>% 
  mutate(prop = n/10000) %>% 
  mutate(sample = paste0("N = ", sample.size))

# plot percent of sims showing difference vs. sample size
ggplot(effect.size.plot, aes(sample.size , prop, col = `Effect Size`)) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal() + 
  labs(title = "How often were A and B found to be different?", 
       y = 'Percent of simulations showing difference', 
       x = "Sample Size")
# ggsave("effect.size.plot.pdf", path = file.path(rdir,"/figures"))


# effect size density illustration-------------------------------------------

# effect size translated to mu
mus <- 120 * (1 + c(0, 0.001, 0.01, 0.05, 0.1))

# matrix of normal distributions at different effect sizes
norm.mat <- sapply(mus, function(x) rnorm(n = 100000, mean = x, sd = 15)) %>% as_tibble()
names(norm.mat) <- c("Original", paste0(scales::percent(c(0.001, 0.01, 0.05, 0.1)), " shift"))

# density plot of various effect size 
norm.mat %>% 
  pivot_longer(2:length(mus)) %>% 
  mutate(name = factor(name, levels = paste0(scales::percent(c(0.001, 0.01, 0.05, 0.1)), " shift"))) %>% 
  ggplot() +
  geom_density(aes(x = Original)) +
  geom_density(aes(x = value, color = name)) +
  facet_wrap(~name) +
  labs(title = "Changes in mean still results in significant overlap of distributions",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")
# ggsave("effect_comp.pdf", path = file.path(rdir,"/figures"))
