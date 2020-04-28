source("scripts/helper_functions.R")
library(parallel)

# set number of cores available for parallel processing
cpu.cores <- detectCores()

# set alpha of a/b checks to .05
alpha <- 0.05

# difference in effect size translated to mu
mus <- 120 * (1 + c(0, 0.01, 0.05, 0.1))


# sample size illustration ------------------------------------------------

sample.size.falacy <- function(mu, effect, simulations = 10000) {
  # function conducts ttest of two normal distributions 
  #    with mu and effect size as their respective means
  # returns dataframe with pvalue for each sample size
  
  # set sequence of sample sizes to run
  sample.size <- c(100, 500, 1000, 2500, 5000, 10000)
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
one.difference <- sample.size.falacy(mus[1], mus[2]) %>% 
  mutate(`Effect Size` = paste0(scales::percent(0.01, accuracy = 1), " difference"))
two.difference <- sample.size.falacy(mus[1], mus[3]) %>% 
  mutate(`Effect Size` = paste0(scales::percent(0.05, accuracy = 1), " difference"))
three.difference <- sample.size.falacy(mus[1], mus[4]) %>% 
  mutate(`Effect Size` = paste0(scales::percent(0.1, accuracy = 1), " difference"))

# combine results into one dataframe and calculate proportion of values less than alpha
effect.size.plot <- rbind(one.difference, 
                          two.difference, 
                          three.difference) %>% 
  group_by(sample.size, `Effect Size`) %>% 
  count(sig = pval < alpha) %>% 
  filter(sig == T) %>% 
  arrange(sample.size) %>% 
  mutate(prop = n/10000) %>% 
  mutate(sample = paste0("N = ", sample.size)) %>% 
  ungroup()

# plot percent of sims showing difference vs. sample size
effect.size.plot %>% 
  mutate(`Effect Size` = factor(`Effect Size`,
                                levels = paste0(scales::percent(c(0.01, 0.05, 0.1),
                                                                accuracy = 1),
                                                " difference"))) %>% 
  ggplot(aes(x = sample.size, y = prop, col = `Effect Size`)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Large sample size increases the chance of finding an effect", 
       subtitle = "10,000 simulations",
       y = 'Probability of finding an effect', 
       x = "Sample size") +
  theme(legend.position = c(0.8, 0.35),
        legend.box.background = element_rect(color = 'grey90'))

save_plot(name = 'effect_size')


# effect size density illustration-------------------------------------------

# set sample size
samp.size <- 5000

# matrix of normal distributions at different effect sizes
norm.mat <- sapply(mus, function(x) rnorm(n = samp.size, mean = x, sd = 15)) %>% as_tibble()

# run t.tests
pvals <- apply(norm.mat[,2:4], MARGIN = 2, function(col) t.test(x = norm.mat$V1, y = col, var.equal = TRUE)$p.value)

# set names
col.names <- c("Original", paste0(scales::percent(c(0.01, 0.05, 0.1), accuracy = 1), 
                                     " difference\n p-value = ",
                                  scales::comma(pvals, accuracy = .001)))
names(norm.mat) <- col.names

# density plot of various effect size 
norm.mat %>% 
  pivot_longer(2:length(mus)) %>% 
  mutate(name = factor(name, levels = col.names)) %>% 
  ggplot() +
  geom_density(aes(x = Original), size = 1, color = "grey50", alpha = 0.8) +
  geom_density(aes(x = value, color = name), size = 1, alpha = 0.8) +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~name, nrow = 1) +
  labs(title = "Statistical significance is not always meaningful",
       subtitle = paste0("Sample size of ", scales::comma(samp.size), " each"),
       x = NULL,
       y = NULL) +
  theme(legend.position = "none")

save_plot(name = 'effect_comp')

