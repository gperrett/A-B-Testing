library(tidyverse)
set.seed(44)

# set scintific notation options 
options(scipen=100, digits=4)

# set number of simulations to 10,000
simulations <- 1000

# set alpha of a/b checks to .05
alpha <- .05

# set total number of data points to 1000
N <- 1000


# Q1 ----------------------------------------------------------------------

# Generate data from a non-normal distribution then use a t-test and mann-whitney

## simulate a true difference; do a different levels
pvalues <- map_dfr(1:simulations, function(i){
  A <- rgamma(N, 4) # rpois(N, 3) # rnorm(N, 120, 15)
  B <- rgamma(N, 4) # rpois(N, 3) # rnorm(N, 120, 15)
  t.pvals <- t.test(A, B, var.equal = TRUE)$p.value
  whit.pvals <- wilcox.test(A, B)$p.value
  return(tibble(t.pvals, whit.pvals))
})

# proportion of false positives
sum(pvalues$t.pvals <= alpha) / simulations
sum(pvalues$whit.pvals <= alpha) / simulations


# calculate false negatives rate are various sample size levels
n <- c(10, 20, 30, 40, 50, 75, 100, 200, 300, 400, 500, 750, 1000)

false.negative.rates <- map_dfr(n, function(N) {
  
  # calculate {simulations} with sample size {N}
  pvalues <- map_dfr(1:simulations, function(i) {
    A <- rgamma(N, 4.5) # rpois(N, 3) # rnorm(N, 120, 15)
    B <- rgamma(N, 4) # rpois(N, 3) # rnorm(N, 120, 15)
    gam.pvals <- t.test(A, B, var.equal = TRUE)$p.value
    
    A <- rnorm(N, mean = 120*4.5/4, sd = 15)
    B <- rnorm(N, mean = 120, sd = 15)
    norm.pvals <- t.test(A, B, var.equal = TRUE)$p.value
    
    A <- rpois(N, lambda = 3)
    B <- rpois(N, lambda = 4)
    pois.pvals <- t.test(A, B, var.equal = TRUE)$p.value
    return(tibble(gam.pvals, norm.pvals, pois.pvals))
  })
  
  # calculate the proportion of false negatives
  false.neg.gam <- sum(pvalues$gam.pvals > alpha) / simulations
  false.neg.norm <- sum(pvalues$norm.pvals > alpha) / simulations
  false.neg.pois <- sum(pvalues$pois.pvals > alpha) / simulations
  
  return(tibble(false.neg.gam, false.neg.norm, false.neg.pois))
})

# plot of false positive rate vs. sample size
false.negative.rates %>%
  rename("Gamma distribution" = false.neg.gam,
         "Normal distribution" = false.neg.norm,
         "Poisson distribution" = false.neg.pois) %>% 
  mutate(n = n) %>% 
  pivot_longer(cols = 1:3) %>% 
  ggplot(aes(x = n, y = value, group = name, color = name)) +
  geom_line() +
  geom_point() +
  labs(title = 'False negatives more likely at smaller sample sizes when assumptions of t-test are not met',
       caption = paste0('Results from ', scales::comma(simulations), ' simulations per sample size'),
       x = 'Sample size',
       y = 'False negative rate') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank())


# Q2 ----------------------------------------------------------------------

# test of binary data

N <- 50

# Generate a binary dataset and fit a t-test and logistic regression
pvalues <- map_dfr(1:simulations, function(i){
  A <- rbinom(N, size = 1, prob = 0.2)
  B <- rbinom(N, size = 1, prob = 0.2)
  t.pvals <- t.test(A, B, var.equal = TRUE)$p.value
  
  log.pvals <- tibble(A, B) %>% 
    pivot_longer(cols = everything()) %>% 
    glm(value ~ name, data = ., family = 'binomial') %>% 
    broom::tidy() %>% 
    .[[2,5]]
  
  # whit.pvals <- wilcox.test(A, B)$p.values
  # mc.pvals <- mcnemar.test(A, B)$p.value #https://en.wikipedia.org/wiki/McNemar%27s_test
  # log.pvals <- sum(A > B) / N # ????? logistic regression here
  # log.pvals <- t.test(sum(A)/N, sum(B)/N) # ????? logistic regression here
  return(tibble(t.pvals, log.pvals))
})

sum(pvalues$t.pvals <= 0.05) / simulations
sum(pvalues$log.pvals <= 0.05) / simulations
sum(pvalues$whit.pvals <= 0.05) / simulations
sum(pvalues$mc.pvals <= 0.05) / simulations

