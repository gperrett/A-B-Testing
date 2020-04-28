## A/B Testing: Multiple Comparisons/Multiple Testing issues:
## For each simulation, we will generate K datasets (k = 1, 5, 10, 20) which will represent changes due to
## several different hypothetical tests (changing a website's color, text, etc.)

## This will illustrate that as the number of comparisons/tests increases, the chances of a type 1 error
## increase significantly.

source("scripts/helper_functions.R")

N <- 1000 # Set sample size (i.e. number of data points or people viewing website)
Nsim <- 10000 # Set number of simulations
k <- 20 # Set maximum number of datasets to generate each simulation
pmat <- matrix(NA, nrow = Nsim, ncol = k) # Create matrix to store p-values of each dataset for each simulation
error_rate <- data.frame(matrix(NA, nrow = k, ncol = 3)) # Create dataframe to store error rates
error_rate[,1] <- seq(1:k); colnames(error_rate) = c("k", "error_rate1", "error_rate2")

# Generate 10,000 simulations of 20 datasets, and for each one, test if the mean = 120, or =/= 120.
# We assume we know the true distribution of data to be normal with mean 120 and standard deviation 15.

for(i in 1:Nsim){
  xmat = matrix(rnorm(k*N, 120, 15), nrow = N, ncol = k)
  TS = (colMeans(xmat) - 120)/(15/sqrt(N))
  pmat[i,] = pnorm(TS, lower.tail=F)
  
}

# Calculate total type 1 error rates for each number of k (with and without Bonferroni correction):
for (j in 1:k) {
  error_rate[j,2] <- mean(rowSums(pmat[,1:j, drop = FALSE] < 0.05) >= 1)
  error_rate[j,3] <- mean(rowSums(pmat[,1:j, drop = FALSE] < 0.05/j) >= 1)
}


# plot of false positives vs. adjusted
error_rate %>% 
  rename('Unadjusted rate' = error_rate1,
         'Bonferroni adjusted rate' = error_rate2) %>% 
  pivot_longer(cols = -k) %>% 
  mutate(name = factor(name, levels = c('Unadjusted rate', 'Bonferroni adjusted rate'))) %>% 
  ggplot(aes(x = k, y = value, group = name, color = name)) + 
  geom_point() + 
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.70)) + 
  labs(title = "Unadjusted multiple comparisons increases false positives",
       subtitle = paste0(scales::comma(Nsim), " simulations"),
       x = 'Number of comparisons',
       y = 'False positive rate') +
  theme(legend.position = c(0.775, 0.375),
        legend.box.background = element_rect(color = 'grey90'),
        legend.title = element_blank())

save_plot(name = 'multiple_comparisons')


