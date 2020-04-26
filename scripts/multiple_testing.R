## A/B Testing: Multiple Comparisons/Multiple Testing issues:
## For each simulation, we will generate K datasets (k = 1, 5, 10, 20) which will represent changes due to
## several different hypothetical tests (changing a website's color, text, etc.)

## This will illustrate that as the number of comparisons/tests increases, the chances of a type 1 error
## increase significantly.

library(tidyverse)

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

# Plot Type 1 Error rate (False Positive Rate) vs. Number of Comparisons (k):
ggplot(error_rate, aes(x = k, y = error_rate1)) + 
  geom_point() + 
  geom_line() +
  ggtitle(label = "Increase in False Positive Rate due to Multiple Comparisons") +
  xlab("Number of Comparisons") + ylab("False Positive Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  theme_minimal()

ggplot(error_rate, aes(x = k, y = error_rate2)) + 
  geom_point() + 
  geom_line() +
  ggtitle(label = "False Positive Rate - Multiple Comparisons",
          subtitle = "With Bonferroni Correction") +
  xlab("Number of Comparisons") + ylab("False Positive Rate") +
  scale_y_continuous(limits = c(0,.10), labels = scales::percent) + 
  theme_minimal()




# plot of false positives vs. adjusted
error_rate %>% 
  mutate(actual1 = 1-(.95)^(1:20),
         actual2 = 0.05) %>% 
  pivot_longer(cols = -k) %>% 
  ggplot(aes(x = k, y = value, group = name, color = name, alpha = name)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c('red', 'blue', 'red', 'blue')) +
  scale_alpha_manual(values = c(1, 1, 0.5, 0.5)) +
  geom_curve(aes(x = 11, y = 0.6, 
                 xend = 14.5, yend = 0.54),
             curvature = -0.4, color = 'red', size = 0.1,
             arrow = arrow(type = 'closed', length = unit(0.3, "cm"))) +
  annotate('label', x = 8, y = 0.6, label = "Simulated unadjusted rate", 
           fill = 'white', color = 'red') +
  
  geom_curve(aes(x = 13.5, y = 0.35, 
                 xend = 8.5, yend = 0.32),
             curvature = -0.4, color = 'red', size = 0.1,
             arrow = arrow(type = 'closed', length = unit(0.3, "cm"))) +
  annotate('label', x = 15, y = 0.35, label = "Analytical solution", 
           fill = 'white', color = "red") +
  
  geom_curve(aes(x = 13, y = 0.12, 
                 xend = 15.5, yend = 0.07),
             curvature = -0.4, color = 'blue', size = 0.1,
             arrow = arrow(type = 'closed', length = unit(0.3, "cm"))) +
  annotate('label', x = 10, y = 0.12, label = "Bonferroni adjusted rate",
           fill = 'white', color = 'blue') +
  labs(title = "Unadjusted multiple comparisons increase false positives",
       subtitle = paste0(scales::comma(Nsim), " simulations"),
       x = "Number of comparisons",
       y = "False positive rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.70)) + 
  theme_minimal() +
  theme(legend.position = 'none')

ggsave('figures/mutliple_comparisons.png',
       device = "png",
       height = 4,
       width = 6)


