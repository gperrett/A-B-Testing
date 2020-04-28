source("scripts/helper_functions.R")
library(parallel)

# set number of cores available for parallel processing
cpu.cores <- detectCores()

# set number of simulations to 10,000
simulations <- 10000

# set alpha of a/b checks to 0.05
alpha <- 0.05

# set total number of data points to 1000
N <- 1000

# set the maximum number of checks
checks <- 20

# run the simulation
results <- mclapply(1:simulations, mc.cores = cpu.cores, FUN = function(i){
  
  # set distributions to conduct t-test on
  A <- rnorm(N, 120, 15)
  B <- rnorm(N, 120, 15)
  
  single.sim <- map_dfr(1:checks, function(n.checks) {
    
    # set sample sizes to conduct checks at (i.e. the intervals)
    looks <- seq(from = N / n.checks,
                 to = N,
                 by = N / n.checks) %>%
      ceiling()
    
    # 'look' at the data at each interval and run a t-test
    p.vals <- map_dbl(looks, function(look) {
      t.test(A[1:look],
             B[1:look],
             var.equal = TRUE)$p.value
    })
    
    # check to see if any of the pvalues are below alpha
    false.positive <- any(p.vals < alpha)
    
    return(tibble(sim = i, checks = n.checks, false.positive = false.positive))
  })
  
  return(single.sim)
}) %>% bind_rows()


# plot of optional stopping vs false positives
results %>% 
  group_by(checks) %>% 
  summarize(`Percentage of false positives` = sum(false.positive) / simulations) %>% 
  ggplot(aes(x = checks, y = `Percentage of false positives`)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(title = "Frequent stoppage increases false positives",
       subtitle = paste0(scales::comma(simulations), " simulations"),
       x = "Number of stops")

save_plot(name = 'optional_stops')

