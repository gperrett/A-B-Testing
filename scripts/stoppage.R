library(tidyverse)
library(here)

rdir <- here()

# set scintific notation options 
options(scipen=100, digits=4)

# set number of simulations to 10,000
simulations <- 10000

# set alpha of a/b checks to .05
alpha <- .05

# set total number of data points to 1000
N <- 1000

# 
# raw_count_fp_reference<- rep(NA,simulations)
# true_alpha_reference <- rep(NA,simulations)
checks <- 20
raw_count_fp_os <- rep(NA,checks)
true_alpha_os <- rep(NA,checks)


for (k in 1:checks) {
  
# set number of potential stops 
checks <- k


# set sample sizes to conduct checks at (sequence from 100 to 1000 by 100 increments)
looks <- ceiling(seq(0,N,N/checks)) %>% subset(. > 0)

# create a matrix to store p values 
p.mat <- matrix(NA, nrow = simulations, ncol = checks)
# add col names to represent checks 1:10
colnames(p.mat) <- paste0(rep("test_", max(checks)), 1:max(checks))


for(i in 1:simulations) {
  A <- rnorm(N, 120, 15)
  B <- rnorm(N, 120, 15)
  for (j in 1:checks) {
    p.mat[i,j] <-t.test(A[1:looks[j]],B[1:looks[j]], var.equal=TRUE)$p.value 
  }
}

#  variable for stopping points 
OptStop<-numeric(simulations)



for (i in 1:simulations){
  # if any checks are below .05 record which look
  if(any(p.mat[i,] < alpha))
  {OptStop[i] <- min(which(p.mat[i,] < alpha))}  
  # if no checks are below alpha record the highest posible look
  else{OptStop[i] <- checks} 
}


false.p <- numeric(simulations)
for (i in 1:simulations){
  false.p[i] <- p.mat[i,OptStop[i]]
}

raw_count_fp_os[k] <- sum(false.p < alpha) # raw count of false positives
true_alpha_os[k] <- sum(false.p < alpha)/simulations # false positive percentage

}

results <- tibble(Stops = 1:checks, `Percentage of False Positives` = true_alpha_os)

ggplot(results, aes(Stops, `Percentage of False Positives`)) + 
  geom_point() + 
  geom_smooth(method = 'loess',se = F) + 
  scale_x_continuous(breaks = c(1,5,10,15,20)) + 
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal() + 
  labs(title = "Optional Stoping and Flase Postives")
ggsave("optional.stops.pdf", path = file.path(rdir,"/figures"))

