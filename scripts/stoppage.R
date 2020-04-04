library(tidyverse)

# set scintific notation options 
options(scipen=100, digits=4)

# set number of simulations to 10,000
simulations <- 10000

# set alpha of a/b checks to .05
alpha <- .05

# set total number of data points to 1000
N <- 1000

# set number of potential stops 
checks <- 10 


# set sample sizes to conduct checks at (sequence from 100 to 1000 by 100 increments)
looks <- ceiling(seq(0,N,N/checks)) %>% subset(. > 0)

# create a matrix to store p values 
p.mat <- matrix(NA, nrow = simulations, ncol = checks)
# add col names to represent checks 1:10
colnames(p.mat) <- paste0(rep("test_", 10), 1:10)


for(i in 1:simulations) {
  A <- rnorm(N, 120, 15)
  B <- rnorm(N, 120, 15)
  for (j in 1:checks) {
    p.mat[i,j] <-t.test(A[1:looks[j]],B[1:looks[j]], var.equal=TRUE)$p.value 
  }
}

sum(p.mat[,max(checks)] < alpha) # raw count of false positives
sum(p.mat[,max(checks)] < alpha)/simulations # false positive percentage


#  variable for stopping points 
OptStop<-numeric(simulations)

for (i in 1:simulations){
  if(any(p.mat[i,] < alpha))
    {OptStop[i] <- min(which(p.mat[i,] < alpha))} # if any checks are below .05 record which look 
  else{OptStop[i] <- checks} # if no checks are below alpha record the highest posible look
}


false.p <- numeric(simulations)

for (i in 1:simulations){
  false.p[i] <- p.mat[i,OptStop[i]]
}

sum(false.p < alpha) # raw count of false positives
sum(false.p < alpha)/simulations # false positive percentage


