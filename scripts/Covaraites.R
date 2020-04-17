library(tidyverse)
library(broom)
library(ggrepel)

# set seed
set.seed(44)

# set scintific notation options 
options(scipen=100, digits=4)

# Generate Distributions
A <- tibble(A = rnorm(250, 45, 15))

B <- tibble(B = c(rnorm(100, 30, 20), rnorm(150, 50,18)))

# create variable to represent medium of site engagement
Medium <- c(rep("Computer", 100), rep("Mobile", 150))

# combine into a single df
study <- cbind(A, B, Medium) %>%
  as_tibble() %>%
  pivot_longer(cols = 1:2) %>%
  filter(value >= 0) %>%
  mutate(conditional = str_c(name, Medium, sep =  " "))

# pull out A and B results for naive A/B test
A <- study %>% filter(name == "A") %>% select(value) %>% as_vector()
B <- study %>% filter(name == "B") %>% select(value) %>% as_vector()

# implement test
t.test(A,B)


# plot data 
ggplot(study, aes(value, fill = name)) + 
  geom_density(alpha = .7) + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
  legend.position = "bottom")


# concider the impact of engagement medium 
ggplot(study, aes(value,group = conditional, fill = name)) + 
  geom_density(alpha = .7) + 
  theme_minimal()+ 
  theme(strip.text.x = element_text(size = 12,face = "bold"),
        legend.title = element_blank(),
        legend.position = "bottom")

# create seperate facets by medium type
ggplot(study, aes(value, fill = name)) + 
  geom_density(alpha = .7) + 
  facet_wrap(~Medium, scales = "free") + 
  theme_minimal()+ 
  theme(strip.text.x = element_text(size = 12,face = "bold"),
        legend.title = element_blank(),
        legend.position = "bottom")

# conduct a test to specify engagement types
reg.output <- tidy(summary(lm(value~ name*Medium, study)))
reg.output[2:4,1] <- c("B", "Mobile", "B x Mobile Interaction")
reg.output
