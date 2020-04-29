source("scripts/helper_functions.R")
library(broom)
library(ggrepel)

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
  labs(title = "A/B test results",
       subtitle = "A = 44.56 minutes\nB = 42.72 minutes\np-value = .2", 
       x = "Minutes spent on site", 
       y = element_blank()) + 
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.box.background = element_rect(color = 'white'), 
        axis.text.y = element_blank())

save_plot(name = 'no_interaction')


# conduct a test to specify engagement types
reg.output <- tidy(summary(lm(value~ name*Medium, study)))
reg.output[2:4,1] <- c("B", "Mobile", "B x Mobile Interaction")

lab.sup <- c("Computer\nA = 43.5\nB = 32.4\np-value < .001", 
          "Mobile\nA = 45.33\nB = 51.16\np-value < .001")

names(lab.sup) <- c("Computer", "Mobile")
# create seperate facets by medium type
ggplot(study, aes(value, fill = name)) + 
  geom_density(alpha = .7) + 
  labs(title = "A/B test results with interaction",
       x = "Minutes spent on site", 
       y = element_blank()) + 
  facet_wrap(~Medium,
             labeller = labeller(Medium = lab.sup)) + 
  theme(legend.title = element_blank(),
        legend.position = c(0.5, 0.85),
        legend.box.background = element_rect(color = 'white'), 
        axis.text.y = element_blank())

save_plot(name = 'interaction')

