library(tidyverse)
library(pwr)

# Test Data - Cookie Cats:
testdata <- read.csv("data/cookie_cats.csv")

# Exploratory Data Analysis:
summary(testdata)

# remove outlier:
testdata <- testdata[-which.max(testdata$sum_gamerounds),]

# change true/false to 1,0:
testdata$retention_1 = ifelse(testdata$retention_1 == TRUE, 1, 0)
testdata$retention_7 = ifelse(testdata$retention_7 == TRUE, 1, 0)

# density plot of games played:
ggplot(testdata, aes(x = sum_gamerounds)) + 
  geom_density(aes(group = version, fill = version), alpha = 0.3) +
  xlim(c(0,100)) + 
  theme_minimal() +
  labs(title = "Density Plot of Total Games Played per Individual",
       subtitle = "Group 1 (Gate at 30) and Group 2 (Gate at 40)") +
  xlab("Total Games Played") +
  ylab("Density") # heavily skewed, with most people < 100 games

# ECDF plot of games played to confirm:
ggplot(testdata, aes(x = sum_gamerounds)) + stat_ecdf() +
  facet_wrap(~version, ncol = 2) +
  ggtitle(label = "Cumulative Density plot of Total Games Played", subtitle = "Group 1 v. Group 2") +
  geom_hline(yintercept = 0.90, color = "red", alpha = 0.50) + 
  scale_x_continuous(breaks = scales::pretty_breaks(10), labels = scales::comma) + 
  scale_y_continuous(labels = scales::percent) + 
  xlab("Cumulative Games Played") +
  ylab("Cumulative Density") +
  theme_minimal()

# Summary statistics for both groups:
testdata %>% group_by(version) %>% 
                    summarise("Avg. Games" = mean(sum_gamerounds), 
                              "Avg. Ret1" = mean(retention_1), "Avg. Ret7" = mean(retention_7))


# Power analysis to determine sample size needed to detect true mean difference of 0.20:
powertest <- pwr.t.test(d = 0.20, power = 0.8, type = "two.sample", alternative = "two.sided")
plot(powertest)

# Power analysis to determine power for given data to detect sample size of 0.20:
powertest2 <- pwr.t2n.test(n1 = sum(testdata$version == "gate_30"), n2 = sum(testdata$version == "gate_40"), 
                           d = .20, alternative = "two.sided")
powertest2$power

# T-test of both groups:
gate30 <- testdata %>% filter(version == "gate_30") %>% select(sum_gamerounds)
gate40 <- testdata %>% filter(version == "gate_40") %>% select(sum_gamerounds)

t.test(x = gate30, y = gate40, alternative = "two.sided", var.equal = T)

# Logistic regression:
ret1logit <- glm(retention_1 ~ version, data = testdata, family = binomial())
summary(ret1logit)

ret7logit <- glm(retention_7 ~ version, data = testdata, family = binomial())
summary(ret7logit)


# Logistic regression shows that players are slightly less likely (.95 times the odds) to stay 
# on the game after 7 days of downloading with the gate at 40 than when the gate is at 30.