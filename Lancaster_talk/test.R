library(tidyverse)
library(gganimate)
library(MASS)
library(Hmisc)

mu <- c(1000, 2000) 
myr <- 35.35534
mysigma <- matrix(c(100, myr, myr, 50), 2, 2) 

data <- NULL
sample_size <- 25

set.seed(1234)
for (i in 2:10) {
  sample <- data.frame(mvrnorm(sample_size, mu, mysigma))
  sample$sample <- i
  data <- rbind(cbind(sample, rcorr(sample$X1, sample$X2)$r[2]), data)
}

sample <- data.frame(mvrnorm(sample_size, mu, mysigma, empirical = FALSE))
sample$sample <- 1
data <- rbind(cbind(sample, rcorr(sample$X1, sample$X2)$r[2]), data)

colnames(data) <- c("Var_1", "Var_2", "Sample", "Correlation")
data$Correlation <- paste("r = ", round(data$Correlation, 3))

ggplot(data, aes(x = Var_1, y = Var_2)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) + 
  transition_time(Sample) + 
  labs(x = "Variable 1", 
       y = "Variable 2", 
       title = "10 samples where each sample size = 500\nSample number: {as.integer(frame_time)}") + 
  theme(title = element_text(size = 15)) +
  ease_aes('cubic-in-out')
