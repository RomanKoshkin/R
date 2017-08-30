library(statsr)
library(dplyr)
library(ggplot2)
set.seed(9102015)
data(ames)
z_star_99 <- qnorm(0.995)
n <- 60 # sample size
Reps = 50

params <- ames %>%
  summarise(mu = mean(area))

ci <- ames %>%
  rep_sample_n(size = n, reps = Reps, replace = TRUE) %>%
  summarise(lower = mean(area) - z_star_99 * (sd(area) / sqrt(n)),
            upper = mean(area) + z_star_99 * (sd(area) / sqrt(n)))

ci <- ci %>%
  mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))

ci_data <- data.frame(ci_id = c(1:Reps, 1:Reps),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))

print(str(nrow(subset(ci, capture_mu=='no'))/Reps))

ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray") # draw vertical line
