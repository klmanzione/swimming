## Swimming analysis for NCAA altitude training paper. 

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(multcomp)

df <- readRDS("full-data.rds")

dplyr::group_by(df, event, School) %>% 
  dplyr::summarize(n = n())

df <- df %>% 
  dplyr::mutate(., event_long = paste(event, "yard freestyle", sep = " "),
                event_reorder = factor(event_long, levels = c('50 yard freestyle',
                                                              '200 yard freestyle',
                                                              '1650 yard freestyle')))

p <- ggplot(data = df,
            aes(x = elevation_difference, y = improvement, 
                group = event_reorder, col = event_reorder, pch = event_reorder))
p + geom_point(alpha = .5) +
  geom_smooth(method = "lm", alpha = .5) +
  scale_color_brewer("event", palette = "Dark2", labels = c("50", "200", "1650")) +
  labs(x = "change in altitude (feet)",
       y = "improvement (seconds)") +
  guides(pch = "none") +
  theme_light()

ggsave("fig/regression-fits.png", hei = 6, wid = 8)

## Figs in paper

p <- ggplot(data = df,
            aes(x = improvement, fill = Altitude))
p + geom_density(alpha = .5) +
  facet_wrap(~ factor(event_long, levels = c('50 yard freestyle',
                                             '200 yard freestyle',
                                             '1650 yard freestyle')), 
             nrow = 3, scales = "free") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

p <- ggplot(data = df,
            aes(x = School, y = improvement, fill = Altitude))
p + geom_boxplot() +
  facet_wrap(~ factor(event_long, levels = c('50 yard freestyle',
                                             '200 yard freestyle',
                                             '1650 yard freestyle')), 
             nrow = 3, scales = "free") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

## Regression models

swim_lm <- lm(improvement ~ elevation_difference*event,
              data = df)
summary(swim_lm)

K <- matrix(c(0, 1, 0, 0, 0, 1), nr = 1)
t <- glht(swim_lm, linfct = K)
summary(t)

K <- matrix(c(0, 1, 0, 0, 1, 0), nr = 1)
t <- glht(swim_lm, linfct = K)
summary(t)
