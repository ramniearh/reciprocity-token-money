library(here)
library(tidyverse)
library(janitor)

# Import ant treat data:

df <- 
  here("BehaviorSpace", "BEHAVE", "baseline run BEHAVE money-reciprocity 3.0 experiment-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  select(-




  # OLD:
## plot state of surviving strategies at t=5000 in base model, for different configurations
final_state <- df %>% 
  filter(
    step == 5000, 
    memory == F,
    unbounded == F,#keep false for first code review - focus on Nowak
    )

df %>% group_by(money, offspring, mutation, conditional_strategy, interactions_per_generation) %>% 
  summarise(mean(cooperation_rate), sd(cooperation_rate))# %>% view()

# Specific charts for Nowak reimplementation with money modification
final_state %>% 
  filter(offspring == F & mutation == F) %>% 
  filter(conditional_strategy == "NONE", interactions_per_generation == 125) %>% 
  ggplot(aes(x = cooperation_rate, fill=money)) +
  geom_density(position = "identity", alpha = 0.4)


summary(final_state %>% lm(formula = cooperation_rate ~ money + offspring + mutation + memory + unbounded+ conditional_strategy + interactions_per_generation))

# Chart overviews for conceptual discussion:
final_state %>% 
  filter(offspring == F & mutation == F) %>% 
  ggplot(aes(x = cooperation_rate, fill=money)) +
  geom_histogram(position = "identity", alpha = 0.4) +
  facet_grid(conditional_strategy ~ interactions_per_generation)

final_state %>% 
  filter(offspring == F & mutation == F) %>% 
  ggplot(aes(x = average_k, cooperation_rate, color=money)) +
  geom_point() +
  facet_grid(conditional_strategy ~ interactions_per_generation)


## plot distribution of winning strategies across run (unfinished)
state_sample <- df %>% 
  filter(
    memory == F,
    unbounded == F,#keep false for first code review - focus on Nowak
    conditional_strategy == "NONE",
    interactions_per_generation == 300
  ) #%>% 
  
state_sample %>% 
  pivot_longer(cols = 11:22, names_to = "strategy_type", values_to = "count") %>%
  ggplot(aes(x=factor(strategy_type, level=c(-5,-4,-3,-2,-1,-0,1,2,3,4,5,6)),
             y=cooperation_rate, color = interactions_per_generation, size = count )) +
  geom_point()
