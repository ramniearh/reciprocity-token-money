library(here)
library(tidyverse)
library(janitor)
here()
df <- 
  here("BehaviorSpace", "3rd-gen - balances and image scores", "MASTER money-reciprocity 3.0 - balances and image scores experiment-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  select(-population,-benefit_to_cost_ratio, -cost) %>%
  rename(
    step = x_step, run_number = x_run_number,
    average_score_balance = sum_score_balance_of_turtles_count_turtles,
    average_fitness = sum_fitness_of_turtles_count_turtles,
    average_k = sum_k_of_turtles_count_turtles
  ) %>% 
  mutate(
    money = as.logical(money),
    interactions_per_generation = as.factor(interactions_per_generation),
    memory = as.logical(memory),
    offspring = as.logical(offspring),
    mutation = as.logical(mutation),
    unbounded = as.logical(unbounded)
  ) %>% 
  rename(
    "-5" = ncooperators,
    "-4" = ncoop4,
    "-3" = ncoop3,
    "-2" = ncoop2,
    "-1" = ncoop1,
    "0" = ndiscriminators,
    "1" = ndef1,
    "2" = ndef2,
    "3" = ndef3,
    "4" = ndef4,
    "5" = ndef5,
    "6" = ndefectors
  )

df %>% ggplot(aes(x = cooperation_rate)) +
  geom_density(alpha=0.3, fill = money)

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
