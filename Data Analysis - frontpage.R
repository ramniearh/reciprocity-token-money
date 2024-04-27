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
  geom_histogram()

## plot state of surviving strategies at t=5000 in base model, for different configurations
final_state <- df %>% 
  filter(
    step == 5000, 
    money == F,
    offspring == T,
    mutation == T,
    memory == T,
    unbounded == T,
    #conditional_strategy == "OR",
    #interactions_per_generation == 500
    )


final_state %>% ggplot(aes(x = average_k, cooperation_rate, color=interactions_per_generation, shape=conditional_strategy)) +
  geom_point() 
final_state %>% ggplot(aes(x = cooperation_rate)) +
  geom_histogram()
summary(final_state %>% lm(formula = cooperation_rate ~ money + offspring + mutation + memory + unbounded+ conditional_strategy + interactions_per_generation))

## plot distribution of winning strategies at end of run
final_state %>% 
  pivot_longer(cols = 11:22, names_to = "strategy_type", values_to = "count") %>%
  ggplot(aes(x=factor(strategy_type, level=c(-5,-4,-3,-2,-1,-0,1,2,3,4,5,6)),
             y=cooperation_rate, color = interactions_per_generation, size = count )) +
  geom_point()
