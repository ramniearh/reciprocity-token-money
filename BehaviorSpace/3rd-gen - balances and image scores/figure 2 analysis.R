library(here)
library(tidyverse)
library(janitor)
here()
df <- 
  here("BehaviorSpace", "3rd-gen - balances and image scores", "figure 2 money-reciprocity 3.0 - balances and image scores experiment-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  select(-population,-benefit_to_cost_ratio, -cost, -memory, -offspring, -mutation) %>%
  rename(
    step = x_step, run_number = x_run_number,
    average_score_balance = sum_score_balance_of_turtles_count_turtles,
    average_fitness = sum_fitness_of_turtles_count_turtles,
    average_k = sum_k_of_turtles_count_turtles
  ) %>% 
  mutate(
    money = as.logical(money),
    interactions_per_generation = as.factor(interactions_per_generation)
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



## plot state of surviving strategies at t=5000 in base model without mutation
final_state <- df %>% 
  filter(step == 1000, money == T )

#final_state %>% select(-average_fitness) %>% select(7:21) %>% view()

final_state %>% ggplot(aes(x = cooperation_rate)) +
  geom_histogram()

final_state %>% ggplot(aes(x = average_k, cooperation_rate, color=interactions_per_generation)) +
  geom_point() 

## plot distribution of winning strategies at end of run
final_strategies <- final_state %>% 
  pivot_longer(cols = 8:19, names_to = "strategy_type", values_to = "count")
final_strategies %>% ggplot(aes(x = strategy_type, y = average_k, size = cooperation_rate)) +
  geom_point(height = 0)
#final_strategies %>%ggplot(aes(x=strategy_type, y = cooperation_rate)) +
#geom_violin()



## plot evolution of average k over generations
#evol_strat <- df %>% 
  #filter(money == T) %>% 
  #pivot_longer(starts_with("n"), names_to = "strategy_type", values_to = "count") 

df %>% 
  filter(run_number == 50) %>%
  ggplot(aes(x=step, y=average_k, color=money)) + 
  geom_point()







## plot evolutions - OLD

# 3 selected strategies
evol_strat <- df %>% 
  filter(offspring == F & mutation == F & money == T & memory == F) %>% 
  pivot_longer(starts_with("n"), names_to = "strategy_type", values_to = "count") 

evol_strat %>% ggplot(aes(x = step, y = count, color = strategy_type)) +
  geom_point(size = 0.5)

## plot evolution of selected variables, in money or reputation treatments, without learning or mutation
evol_static <- df %>% 
  filter(offspring == F & mutation == F) %>% 
  pivot_longer(money, values_to = "money") 

# cooperation:
evol_static %>% ggplot(aes(x = step, y = cooperation_rate, color = money)) +
  geom_point(size = 0.5)

# fitness:
evol_static %>% ggplot(aes(x = step, y = average_fitness, color = money)) +
  geom_point(size = 0.5)

# score-balances
evol_static %>% ggplot(aes(x = step, y = average_score_balance, color = money)) +
  geom_point(size = 0.5)

# k threshold
evol_static %>% ggplot(aes(x = step, y = average_threshold, color = money)) +
  geom_point(size = 0.5)


# # plot evolutions, in money or reputation treatments, with learning but no mutation
evol_learning <- df %>% 
  filter(offspring == T & mutation == F) %>% 
  pivot_longer(money, values_to = "money") 

# cooperation:
evol_learning %>% ggplot(aes(x = step, y = cooperation_frequency, color = money)) +
  geom_point(size = 0.5)

# fitness:
evol_learning %>% ggplot(aes(x = step, y = average_fitness, color = money)) +
  geom_point(size = 0.5)

# score-balances
evol_learning %>% ggplot(aes(x = step, y = average_score_balance, color = money)) +
  geom_point(size = 0.5)

# k threshold
evol_learning %>% ggplot(aes(x = step, y = average_threshold, color = money)) +
  geom_point(size = 0.5)




# # plot evolutions, in money or reputation treatments, with learning AND mutation
evol_learning_mutation <- df %>% 
  filter(offspring == T & mutation == T) %>% 
  pivot_longer(money, values_to = "money") 

# cooperation:
evol_learning_mutation %>% ggplot(aes(x = step, y = cooperation_frequency, color = money)) +
  geom_point(size = 0.5)

# fitness:
evol_learning_mutation %>% ggplot(aes(x = step, y = average_fitness, color = money)) +
  geom_point(size = 0.5)

# score-balances
evol_learning_mutation %>% ggplot(aes(x = step, y = average_score_balance, color = money)) +
  geom_point(size = 0.5)

# k threshold
evol_learning %>% ggplot(aes(x = step, y = average_threshold, color = money)) +
  geom_point(size = 0.5)

