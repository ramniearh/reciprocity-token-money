library(tidyverse)
library(here)
library(janitor)
here()

# Acquire data: evolution and final payoffs for all 6 strategies interacting

df <- 
  here("GitHub", "reciprocity-token-money", "BehaviorSpace", "2nd-gen - persistence", "money-token-reciprocity - more than memory - 2nd gen with breeds persistence-assortativity-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  rename(step = x_step, run_number = x_run_number) %>% 
  rename(n_suckers = count_turtles_with_my_strategy_sucker, n_cheaters = count_turtles_with_my_strategy_cheater, n_grudgers = count_turtles_with_my_strategy_grudger, n_ledger = count_turtles_with_my_strategy_ledger) 

#df %>% head(10)
#summary(df)

# Cross-plot welfares at 1000 steps

df_w <- df %>% 
  filter( step == 1000 ) %>% 
  select( -starts_with("n_"), -step ) %>% 
  arrange(run_number, cost, benefit, link_persistence_prob) %>% 
  select(-total_welfare, -avg_welfare) %>% 
  pivot_longer(contains("welfare"), values_to = "welfare_at_1000_steps") 

#glimpse(df_500$link_persistence_prob)

df_w %>% 
  ggplot(aes(x=link_persistence_prob, y = welfare_at_1000_steps, color = name)) +
  geom_jitter() #+ ylim(0, 1000000)

# Plot evolution of strategies over 1000 steps

## create new df using only turtle-counts per strategy
df_evol <- df %>% 
  select(-contains("welfare")) %>% 
  filter(step <= 1000) %>% 
  arrange(step, run_number, cost, benefit)

## compute means of all observations per strategy at each time step
means <- df_evol %>% 
  group_by(step) %>% 
  summarize_at(
    vars(n_suckers, n_cheaters, n_grudgers, n_ledger),
    list(mean)
  ) %>% 
  pivot_longer(n_suckers:n_ledger) %>% 
  rename(strategy = name, share_of_agents = value)

# plot per-step strategy evolution (points) and mean (line)

df_evol %>%
  pivot_longer(n_suckers:n_ledger, names_to = "strategy", values_to = "share_of_agents") %>% 
  ggplot(aes(x = step, y = share_of_agents, color = strategy)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_line(data = means, linewidth = 1.5)

# small persistence:

df_evol_low_persist <- df %>% 
  select(-contains("welfare")) %>% 
  filter(step <= 1000, link_persistence_prob == 0) %>% 
  arrange(step, run_number, cost, benefit)
  
means_low_persist <- df_evol_low_persist %>% 
  group_by(step) %>% 
  summarize_at(
    vars(n_suckers, n_cheaters, n_grudgers, n_ledger),
    list(mean)
  ) %>% 
  pivot_longer(n_suckers:n_ledger) %>% 
  rename(strategy = name, share_of_agents = value)

df_evol_low_persist %>% 
  pivot_longer(n_suckers:n_ledger, names_to = "strategy", values_to = "share_of_agents") %>% 
  ggplot(aes(x = step, y = share_of_agents, color = strategy)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_line(data = means_low_persist, linewidth = 1.5)

# high persistence:

df_evol_high_persist <- df %>% 
  select(-contains("welfare")) %>% 
  filter(step <= 1000, link_persistence_prob == 0.1) %>% 
  arrange(step, run_number, cost, benefit)

means_high_persist <- df_evol_high_persist %>% 
  group_by(step) %>% 
  summarize_at(
    vars(n_suckers, n_cheaters, n_grudgers, n_ledger),
    list(mean)
  ) %>% 
  pivot_longer(n_suckers:n_ledger) %>% 
  rename(strategy = name, share_of_agents = value)

df_evol_high_persist %>% 
  pivot_longer(n_suckers:n_ledger, names_to = "strategy", values_to = "share_of_agents") %>% 
  ggplot(aes(x = step, y = share_of_agents, color = strategy)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_line(data = means_high_persist, linewidth = 1.5)

#structure(df_evol$link_persistence_prob)
