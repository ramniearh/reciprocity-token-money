library(tidyverse)
library(here)
library(janitor)
here()

# Acquire data: evolution and final payoffs for all 6 strategies interacting

df <- 
  here("GitHub", "reciprocity-token-money", "BehaviorSpace", "2nd-gen - global choice", "money-token-reciprocity - 2nd gen with choice global probabilistic choice - all 6 strategies - vary benefit-table - fullRLM.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  rename(step = x_step, run_number = x_run_number) %>% 
  rename(n_suckers = count_turtles_with_my_strategy_sucker, n_cheaters = count_turtles_with_my_strategy_cheater, n_grudgers = count_turtles_with_my_strategy_grudger, n_reputation = count_turtles_with_my_strategy_reputation,n_tokens = count_turtles_with_my_strategy_token, n_rlm = count_turtles_with_my_strategy_rlm) %>% 
  select(-evolve, - population, -replacement_rate, -grudger_memory, -sucker, -cheater, -reputation, -grudger, -token, -rlm, -klrm, -leaky ) %>% 
  select(-stopper_100, -stopper_500, -token_share, -min_k_score, -visibility, -oversupply)

df %>% head(10)
summary(df)

# Cross-plot welfares at 500 steps

df_500 <- df %>% 
  filter( step == 500 ) %>% 
  select( -starts_with("n_"), -step ) %>% 
  arrange(run_number, cost, benefit) %>% 
  select(-total_payoffs) %>% 
  pivot_longer(contains("payoffs")) 
  
# glimpse()

df_500 %>% 
  ggplot(aes(x=benefit/cost, y = value, color = name)) +
  geom_point() #+ ylim(0, 1000000)

# Plot evolution of strategies over 1000 steps

## create new df using only turtle-counts per strategy
df_evol <- df %>% 
  select(-contains("payoffs")) %>% 
  filter(step <= 200) %>% 
  arrange(step, run_number, cost, benefit)

## compute means of all observations per strategy at each time step
means <- df_evol %>% 
  group_by(step) %>% 
  summarize_at(
    vars(n_suckers, n_cheaters, n_grudgers, n_reputation, n_tokens, n_rlm),
    list(mean)
  ) %>% 
  pivot_longer(n_suckers:n_rlm) %>% 
  rename(strategy = name, share_of_agents = value)

# plot per-step strategy evolution (points) and mean (line)
long_df_evol <- df_evol %>% 
  pivot_longer(n_suckers:n_rlm, names_to = "strategy", values_to = "share_of_agents")

long_df_evol %>%
  ggplot(aes(x = step, y = share_of_agents, color = strategy)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_line(data = means, linewidth = 1.5)
