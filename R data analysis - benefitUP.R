library(tidyverse)
library(here)
library(janitor)
here()

df <- 
  here("BehaviorSpace", "benefitUP", "BigoniCC - more than memory - streamlined - BehaviorSpace benefitUP-p100-e1%-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  rename(step = x_step, run_number = x_run_number) %>% 
  rename(n_suckers = count_turtles_with_my_strategy_sucker, n_cheaters = count_turtles_with_my_strategy_cheater, n_grudgers = count_turtles_with_my_strategy_grudger, n_tokens = count_turtles_with_my_strategy_token, n_rlm = count_turtles_with_my_strategy_rlm, rlm_balances = sum_my_balance_of_turtles, circulating_tokens = count_turtles_with_has_token_true) %>% 
  select(-evolve, - population, -replacement_rate ) 
df %>% head(10)


# Cross-plot welfare for pure and 2x2 strategy interactions at 500 steps

df_500 <- df %>% 
  filter( step == 500 ) %>% 
  select( -starts_with("n_"), -step ) %>% 
  arrange(run_number, cost, benefit, grudger_memory, token_share) %>% 
  glimpse()

df_500_S <- df_500 %>% 
  filter( sucker == "true" & cheater == "false" & grudger == "false" & token == "false" & rlm == "false" & token_share == 50 & grudger_memory == 5 & rlm_oversupply == "false") 
df_500_SC <- df_500 %>% 
  filter( sucker == "true" & cheater == "true" & grudger == "false" & token == "false" & rlm == "false" & token_share == 50 & grudger_memory == 5 & rlm_oversupply == "false")
df_500_SCG <- df_500 %>% 
  filter( sucker == "true" & cheater == "true" & grudger == "true" & token == "false" & rlm == "false" & token_share == 50 & grudger_memory == 5 & rlm_oversupply == "false")
df_500_SCGT <- df_500 %>% 
  filter( sucker == "true" & cheater == "true" & grudger == "true" & token == "true" & rlm == "false" & token_share == 50 & grudger_memory == 5 & rlm_oversupply == "false")
df_500_all5 <- df_500 %>% 
  filter( sucker == "true" & cheater == "true" & grudger == "true" & token == "true" & rlm == "true" & token_share == 50 & grudger_memory == 5 & rlm_oversupply == "false")

## plot total welfare at 500 steps for different strategy combinations:

df_500_S %>% 
  select(-total_welfare, -avg_welfare) %>% 
  pivot_longer(contains("welfare")) %>% 
  ggplot(aes(x=benefit/cost, y = value, color = name)) +
  geom_jitter() +
  ylim(0, 1000000)

df_500_SC %>% 
  select(-total_welfare, -avg_welfare) %>% 
  pivot_longer(contains("welfare")) %>% 
  ggplot(aes(x=benefit/cost, y = value, color = name)) +
  geom_jitter() +
  ylim(0, 1000000)

df_500_SCG %>% 
  select(-total_welfare, -avg_welfare) %>% 
  pivot_longer(contains("welfare")) %>% 
  ggplot(aes(x=benefit/cost, y = value, color = name)) +
  geom_jitter() +
  ylim(0, 1000000)

df_500_SCGT %>% 
  select(-total_welfare, -avg_welfare) %>% 
  pivot_longer(contains("welfare")) %>% 
  ggplot(aes(x=benefit/cost, y = value, color = name)) +
  geom_jitter() +
  ylim(0, 1000000)

df_500_all5 %>% 
  select(-total_welfare, -avg_welfare) %>% 
  pivot_longer(contains("welfare")) %>% 
  ggplot(aes(x=benefit/cost, y = value, color = name)) +
  geom_jitter() +
  ylim(0, 1000000)

# Plot evolution of strategies over 500 steps for situations where all 5 strategies co-exist

## create new df using only turtle-counts per strategy
df_evol_all5 <- df %>% 
  filter( sucker == "false" & cheater == "false" & grudger == "false" & token == "true" & rlm == "true" ) %>%
  select(-cheater, -sucker, -grudger, -token, -rlm, -contains("welfare")) %>% 
  arrange(step, run_number, cost, benefit, grudger_memory, token_share)

## compute means of all observations per strategy at each time step
means <- df_evol_all5 %>% 
  group_by(step) %>% 
  summarize_at(
    vars(n_suckers, n_cheaters, n_grudgers, n_tokens, n_rlm),
    list(mean)
  )
long_means <- means %>% 
  pivot_longer(n_suckers:n_rlm) %>% 
  rename( strategy = name, share_of_agents = value)

# plot per-step strategy evolution (points) and mean (line)
long_df_evol_all5 <- df_evol_all5 %>% 
  pivot_longer(n_suckers:n_rlm, names_to = "strategy", values_to = "share_of_agents")

long_df_evol_all5 %>%
  ggplot(aes(x = step, y = share_of_agents, color = strategy)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_line(data = long_means, linewidth = 1.5)
