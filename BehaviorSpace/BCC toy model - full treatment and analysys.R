library(tidyverse)
library(here)
library(janitor)

df <- 
  here("BigoniCC", "BigoniCC - more than memory - streamlined - BehaviorSpace complete-p100-c2b5-e1%-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  rename(step = x_step, run_number = x_run_number) %>% 
  rename(n_suckers = count_turtles_with_my_strategy_sucker, n_cheaters = count_turtles_with_my_strategy_cheater, n_grudgers = count_turtles_with_my_strategy_grudger, n_tokens = count_turtles_with_my_strategy_token, n_rlm = count_turtles_with_my_strategy_rlm, rlm_balances = sum_my_balance_of_turtles, circulating_tokens = count_turtles_with_has_token_true) %>% 
  select(-evolve, - population, -replacement_rate ) 

glimpse(df)

# Plot evolution of strategies over 300 steps for situations where all 5 strategies co-exist

## create new df using only turtle-counts per strategy
df_evol_all5 <- df %>% 
  filter( sucker == "true" & cheater == "true" & grudger == "true" & token == "true" & rlm == "true") %>%
  select(-cheater, -sucker, -grudger, -token, -rlm, -contains("welfare")) %>% 
  arrange(step, run_number, cost, benefit, grudger_memory, token_share) %>% 
  glimpse  

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
view(long_means)

# plot per-step strategy evolution (points) and mean (line)
long_df_evol_all5 <- df_evol_all5 %>% 
  pivot_longer(n_suckers:n_rlm, names_to = "strategy", values_to = "share_of_agents")
view(long_df_evol_all5)

long_df_evol_all5 %>%
  ggplot(aes(x = step, y = share_of_agents, color = strategy)) +
  geom_point() +
  geom_line(data = long_means)


# Cross-plot 2x2 strategy interactions 

SvS <- df %>% 












#  + ggtitle("all models are wrong, some are usef-#%&&#%-RIDICULOUS")

# factor(long_df_evol_all5$strategy, levels = c("RLM", "token", "grudger", "cheater", "sucker"))
#+  scale_fill_manual(values = c("violet", "blue", "orange", "grey", "green"), breaks = c("RLM", "token", "grudger", "cheater", "sucker"))









