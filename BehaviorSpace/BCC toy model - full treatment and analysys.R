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
## TO ADD: change bare-variable columns from e.g. "true" to TRUE
#glimpse(df)
#structure(df)
#view(df)

# Plot evolution of strategies over 300 steps for situations where all 5 strategies co-exist

## create new df using only turtle-counts per strategy
df_evol_all5 <- df %>% 
  filter( sucker == "true" & cheater == "true" & grudger == "true" & token == "true" & rlm == "true" ) %>%
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
  geom_point(alpha = 0.01, size = 1) +
  geom_line(data = long_means, linewidth = 1.5)

# Cross-plot pure and 2x2 strategy interactions at 300 steps

df_300 <- df %>% 
  filter( step == 300 ) %>% 
  select( -starts_with("n_"), -step ) %>% 
  arrange(run_number, cost, benefit, grudger_memory, token_share) %>% 
  glimpse  
  

## plot pure strategies: sucker

df_300 %>% 
  filter( token == "false" ) %>% #sucker == "true"  & cheater == "false" & grudger == "true" & token == "false" & rlm == "false") %>% 
  glimpse### NO FALSE TOKENS BUG - except at 0 steps?



## plot sucker x sucker

SxS <- df_300 %>% 
  filter( sucker == "true" & cheater == "false" & grudger == "false" & token == "false" & rlm == "false" ) %>% 
view


df %>% filter(token=="false") %>% glimpse







#  + ggtitle("all models are wrong, some are usef-#%&&#%-RIDICULOUS")

# factor(long_df_evol_all5$strategy, levels = c("RLM", "token", "grudger", "cheater", "sucker"))
#+  scale_fill_manual(values = c("violet", "blue", "orange", "grey", "green"), breaks = c("RLM", "token", "grudger", "cheater", "sucker"))









