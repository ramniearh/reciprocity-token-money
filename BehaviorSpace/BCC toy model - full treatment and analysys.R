library(tidyverse)
library(here)
library(janitor)

df <- 
  here("BigoniCC", "BigoniCC - more than memory - streamlined - BehaviorSpace complete-p100-c2b5-e1%-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  rename(step = x_step, run_number = x_run_number) %>% 
  rename(n_suckers = count_turtles_with_my_strategy_sucker, n_cheaters = count_turtles_with_my_strategy_cheater, n_grudgers = count_turtles_with_my_strategy_grudger, n_tokens = count_turtles_with_my_strategy_token, n_rlm = count_turtles_with_my_strategy_rlm, rlm_balances = sum_my_balance_of_turtles, circulating_tokens = count_turtles_with_has_token_true)

glimpse(df)
#view(df)

df_test <- df %>% 
  