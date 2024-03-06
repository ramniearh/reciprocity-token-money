# if token=false in early data filter, all steps disappear?

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


df_evol_notok <- df %>% 
  filter( sucker == "true" & cheater == "true" & grudger == "true" & token == "false" & rlm == "true" )

df_evol_norlm <- df %>% 
  filter( sucker == "true" & cheater == "true" & grudger == "true" & token == "true" & rlm == "false" )

df_evol_nogru <- df %>% 
  filter( sucker == "true" & cheater == "true" & grudger == "false" & token == "true" & rlm == "true" )

df_evol_gru <- df %>% 
  filter( sucker == "false" & cheater == "false" & grudger == "true" & token == "false" & rlm == "false" )

df_evol_tok <- df %>% 
  filter( sucker == "false" & cheater == "false" & grudger == "false" & token == "true" & rlm == "false" )

df_evol_rlm <- df %>% 
  filter( sucker == "false" & cheater == "false" & grudger == "false" & token == "false" & rlm == "true" )

df_evol_suc <- df %>% 
  filter( sucker == "true" & cheater == "false" & grudger == "false" & token == "false" & rlm == "false" )

df_evol_che <- df %>% 
  filter( sucker == "false" & cheater == "true" & grudger == "false" & token == "false" & rlm == "false" )

## create new df using only turtle-counts per strategy
df_evol_all5 <- df %>% 
  filter( sucker == "true" & cheater == "true" & grudger == "true" & token == "false" & rlm == "true" ) %>%
  select(-cheater, -sucker, -grudger, -token, -rlm, -contains("welfare")) %>% 
  arrange(step, run_number, cost, benefit, grudger_memory, token_share) %>% 
  view