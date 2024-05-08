library(tidyverse)
library(here)
library(janitor)
here()
df <- 
  here("token-money-reciprocity extended token-grudger-cheater-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  rename(step = x_step, run_number = x_run_number) %>% 
  rename(
    n_suckers = count_turtles_with_my_strategy_sucker, 
    n_cheaters = count_turtles_with_my_strategy_cheater, 
    n_grudgers = count_turtles_with_my_strategy_grudger, 
    n_tokens = count_turtles_with_my_strategy_token, 
    n_rlm = count_turtles_with_my_strategy_rlm, 
    circulating_tokens = count_turtles_with_has_token_true) %>% 
  select(-evolve, - population, -replacement_rate ) 

df_1000 <- df %>% 
  filter( step == 1000 ) %>% 
  select( -starts_with("n_"), -step ) %>% 
  arrange(run_number, cost, benefit, grudger_memory, token_share)
glimpse(df_1000)

df_1000_GT <- df_1000 %>% 
  filter( 
    sucker == "false" & 
      cheater == "false" & 
      grudger == "true" & 
      token == "true" & 
      rlm == "false") %>% 
  select(-total_welfare, -avg_welfare) %>% 
  pivot_longer(contains("welfare")) %>% 
  ggplot(aes(x=grudger_memory, y = value, color = name)) +
  geom_jitter()

view(df_1000)
