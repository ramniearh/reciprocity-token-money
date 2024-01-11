library(tidyverse)
library(here)
library(janitor)

df <- 
  here("BigoniCC", "BigoniCC - more than memory - streamlined complete-p100-c2b5-e1%-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  rename(step = x_step, run_number = x_run_number) %>% 
  rename(n_suckers = count_turtles_with_my_strategy_sucker, n_cheaters = count_turtles_with_my_strategy_cheater, n_grudgers = count_turtles_with_my_strategy_grudger, n_tokens = count_turtles_with_my_strategy_token)

glimpse(df)
#view(df)
