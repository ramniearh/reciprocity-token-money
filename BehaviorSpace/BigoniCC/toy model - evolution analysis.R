library(tidyverse)
library(here)
library(janitor)

df <- 
  here("population 100 cost2 benefit 5 replacement 0.01", "token-reciprocity exp complete-p100-c2b5-e1 streamlined-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  rename(grudger_memory = grudger_memory_cap) %>% 
  rename(token_share = token) %>% 
  rename(step = x_step, run_number = x_run_number) %>% 
  rename(n_suckers = count_turtles_with_my_strategy_sucker, n_cheaters = count_turtles_with_my_strategy_cheater, n_grudgers = count_turtles_with_my_strategy_grudger, n_tokens = count_turtles_with_my_strategy_token)

glimpse(df)

# plot evolution of strategies in runs with cheaters and grudgers

evolCG <- df %>% 
  filter(step %% 10 == 0) %>% 
  filter(sucker == "false" & cheater == "true" & grudger_memory > 0 & token_share == 0) 
view(evolCG)

ggplot(evolCG, aes(x = step, y = total_welfare)) +
  #geom_point(aes(y = cheater_welfare)) +
  geom_point(aes(y = grudger_welfare, color = grudger_memory))

# plot evolution of strategies in runs with all 4 strategies present

evol <- df %>% 
  filter(step %% 10 == 0) %>% 
  filter(sucker == "true" & cheater == "true" & grudger_memory > 0 & token_share > 0) 

  
