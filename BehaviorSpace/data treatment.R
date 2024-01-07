library(tidyverse)
library(here)
library(janitor)

df <- 
  here("population 100 cost2 benefit 5 replacement 0.01", "token-reciprocity exp complete-p100-c2b5-e1 streamlined-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  rename(grudger_memory = grudger_memory_cap) %>% 
  rename(token_share = token) 
glimpse(df)

data_1000 <- filter(df, x_step == 1000)

# Plot total payoffs with pure strategies after 1000 rounds

pure_sucker <- data_1000 %>% 
  filter(sucker == "true" & cheater == "false" & grudger_memory == 0 & token_share == 0) %>% 
  select(x_run_number, sucker_welfare, total_welfare) %>%
  mutate(type = "sucker", order = 1:25)

pure_cheater <- data_1000 %>% 
  filter(sucker == "false" & cheater == "true" & grudger_memory == 0 & token_share == 0) %>% 
  select(x_run_number, cheater_welfare, total_welfare) %>% 
  mutate(type = "cheater", order = 26:50)

pure_grudger <- data_1000 %>% 
  filter(sucker == "false" & cheater == "false" & grudger_memory > 0 & token_share == 0) %>% 
  select(x_run_number, grudger_welfare, total_welfare, grudger_memory) %>% 
  mutate(type = "grudger", order = 51:150)

pure_token <- data_1000 %>% 
  filter(sucker == "false" & cheater == "false" & grudger_memory == 0 & token_share > 0) %>% 
  select(x_run_number, token_welfare, total_welfare, token_share) %>% 
  mutate(type = "token", order = 151:275)

pure_1000 <- bind_rows(pure_sucker, pure_cheater, pure_grudger, pure_token)
view(pure_1000)

ggplot(pure_1000, aes(x = order, y = total_welfare, color = type)) + 
  geom_point()

# Plot total payoffs with mixed sucker - cheater population
only_sucker_cheater_1000 <- data_1000 %>% 
  filter(sucker == "true" & cheater == "true" & grudger_memory == 0 & token_share == 0) %>% 
  select(x_run_number, total_welfare, sucker_welfare, cheater_welfare) %>%
  mutate(type = "sucker+cheater", order = 276:300)
view(only_sucker_cheater_1000)

ggplot(only_sucker_cheater_1000, aes(x = x_run_number, y = total_welfare, color = type)) + 
  geom_point()

# Plot total payoffs with mixed grudger - cheater population
only_cheater_grudger_1000 <- data_1000 %>% 
  filter(sucker == "false" & cheater == "true" & grudger_memory > 0 & token_share == 0) %>% 
  select(x_run_number, total_welfare, grudger_welfare, cheater_welfare, grudger_memory) %>%
  mutate(type = "cheater+grudger", order = 301:400)
view(only_cheater_grudger_1000)

ggplot(only_cheater_grudger_1000, aes(x = x_run_number, y = total_welfare, color = type)) + 
  geom_point()

## replot with different shapes for each payoff type
ggplot(only_cheater_grudger_1000, aes(x = x_run_number, y = total_welfare, color = type)) + 
  geom_point()

# Plot total payoffs with mixed token - cheater population
only_token_cheater_1000 <- data_1000 %>% 
  filter(sucker == "false" & cheater == "true" & grudger_memory == 0 & token_share > 0) %>% 
  select(x_run_number, total_welfare, cheater_welfare, token_welfare, token_share) %>%
  mutate(type = "cheater+token", order = 401:525)

ggplot(only_token_cheater_1000, aes(x = x_run_number, y = total_welfare, color = type)) + 
  geom_point()

# Plot total payoffs with four-strategy mixed population
all_4_1000 <- data_1000 %>% 
  filter(sucker == "true" & cheater == "true" & grudger_memory > 0 & token_share > 0) %>% 
  select(x_run_number, total_welfare, sucker_welfare, cheater_welfare, grudger_welfare, token_welfare, grudger_memory, token_share) %>%
  mutate(type = "sucker+cheater+grudger+token", order = 526:1025)

# Plot compared payoffs: pure strategies, sucker-cheater, cheater-grudger, all4
pures_SC_CG_CT_4_1000 <- bind_rows(pure_1000, only_sucker_cheater_1000, only_cheater_grudger_1000, only_token_cheater_1000, all_4_1000)
view(pures_SC_CG_CT_4_1000)        

ggplot(pures_SC_CG_CT_4_1000, aes(x = order, y = total_welfare, color = type)) + 
  geom_point()
  
# #reorder legend
new_pures_SC_CG_CT_4_1000 <- pures_SC_CG_CT_4_1000
new_pures_SC_CG_CT_4_1000$type <- factor(new_pures_SC_CG_CT_4_1000$type, levels = c('sucker', 'cheater', 'grudger', 'token', 'sucker+cheater', 'cheater+grudger', 'cheater+token', 'sucker+cheater+grudger+token'))
ggplot(new_pures_SC_CG_CT_4_1000, aes(x = order, y = total_welfare, color = type)) + 
  geom_point()

  