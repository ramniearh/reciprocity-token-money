library(tidyverse)
library(here)
library(janitor)

df <- 
  here("population 100 cost2 benefit 5 replacement 0.01", "token-reciprocity exp complete-p100-c2b5-e1 streamlined-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()

structure(df)

# Plot total payoffs with pure strategies after 1000 rounds

data_1000 <- filter(df, x_step == 1000)
data_pure_1000 <- filter(data_1000, (sucker == "true" & cheater == "false" & grudger_memory_cap == 0 & token == 0) | (sucker == "false" & cheater == "true" & grudger_memory_cap == 0 & token == 0) | (sucker == "false" & cheater == "false" & grudger_memory_cap > 0 & token == 0) | (sucker == "false" & cheater == "false" & grudger_memory_cap == 0 & token > 0))  
view(data_pure_1000)

typify <- function( X ) {
  if(X$cheater == "true") { r <- "cheater" }
  # if (X$sucker == "true") { r <- "sucker" }
  # if (X$grudger_memory_cap > 0) { r <- "grudger"}
  # if (X$token > 0) { r <- "token" }
  return(r)
}

typify(data_pure_1000)

DP1test <-
  data_pure_1000 %>%
  group_by()

view(DP1test)  
  
  
  
  
  
  
data_pure_1000 %>%
  ggplot(aes(x = x_run_number)) + 
  geom_point(aes(y = sucker_welfare))

     
# Plot payoff evolution with only suckers and cheaters:

data_SC <- subset(df, sucker == "true" & cheater == "true" & grudger_memory_cap == 0 & token == 0)
view(data_SC)
data_SC %>%
  ggplot(aes(x = x_step, color = x_run_number)) + 
  geom_line(aes( y = count_turtles_with_my_strategy_sucker )) + 
  geom_line(aes( y = count_turtles_with_my_strategy_cheater )) 

# Plot payoff evolution with suckers, cheaters and grudgers:

data_SCG <- subset(df, sucker == "true" & cheater == "true" & grudger_memory_cap > 0 & token == 0)
view(data_SCG)
data_SCG %>%
  ggplot(aes(x = x_step)) + 
  geom_line(aes( y = count_turtles_with_my_strategy_sucker )) + 
  geom_line(aes( y = count_turtles_with_my_strategy_cheater )) +
  geom_line(aes( y = count_turtles_with_my_strategy_grudger )) 
  