##### R script: Money and Generalized Cooperation #####
## Auxiliary plot for no-evolution mechanics analysis. Evolution and cooperation rate charts
## Simulated data for money scenario, with  variation of liquidity and benefit/cost parameters. 
## No evolutionary updating: 100 repetitions, 500 steps
# ["N-coop" 100]
# ["N-defect" 100]
# ["N-direct" 100]
# ["N-indirect" 100]
# ["N-money" 100]
# ["benefit-to-cost-ratio" 1 2 10 100]
# ["initial-money" 0 0.25 1 5 100 1000]
# ["debt-threshold" 0]
# # ["quid-pro-quo?" true]
# # ["evolutionary-updating?" false]
# # ["initial-reputation" 1]
# # ["reputation-threshold" -1]
# # ["forgiveness?" true]
# # ["full-memory?" true]
# # ["error-noise" 0]
# # ["force-mutate-share" 0]
# cooperation-rate
# count cooperators
# count defectors
# count directs
# count indirects
# count moneys
# (sum [length memory] of turtles) / count turtles
# sum [balance] of cooperators
# sum [balance] of defectors
# sum [balance] of directs
# sum [balance] of indirects
# sum [balance] of moneys
# sum [score] of cooperators
# sum [score] of defectors
# sum [score] of directs
# sum [score] of indirects
# sum [score] of moneys
# fitness-cooperators-this-round
# fitness-defectors-this-round
# fitness-directs-this-round
# fitness-indirects-this-round
# fitness-moneys-this-round

### Plot poster variants
# Poster colorset:
  # "cooperators" = "#809ec2",
  # "defectors"="#d092a7",
  # "direct-reciprocators"="#f3a447",
  # "indirect-reciprocators"="#a5b592",
  # "money-users"="#7153a1"
# 
# Original colorset:
  # scale_color_manual(values = c("cooperators" = "#00B8E7",
  #                               "defectors"="#F8766D",
  #                               "direct-reciprocators"="#00BE67",
  #                               "indirect-reciprocators"="#CD9600",
  #                               "money-users"="#C77CFF"
  # )) +
  # scale_fill_manual(values = c("cooperators" = "#00B8E7",
  #                              "defectors"="#F8766D",
  #                              "direct-reciprocators"="#00BE67",
  #                              "indirect-reciprocators"="#CD9600",
  #                              "money-users"="#C77CFF"
  
# add variance moneys?
# 100 agents for each strategy, invariant

# 1. Import libraries and packages -------------------------------------------
library(here)
library(tidyverse)
library(janitor)
here()



# 2. Import and process data -------------------------------------------------

df_no_evol <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "1. No-evolution validation", "money-reciprocity 3.0 - BehaviorSpace CORE - 100x5 500s100r NOEVOL-table.csv") %>% 
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>% 
  select(-starts_with("count")) %>%
  rename(
    step = x_step, 
    run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    liquidity = initial_money,
    used_memory = x_sum_length_memory_of_turtles_count_turtles,
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio),
  )  

df_no_evol_no_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "1. No-evolution validation", "money-reciprocity 3.0 - BehaviorSpace CORE NOEVOL NOMONEY-table.csv") %>% 
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>% 
  select(-starts_with("count")) %>%
  rename(
    step = x_step, 
    run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    liquidity = initial_money,
    used_memory = x_sum_length_memory_of_turtles_count_turtles,
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio),
  )  

# 3. Draw main plots: cooperation, fitness, memory, scores, balances -----------

p_coop_no_evol = df_no_evol %>% 
  ggplot(aes(x=step)) +
  ylim(0, 1) +
  stat_summary(
    aes(y = cooperation_rate, shape = ""),
    fun.data = "median_hilow",
    geom = "point",
    size = 0.5,
    alpha = 0.9
  ) +
  stat_summary(
    aes(y = cooperation_rate, linetype = ""),
    fun.data = "median_hilow",
    geom = "errorbar",
    alpha = 0.6
  ) +
  labs(
    x = "Simulation time step",
    y = "Proportion (0-1)",
    color = "Share of survivors by strategy",
    fill = "Share of survivors by strategy",
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    #title = "No evolution: Cooperation rates (Median and IQR over 100 repetitions; total population = 500)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

# Fitness
p_fitness_no_evol = df_no_evol %>% 
  pivot_longer(cols = starts_with("fitness"), names_to = "strategy", values_to = "fitness_count") %>% 
  mutate(strategy =
           case_when(
             strategy == "fitness_cooperators_this_round" ~ "cooperators",
             strategy == "fitness_defectors_this_round" ~ "defectors",
             strategy == "fitness_directs_this_round" ~ "direct-reciprocators",
             strategy == "fitness_indirects_this_round" ~ "indirect-reciprocators",
             strategy == "fitness_moneys_this_round" ~ "money-users",
           )
  ) %>% 
  filter(bc_ratio == 100) %>%  
  filter(liquidity == 1) %>%  
  #filter(step > 0) %>%
  #filter(step < 100) %>% 
  ggplot(aes(x=step)) +
  #ylim(0, 1) +
  stat_summary(
    aes(y=fitness_count, color = strategy),
    fun.data = "median_hilow",
    geom = "line",
    linewidth = 0.75
  ) +
  stat_summary(
    aes(y=fitness_count, fill = strategy),
    fun.data = "median_hilow",
    geom = "ribbon",
    alpha = 0.2
  ) +
  scale_color_manual(values = c("cooperators" = "#F8766D",
                                "defectors"="#ABA300",
                                "direct-reciprocators"="#00BE67",
                                "indirect-reciprocators"="#00B8E7",
                                "money-users"="#C77CFF"
  )) +
  scale_fill_manual(values = c("cooperators" = "#F8766D",
                               "defectors"="#ABA300",
                               "direct-reciprocators"="#00BE67",
                               "indirect-reciprocators"="#00B8E7",
                               "money-users"="#C77CFF"
  )) +
  labs(
    x = "Simulation time step",
    y = "SUM end-of-turn fitness of agents on round", #REVIEW SUM/AVERAGE
    color = "Strategy",
    fill = "Strategy",
    title = "No evolution: fitness of strategy over time (Median and IQR over 100 repetitions; total population = 500)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_fitness_no_evol

# Scores
p_scores_no_evol = df_no_evol %>% 
  pivot_longer(cols = starts_with("sum_score"), names_to = "strategy", values_to = "sum_score_count") %>% 
  mutate(strategy =
           case_when(
             strategy == "sum_score_of_cooperators" ~ "cooperators",
             strategy == "sum_score_of_defectors" ~ "defectors",
             strategy == "sum_score_of_directs" ~ "direct-reciprocators",
             strategy == "sum_score_of_indirects" ~ "indirect-reciprocators",
             strategy == "sum_score_of_moneys" ~ "money-users",
           )
  ) %>% 
  filter(bc_ratio == 10) %>%  
  filter(liquidity == 1) %>%  
  #filter(step > 0) %>%
  filter(step < 100) %>% 
  ggplot(aes(x=step)) +
  #ylim(0, 1) +
  stat_summary(
    aes(y=sum_score_count, color = strategy),
    fun.data = "median_hilow",
    geom = "line",
    linewidth = 0.75
  ) +
  stat_summary(
    aes(y=sum_score_count, fill = strategy),
    fun.data = "median_hilow",
    geom = "ribbon",
    alpha = 0.2
  ) +
  scale_color_manual(values = c("cooperators" = "#F8766D",
                                "defectors"="#ABA300",
                                "direct-reciprocators"="#00BE67",
                                "indirect-reciprocators"="#00B8E7",
                                "money-users"="#C77CFF"
  )) +
  scale_fill_manual(values = c("cooperators" = "#F8766D",
                               "defectors"="#ABA300",
                               "direct-reciprocators"="#00BE67",
                               "indirect-reciprocators"="#00B8E7",
                               "money-users"="#C77CFF"
  )) +
  labs(
    x = "Simulation time step",
    y = "SUM end-of-turn scores of agents by strategy on round",
    color = "Strategy",
    fill = "Strategy",
    #title = "No evolution: fitness, balances, scores and cooperation rates (Median and IQR over 100 repetitions; total population = 500)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

# Balances
p_balances_no_evol = df_no_evol %>% 
  pivot_longer(cols = starts_with("sum_balance"), names_to = "strategy", values_to = "sum_balances_count") %>% 
  mutate(strategy =
           case_when(
             strategy == "sum_balance_of_cooperators" ~ "cooperators",
             strategy == "sum_balance_of_defectors" ~ "defectors",
             strategy == "sum_balance_of_directs" ~ "direct-reciprocators",
             strategy == "sum_balance_of_indirects" ~ "indirect-reciprocators",
             strategy == "sum_balance_of_moneys" ~ "money-users",
           )
  ) %>% 
  filter(bc_ratio == 10) %>%  
  filter(liquidity == 1) %>%  
  #filter(step > 0) %>%
  filter(step < 100) %>% 
  ggplot(aes(x=step)) +
  #ylim(0, 1) +
  stat_summary(
    aes(y=sum_balances_count, color = strategy),
    fun.data = "median_hilow",
    geom = "line",
    linewidth = 0.75
  ) +
  stat_summary(
    aes(y=sum_balances_count, fill = strategy),
    fun.data = "median_hilow",
    geom = "ribbon",
    alpha = 0.2
  ) +
  # scale_color_manual(values = c(  "cooperators" = "#809ec2",
  #                                 "defectors"="#d092a7",
  #                                 "direct-reciprocators"="#f3a447",
  #                                 "indirect-reciprocators"="#a5b592",
  #                                 "money-users"="#7153a1"
  # )) +
  # scale_fill_manual(values = c(  "cooperators" = "#809ec2",
  #                                "defectors"="#d092a7",
  #                                "direct-reciprocators"="#f3a447",
  #                                "indirect-reciprocators"="#a5b592",
  #                                "money-users"="#7153a1"
  # )) +
  labs(
    x = "Simulation time step",
    y = "SUM end-of-turn BALANCES of agents by strategy on round",
    color = "Strategy",
    fill = "Strategy",
    #title = "No evolution: fitness, balances, scores and cooperation rates (Median and IQR over 100 repetitions; total population = 500)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

# Memory
p_memory_no_evol = df_no_evol %>% 
  filter(bc_ratio == 10) %>%  
  filter(liquidity == 1) %>%  
  #filter(step > 0) %>%
  #filter(step < 1000) %>% 
  ggplot(aes(x=step)) +
  #ylim(0, 1) +
  stat_summary(
    aes(y=used_memory),
    fun.data = "median_hilow",
    geom = "line",
    linewidth = 0.75
  ) +
  stat_summary(
    aes(y=used_memory),
    fun.data = "median_hilow",
    geom = "ribbon",
    alpha = 0.2
  ) +
  # scale_color_manual(values = c(  "cooperators" = "#809ec2",
  #                                 "defectors"="#d092a7",
  #                                 "direct-reciprocators"="#f3a447",
  #                                 "indirect-reciprocators"="#a5b592",
  #                                 "money-users"="#7153a1"
  # )) +
  # scale_fill_manual(values = c(  "cooperators" = "#809ec2",
  #                                "defectors"="#d092a7",
  #                                "direct-reciprocators"="#f3a447",
  #                                "indirect-reciprocators"="#a5b592",
  #                                "money-users"="#7153a1"
  # )) +
  labs(
    x = "Simulation time step",
    y = "SUM LENGTH MEMORY of agents by strategy on round",
    color = "Strategy",
    fill = "Strategy",
    #title = "No evolution: fitness, balances, scores and cooperation rates (Median and IQR over 100 repetitions; total population = 500)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()



# Fitness
p_fitness_no_evol = df_no_evol %>% 
  pivot_longer(cols = starts_with("fitness"), names_to = "strategy", values_to = "fitness_count") %>% 
  mutate(strategy =
           case_when(
             strategy == "fitness_cooperators_this_round" ~ "cooperators",
             strategy == "fitness_defectors_this_round" ~ "defectors",
             strategy == "fitness_directs_this_round" ~ "direct-reciprocators",
             strategy == "fitness_indirects_this_round" ~ "indirect-reciprocators",
             strategy == "fitness_moneys_this_round" ~ "money-users",
           )
  ) %>% 
  filter(bc_ratio == 10) %>%  
  filter(liquidity == 1) %>%  
  #filter(step > 0) %>%
  #filter(step < 100) %>% 
  ggplot(aes(x=step)) +
  #ylim(0, 1) +
  stat_summary(
    aes(y=fitness_count, color = strategy),
    fun.data = "median_hilow",
    geom = "line",
    linewidth = 0.75
  ) +
  stat_summary(
    aes(y=fitness_count, fill = strategy),
    fun.data = "median_hilow",
    geom = "ribbon",
    alpha = 0.2
  ) +
  # scale_color_manual(values = c(  "cooperators" = "#809ec2",
  #                                 "defectors"="#d092a7",
  #                                 "direct-reciprocators"="#f3a447",
  #                                 "indirect-reciprocators"="#a5b592",
  #                                 "money-users"="#7153a1"
  # )) +
  # scale_fill_manual(values = c(  "cooperators" = "#809ec2",
  #                                "defectors"="#d092a7",
  #                                "direct-reciprocators"="#f3a447",
  #                                "indirect-reciprocators"="#a5b592",
  #                                "money-users"="#7153a1"
  # )) +
  labs(
    x = "Simulation time step",
    y = "SUM end-of-turn fitness of agents on round", #REVIEW SUM/AVERAGE
    color = "Strategy",
    fill = "Strategy",
    title = "No evolution: fitness of strategy over time (Median and IQR over 100 repetitions; total population = 500)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_fitness_no_evol

# Fitness
p_fitness_no_evol_no_money = df_no_evol_no_money %>% 
  pivot_longer(cols = starts_with("fitness"), names_to = "strategy", values_to = "fitness_count") %>% 
  mutate(strategy =
           case_when(
             strategy == "fitness_cooperators_this_round" ~ "cooperators",
             strategy == "fitness_defectors_this_round" ~ "defectors",
             strategy == "fitness_directs_this_round" ~ "direct-reciprocators",
             strategy == "fitness_indirects_this_round" ~ "indirect-reciprocators",
             strategy == "fitness_moneys_this_round" ~ "money-users",
           )
  ) %>% 
  filter(bc_ratio == 10) %>%  
  filter(liquidity == 1) %>%  
  #filter(step > 0) %>%
  #filter(step < 100) %>% 
  ggplot(aes(x=step)) +
  #ylim(0, 1) +
  stat_summary(
    aes(y=fitness_count, color = strategy),
    fun.data = "median_hilow",
    geom = "line",
    linewidth = 0.75
  ) +
  stat_summary(
    aes(y=fitness_count, fill = strategy),
    fun.data = "median_hilow",
    geom = "ribbon",
    alpha = 0.2
  ) +
  # scale_color_manual(values = c(  "cooperators" = "#809ec2",
  #                                 "defectors"="#d092a7",
  #                                 "direct-reciprocators"="#f3a447",
  #                                 "indirect-reciprocators"="#a5b592",
  #                                 "money-users"="#7153a1"
  # )) +
  # scale_fill_manual(values = c(  "cooperators" = "#809ec2",
  #                                "defectors"="#d092a7",
  #                                "direct-reciprocators"="#f3a447",
  #                                "indirect-reciprocators"="#a5b592",
  #                                "money-users"="#7153a1"
  # )) +
  labs(
    x = "Simulation time step",
    y = "SUM end-of-turn fitness of agents on round", #REVIEW SUM/AVERAGE
    color = "Strategy",
    fill = "Strategy",
    title = "No evolution: fitness of strategy over time (Median and IQR over 100 repetitions; total population = 500)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_fitness_no_evol_no_money

# 4. Generate visualizations -------------------------------------------------
p_coop_no_evol
p_fitness_no_evol
p_scores_no_evol
p_balances_no_evol
p_memory_no_evol