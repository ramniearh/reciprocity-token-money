library(here)
library(tidyverse)
library(janitor)

###################################
## PLOT FOR MONEY (5-strategies) treatment
###################################
# Import ant treat data:

df_large_pop <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "7. large population tests", "money-reciprocity 3.0 - BehaviorSpace half-large population - sensitivity analysis full sweep 100x5 500r-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  select(-quid_pro_quo, -initial_money,-debt_threshold) %>%
  select(-initial_reputation,-reputation_threshold) %>% 
  select(-forgiveness) %>% 
  select(-evolutionary_updating) %>% 
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with("n_"), -starts_with(("x_sum"))) %>%
  select(-starts_with("fitness")) %>% 
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    #average_memory_length = x_sum_length_memory_of_turtles_count_turtles,
  ) %>% 
  mutate(bc_ratio = as.factor(bc_ratio)) %>% 
  mutate( money_prevalence =
            count_moneys / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects)
  ) 

# Analysis:

# Summarize cooperation rate data at end of run
df_large_pop %>%
  group_by(step, bc_ratio) %>% 
  summarise(mean(cooperation_rate), sd(cooperation_rate), mean(money_prevalence))

# Plot cooperation rates x benefit/cost ratios at selected time steps
mean_money_prevalence_bc <- df_large_pop %>%
  filter(step %in% c(10000)) %>%
  group_by(bc_ratio) %>%
  summarise(mean_money_prevalence_bc = mean(money_prevalence))

df_large_pop_with_means <- df_large_pop %>%
  filter(step %in% c(10000)) %>%
  merge(mean_money_prevalence_bc, by = "bc_ratio")

df_large_pop_with_means %>%
  filter(step %in% c(10000)) %>%
  ggplot(aes(x=bc_ratio, y=cooperation_rate, color=mean_money_prevalence_bc)) +
  geom_boxplot() +
  #facet_wrap(~step) +
  ggtitle("Five strategies, pop= 1250, liquidity = 1: Cooperation rates at 10000 steps for different benefit/cost ratios")
#   
#add color for winners! :()
#stat_summary(color = mean(money_count_prevalence)) +

###
# Plot distribution of cooperation rates at selected time steps and benefit/cost ratios
# df_large_pop %>%
#   mutate(step = as.factor(step)) %>% 
#   filter(step %in% c(1000, 5000, 10000)) %>% 
#   ggplot(aes(x=cooperation_rate, fill = step)) +
#   geom_histogram(position = "identity", alpha = 0.3) +
#   facet_wrap( ~ bc_ratio, labeller = label_both) +
#   ggtitle("Five strategies, large populations: distribution of cooperation rates at selected time steps")

  
summary_df_large_pop <- df_large_pop %>% 
  filter(step < 10000) %>% 
  pivot_longer(starts_with("count"), names_to = "strategy", values_to = "survivors") %>% 
  mutate(
    strategy = case_when(
      strategy == "count_cooperators" ~ "cooperators",
      strategy == "count_defectors" ~ "defectors",
      strategy == "count_directs" ~ "direct reciprocators",
      strategy == "count_indirects" ~ "indirect reciprocators",
      strategy == "count_moneys" ~ "money users",
      )
  ) %>% 
  group_by(bc_ratio, step, strategy) %>%
  summarise(
    mean_survivors = mean(survivors),
    sd_survivors = sd(survivors)
    ) 

summary_df_large_pop %>% 
  #filter(initial_money == 5) %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
  facet_wrap(~ bc_ratio, labeller = label_both) +
  ggtitle("Five strategies, large populations: evolution of surviving strategies (pop=1250, liquidity=1)")

###################################
## REPEAT FOR NO-MONEY BASELINE
###################################

# Import ant treat data:

df_no_money_large_pop <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "7. large population tests", "money-reciprocity 3.0 - BehaviorSpace half-large population - sensitivity analysis full sweep 125x4 250r-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  select(-quid_pro_quo, -initial_money,-debt_threshold) %>%
  select(-initial_reputation,-reputation_threshold) %>% 
  select(-forgiveness) %>% 
  select(-evolutionary_updating) %>% 
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with("n_"), -starts_with(("x_sum"))) %>%
  select(-starts_with("fitness")) %>% 
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    #average_memory_length = x_sum_length_memory_of_turtles_count_turtles,
  ) %>% 
  mutate(bc_ratio = as.factor(bc_ratio)) %>% 
  mutate( money_prevalence =
            count_moneys / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects)
  ) 

# Analysis:

# Summarize cooperation rate data at end of run
df_no_money_large_pop %>%
  group_by(step, bc_ratio) %>% 
  summarise(mean(cooperation_rate), sd(cooperation_rate), mean(money_prevalence))

# Plot cooperation rates x benefit/cost ratios at selected time steps
mean_no_money_prevalence_bc <- df_no_money_large_pop %>%
  filter(step %in% c(10000)) %>%
  group_by(bc_ratio) %>%
  summarise(mean_money_prevalence_bc = mean(money_prevalence))

df_no_money_large_pop_with_means <- df_no_money_large_pop %>%
  filter(step %in% c(10000)) %>%
  merge(mean_money_prevalence_bc, by = "bc_ratio")

df_no_money_large_pop_with_means %>%
  filter(step %in% c(10000)) %>%
  ggplot(aes(x=bc_ratio, y=cooperation_rate, color=mean_money_prevalence_bc)) +
  geom_boxplot() +
  #facet_wrap(~step) +
  ggtitle("Four strategies, pop= 1250: Cooperation rates at 10000 steps for different benefit/cost ratios")
#   
#add color for winners! :()
#stat_summary(color = mean(money_count_prevalence)) +

###
# Plot distribution of cooperation rates at selected time steps and benefit/cost ratios
# df_large_pop %>%
#   mutate(step = as.factor(step)) %>% 
#   filter(step %in% c(1000, 5000, 10000)) %>% 
#   ggplot(aes(x=cooperation_rate, fill = step)) +
#   geom_histogram(position = "identity", alpha = 0.3) +
#   facet_wrap( ~ bc_ratio, labeller = label_both) +
#   ggtitle("Five strategies, large populations: distribution of cooperation rates at selected time steps")


summary_df_no_money_large_pop <- df_no_money_large_pop %>% 
  filter(step < 10000) %>% 
  pivot_longer(starts_with("count"), names_to = "strategy", values_to = "survivors") %>% 
  mutate(
    strategy = case_when(
      strategy == "count_cooperators" ~ "cooperators",
      strategy == "count_defectors" ~ "defectors",
      strategy == "count_directs" ~ "direct reciprocators",
      strategy == "count_indirects" ~ "indirect reciprocators",
      strategy == "count_moneys" ~ "money users",
    )
  ) %>% 
  group_by(bc_ratio, step, strategy) %>%
  summarise(
    mean_survivors = mean(survivors),
    sd_survivors = sd(survivors)
  ) 

summary_df_no_money_large_pop %>% 
  #filter(initial_money == 5) %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
  facet_wrap(~ bc_ratio, labeller = label_both) +
  ggtitle("Four strategies, pop=1250: evolution of surviving strategies")
