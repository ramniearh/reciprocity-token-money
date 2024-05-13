library(here)
library(tidyverse)
library(janitor)
here()
# Import ant treat data: 

# FIRST 200 REPETITIONS
df100x5A <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "100x5 - 200 repetitions to (increase)", "full sweep 100x5 money-reciprocity 3.0 - BEHAVE BehaviorSpace experiment-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  select(-quid_pro_quo, -evolutionary_updating) %>%
  select( #REVIEW (BASELINE-ONLY, ) REVALIDATE FOR FULL SWEEP 100x5
    -starts_with("n_"), #equal 5x100-distribution
    #-memory_size, removed for BS
    -initial_reputation,
    -reputation_threshold,
    -starts_with("sum_"), #scores and balances removed now, might be needed for further analysis
    -starts_with("x_sum_")
    ) %>%
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio
    #average_memory_length = x_sum_length_memory_of_turtles_count_turtles,
    #SCORES 
    #BALANCES 
  ) %>% 
  mutate(moneyness = initial_money - debt_threshold) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio),
    initial_money = as.factor(initial_money),
    debt_threshold = as.factor(debt_threshold),
    moneyness = as.factor(moneyness)
  )


# SECOND 300 REPETITIONS
df100x5B <-
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "100x5 - 200 repetitions to (increase)", "full sweep 100x5 300r money-reciprocity 3.0 - BEHAVE BehaviorSpace reference experiment.csv") %>%
  read.csv(skip = 6) %>%
  clean_names() %>%
  as_tibble() %>%
  select(-quid_pro_quo, -evolutionary_updating) %>%
  select( #REVIEW (BASELINE-ONLY, ) REVALIDATE FOR FULL SWEEP 100x5
    -starts_with("n_"), #equal 5x100-distribution
    #-memory_size, removed for BS
    -initial_reputation,
    -reputation_threshold,
    -starts_with("sum_"), #scores and balances removed now, might be needed for further analysis
    -starts_with("x_sum_")
  ) %>%
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio
    #average_memory_length = x_sum_length_memory_of_turtles_count_turtles,
    #SCORES
    #BALANCES
  ) %>%
  mutate(moneyness = initial_money - debt_threshold) %>%
  mutate(
    bc_ratio = as.factor(bc_ratio),
    initial_money = as.factor(initial_money),
    debt_threshold = as.factor(debt_threshold),
    moneyness = as.factor(moneyness)
  )

## COMBINED dataframes for 500 repetitions:
df100x5 <- bind_rows(df100x5A, df100x5B)
rm(df100x5A)
rm(df100x5B)


df100x5 %>% 
  filter(step == 10000) %>% 
  group_by(bc_ratio, initial_money, debt_threshold) %>% 
  summarise(mean(cooperation_rate), sd(cooperation_rate), median(cooperation_rate))

# Plot cooperation rates x benefit/cost ratios at selected time steps
df100x5 %>% 
  filter(step %in% c(5000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
  geom_boxplot() + 
  facet_grid(initial_money ~ debt_threshold, labeller = label_both) +
  ggtitle("Cooperation rates for different benefit/cost ratios at 5000 time steps")


# Plot distribution of cooperation rates at selected time steps and benefit/cost ratios
df100x5 %>%
  mutate(step = as.factor(step)) %>% 
  filter(step %in% c(5000), bc_ratio != 1) %>% 
  group_by(initial_money, debt_threshold) %>% 
  ggplot(aes(x=cooperation_rate, fill = bc_ratio)) +
  geom_histogram(position = "identity", alpha = 0.3) +
  facet_grid(initial_money ~ debt_threshold, labeller = label_both) +
  ggtitle("Distribution of cooperation rates counts at 5000 steps for different money parameters")


# geom_bar()# Plot strategy counts at end of run
df100x5 %>% 
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
  group_by(step, strategy, bc_ratio, moneyness) %>%
  summarise(
    mean_survivors = mean(survivors),
    sd_survivors = sd(survivors)
  ) %>% 
  filter(moneyness %in% c(-10, 0, 1, 10, 999)) %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
  facet_grid(bc_ratio ~ moneyness, labeller = label_both) +
  ggtitle("Evolution of surviving strategies in time for different b/c ratios and initial 'money gap'")

# moneyness means initial """liquidity""" but hold a bit  
# MONEY-ONE

df100x5 %>% 
  filter(step %in% c(1000, 5000, 10000)) %>% 
  filter(moneyness == 1) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate,)) + 
  geom_boxplot() + 
  facet_grid(~step, labeller = label_both) +
  ggtitle("Cooperation rates for different benefit/cost ratios at selected time steps for liquidity = 1 (baseline value)")

df100x5 %>%
  mutate(step = as.factor(step)) %>% 
  filter(step %in% c(1000, 5000, 10000)) %>% 
  filter(moneyness == 1) %>% 
  #group_by(initial_money, debt_threshold) %>% 
  ggplot(aes(x=cooperation_rate, fill = step)) +
  geom_histogram(position = "identity", alpha = 0.3) +
  facet_wrap( ~ bc_ratio, labeller = label_both) +
  ggtitle("Distribution of cooperation rates at selected time steps for liquidity = 1 (baseline value)")


df100x5 %>% 
  filter(step < 10000) %>% 
  filter(moneyness == 1) %>% 
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
  group_by(step, strategy, bc_ratio, moneyness) %>%
  summarise(
    mean_survivors = mean(survivors),
    sd_survivors = sd(survivors)
  ) %>% 
  filter(moneyness %in% c(-10, 0, 1, 10, 999)) %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
  facet_wrap(~ bc_ratio, labeller = label_both) +
  ggtitle("Evolution of surviving strategies in time for liquidity = 1")





########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
# PREVIOUS CODE WITH SEPARATE 200-300 repetitions
# 
# # Summarize cooperation rate data at end of run seeIM
# df100x5A %>% 
#   filter(step == 10000) %>% 
#   group_by(bc_ratio, initial_money, debt_threshold) %>% 
#   summarise(mean(cooperation_rate), sd(cooperation_rate), median(cooperation_rate))
# 
# # Plot cooperation rates x benefit/cost ratios at selected time steps
# df100x5A %>% 
#   filter(step %in% c(5000)) %>% 
#   ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
#   geom_boxplot() + 
#   facet_grid(initial_money ~ debt_threshold, labeller = label_both) +
#   ggtitle("Cooperation rates for different benefit/cost ratios at 5000 time steps")
# 
#   
# # label_both()# Plot distribution of cooperation rates at selected time steps and benefit/cost ratios
# df100x5A %>%
#   mutate(step = as.factor(step)) %>% 
#   filter(step %in% c(5000), bc_ratio != 1) %>% 
#   group_by(initial_money, debt_threshold) %>% 
#   ggplot(aes(x=cooperation_rate, fill = bc_ratio)) +
#   geom_histogram(position = "identity", alpha = 0.3) +
#   facet_grid(initial_money ~ debt_threshold, labeller = label_both) +
#   ggtitle("Distribution of cooperation rates counts at 5000 steps for different money parameters")
# 
# # geom_bar()# Plot strategy counts at end of run
# summary_df100x5A <- df100x5A %>% 
#   filter(step < 10000) %>% 
#   pivot_longer(starts_with("count"), names_to = "strategy", values_to = "survivors") %>% 
#   mutate(
#     strategy = case_when(
#       strategy == "count_cooperators" ~ "cooperators",
#       strategy == "count_defectors" ~ "defectors",
#       strategy == "count_directs" ~ "direct reciprocators",
#       strategy == "count_indirects" ~ "indirect reciprocators",
#       strategy == "count_moneys" ~ "money users",
#       )
#   ) %>% 
#   group_by(step, strategy, bc_ratio, moneyness) %>%
#   summarise(
#     mean_survivors = mean(survivors),
#     sd_survivors = sd(survivors)
#     ) 
#   
# summary_df100x5A %>% 
#   filter(moneyness %in% c(-10, 0, 1, 10, 999)) %>% 
#   ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
#   facet_grid(bc_ratio ~ moneyness, labeller = label_both) +
#   ggtitle("Evolution of surviving strategies in time for different b/c ratios and initial 'money gap'")
# 
# 
# # moneyness means initial """liquidity""" but hold a bit  

# 
# 
# # Summarize cooperation rate data at end of run seeIM
# df100x5B %>% 
#   filter(step == 10000) %>% 
#   group_by(bc_ratio, initial_money, debt_threshold) %>% 
#   summarise(mean(cooperation_rate), sd(cooperation_rate), median(cooperation_rate))
# 
# # Plot cooperation rates x benefit/cost ratios at selected time steps
# df100x5B %>% 
#   filter(step %in% c(5000)) %>% 
#   ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
#   geom_boxplot() + 
#   facet_grid(initial_money ~ debt_threshold, labeller = label_both) +
#   ggtitle("Cooperation rates for different benefit/cost ratios at 5000 time steps")
# 
# 
# # label_both()# Plot distribution of cooperation rates at selected time steps and benefit/cost ratios
# df100x5B %>%
#   mutate(step = as.factor(step)) %>% 
#   filter(step %in% c(5000), bc_ratio != 1) %>% 
#   group_by(initial_money, debt_threshold) %>% 
#   ggplot(aes(x=cooperation_rate, fill = bc_ratio)) +
#   geom_histogram(position = "identity", alpha = 0.3) +
#   facet_grid(initial_money ~ debt_threshold, labeller = label_both) +
#   ggtitle("Distribution of cooperation rates counts at 5000 steps for different money parameters")


# ?? Plot strategy counts at end of run
#df %>% filter(step == 10000) %>% 
#$  pivot_longer(starts_with("count"), names_to = "strategy", values_to = "survivors") %>% 
#  ggplot(aes(x=strategy, y=survivors)) +
# geom_boxplot() +
#facet_wrap(~ bc_ratio, labeller = label_both)
# 
# # geom_bar()# Plot strategy counts at end of run
# summary_df100x5B <- df100x5B %>% 
#   filter(step < 10000) %>% 
#   pivot_longer(starts_with("count"), names_to = "strategy", values_to = "survivors") %>% 
#   mutate(
#     strategy = case_when(
#       strategy == "count_cooperators" ~ "cooperators",
#       strategy == "count_defectors" ~ "defectors",
#       strategy == "count_directs" ~ "direct reciprocators",
#       strategy == "count_indirects" ~ "indirect reciprocators",
#       strategy == "count_moneys" ~ "money users",
#     )
#   ) %>% 
#   group_by(step, strategy, bc_ratio, moneyness) %>%
#   summarise(
#     mean_survivors = mean(survivors),
#     sd_survivors = sd(survivors)
#   ) 
# 
# summary_df100x5B %>% 
#   filter(moneyness %in% c(-10, 0, 1, 10, 999)) %>% 
#   ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
#   facet_grid(bc_ratio ~ moneyness, labeller = label_both) +
#   ggtitle("Evolution of surviving strategies in time for different b/c ratios and initial 'money gap'")
