library(here)
library(tidyverse)
library(janitor)

# Import ant treat data:

df125x4 <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "125x5 - 500 repetitions", "money-reciprocity 3.0 - BEHAVE BehaviorSpace reference experiment - sensitivity analysis full sweep 125x4 no-money-table.csv") %>%
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
    -starts_with("x_sum_"),
    -forgiveness,
    -initial_money,
    -debt_threshold
    ) %>%
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio
    #average_memory_length = x_sum_length_memory_of_turtles_count_turtles,
    #SCORES 
    #BALANCES 
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio),
    #step = as.factor(step)
  )


# Summarize cooperation rate data at end of run seeIM
df125x4 %>% 
  filter(step == 10000) %>% 
  group_by(bc_ratio) %>% 
  summarise(mean(cooperation_rate), sd(cooperation_rate), median(cooperation_rate))

# Plot cooperation rates x benefit/cost ratios at selected time steps
df125x4 %>% 
  filter(step %in% c(5000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
  geom_boxplot() + 
  ggtitle("Reciprocity strategies without money: cooperation rates for different benefit/cost ratios at 5000 time steps")
  
df125x4 %>% 
  filter(step %in% c(1000, 5000, 10000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate,)) + 
  geom_boxplot() + 
  facet_grid(~step, labeller = label_both) +
  ggtitle("Reciprocity strategies without money: cooperation rates for different benefit/cost ratios at selected time steps for liquidity = 1 (baseline value)")
  
# label_both()# Plot distribution of cooperation rates at selected time steps and benefit/cost ratios
df125x4 %>%
  mutate(step = as.factor(step)) %>% 
  filter(step %in% c(5000), bc_ratio != 1) %>% 
  ggplot(aes(x=cooperation_rate, fill = bc_ratio)) +
  geom_histogram(position = "identity", alpha = 0.3) +
  ggtitle("Reciprocity strategies without money: distribution of cooperation rates at 5000 steps")

# 
df125x4 %>%
  mutate(step = as.factor(step)) %>% 
  filter(step %in% c(1000, 5000, 10000)) %>% 
  ggplot(aes(x=cooperation_rate, fill = step)) +
  geom_histogram(position = "identity", alpha = 0.3) +
  facet_wrap( ~ bc_ratio, labeller = label_both) +
  ggtitle("Reciprocity strategies without money: distribution of cooperation rates at selected time steps")



# ?? Plot strategy counts at end of run
#df %>% filter(step == 10000) %>% 
#$  pivot_longer(starts_with("count"), names_to = "strategy", values_to = "survivors") %>% 
#  ggplot(aes(x=strategy, y=survivors)) +
 # geom_boxplot() +
  #facet_wrap(~ bc_ratio, labeller = label_both)
  
# geom_bar()# Plot strategy counts at end of run
df125x4 %>% 
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
  group_by(step, strategy, bc_ratio) %>%
  summarise(
    mean_survivors = mean(survivors),
    sd_survivors = sd(survivors)
    ) %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
  facet_wrap(~ bc_ratio, labeller = label_both) +
  ggtitle("Reciprocity strategies: evolution of surviving strategies in time")


# moneyness means initial """liquidity""" but hold a bit  
  

##### test zone

df %>% 
  filter(step < 1000) %>% 
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
  ggplot(aes(x=step, y=survivors, color = strategy)) +
  stat_summary(fun.data = "mean", geom = "pointrange") +
  stat_summary(fun.data = "mean", geom = "line")
