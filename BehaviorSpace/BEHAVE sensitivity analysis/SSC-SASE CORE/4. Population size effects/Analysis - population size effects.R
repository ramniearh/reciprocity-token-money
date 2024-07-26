##### R script: Money and Generalized Cooperation #####
## Main file. Evolution and cooperation rate charts
## Simulated data for money and no-money scenarios, with  variation of liquidity and benefit/cost parameters
## 2 extra population size scenarios: 100x5 (125x4) and 300x5 (375x4). Benchmark + money.
## 100 repetitions, 10k steps
## PENDING: large population, with money, 90 repetitions left (IN PROGRESS BS)

# 1. Import libraries and packages -------------------------------------------
library(here)
library(tidyverse)
library(janitor)
here()



# 2. Import and process data -------------------------------------------------

# 2.1 Simulation data with with money:

df_money_small <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "4. Population size effects", "money-reciprocity 3.0 - BehaviorSpace FINAL-SM - POPULATION-SMALL - 20x5 10000s 100r.csv") %>% 
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>% 
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with(("x_sum"))) %>%
  select(-starts_with("fitness")) %>%
  rename(
    step = x_step, 
    run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    liquidity = initial_money 
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio),
  ) %>% 
  mutate(
    share_cooperators = count_cooperators / ( count_cooperators + count_defectors + count_directs + count_indirects + count_moneys ),
    share_defectors = count_defectors / ( count_cooperators + count_defectors + count_directs + count_indirects + count_moneys ),
    share_directs = count_directs / ( count_cooperators + count_defectors + count_directs + count_indirects + count_moneys ),
    share_indirects = count_indirects / ( count_cooperators + count_defectors + count_directs + count_indirects + count_moneys ),
    share_moneys = count_moneys / ( count_cooperators + count_defectors + count_directs + count_indirects + count_moneys )
  ) %>% 
  select(-starts_with("count_")) %>% 
  pivot_longer(cols = starts_with("share_"), names_to = "strategy", values_to = "survivor_count") %>% 
  mutate(strategy =
           case_when(
             strategy == "share_cooperators" ~ "cooperators",
             strategy == "share_defectors" ~ "defectors",
             strategy == "share_directs" ~ "direct-reciprocators",
             strategy == "share_indirects" ~ "indirect-reciprocators",
             strategy == "share_moneys" ~ "money-users",
           )
  )

df_money_large <- ##TEMPORARY/POC - PENDING 90 REPETITIONS
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "4. Population size effects", "money-reciprocity 3.0 - BehaviorSpace FINAL-SM - POPULATION-LARGE - 300x5 10ks 10r (fast).csv") %>% 
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>% 
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with(("x_sum"))) %>%
  select(-starts_with("fitness")) %>%
  rename(
    step = x_step, 
    run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    liquidity = initial_money 
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio),
  ) %>% 
  mutate(
    share_cooperators = count_cooperators / ( count_cooperators + count_defectors + count_directs + count_indirects + count_moneys ),
    share_defectors = count_defectors / ( count_cooperators + count_defectors + count_directs + count_indirects + count_moneys ),
    share_directs = count_directs / ( count_cooperators + count_defectors + count_directs + count_indirects + count_moneys ),
    share_indirects = count_indirects / ( count_cooperators + count_defectors + count_directs + count_indirects + count_moneys ),
    share_moneys = count_moneys / ( count_cooperators + count_defectors + count_directs + count_indirects + count_moneys )
  ) %>% 
  select(-starts_with("count_")) %>% 
  pivot_longer(cols = starts_with("share_"), names_to = "strategy", values_to = "survivor_count") %>% 
  mutate(strategy =
           case_when(
             strategy == "share_cooperators" ~ "cooperators",
             strategy == "share_defectors" ~ "defectors",
             strategy == "share_directs" ~ "direct-reciprocators",
             strategy == "share_indirects" ~ "indirect-reciprocators",
             strategy == "share_moneys" ~ "money-users",
           )
  )



# 2.2 Simulation data with without money:
df_no_money_small <-
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "4. Population size effects", "money-reciprocity 3.0 - BehaviorSpace FINAL-SM - POPULATION-SMALL - 25x4 10000s 100r NOMONEY.csv") %>% 
  read.csv(skip = 6) %>%
  clean_names() %>%
  as_tibble()  %>%
  select(-count_moneys) %>% 
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with(("x_sum"))) %>%
  select(-starts_with("fitness")) %>% 
  rename(
    step = x_step,
    run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    liq = initial_money
  ) %>%
  mutate(
    bc_ratio = as.factor(bc_ratio)
  ) %>%
  mutate(
    share_cooperators = count_cooperators / ( count_cooperators + count_defectors + count_directs + count_indirects ),
    share_defectors = count_defectors / ( count_cooperators + count_defectors + count_directs + count_indirects ),
    share_directs = count_directs / ( count_cooperators + count_defectors + count_directs + count_indirects ),
    share_indirects = count_indirects / ( count_cooperators + count_defectors + count_directs + count_indirects )
  ) %>%
  select(-starts_with("count_")) %>%
  pivot_longer(cols = starts_with("share_"), names_to = "strategy", values_to = "survivor_count") %>%
  mutate(strategy =
           case_when(
             strategy == "share_cooperators" ~ "cooperators",
             strategy == "share_defectors" ~ "defectors",
             strategy == "share_directs" ~ "direct-reciprocators",
             strategy == "share_indirects" ~ "indirect-reciprocators",
           )
  )

df_no_money_large <-
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "4. Population size effects", "money-reciprocity 3.0 - BehaviorSpace FINAL-SM - POPULATION-LARGE - 375x4 10000s 100r NOMONEY.csv") %>% 
  read.csv(skip = 6) %>%
  clean_names() %>%
  as_tibble()  %>%
  select(-count_moneys) %>% 
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with(("x_sum"))) %>%
  select(-starts_with("fitness")) %>% 
  rename(
    step = x_step,
    run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    liq = initial_money
  ) %>%
  mutate(
    bc_ratio = as.factor(bc_ratio)
  ) %>%
  mutate(
    share_cooperators = count_cooperators / ( count_cooperators + count_defectors + count_directs + count_indirects ),
    share_defectors = count_defectors / ( count_cooperators + count_defectors + count_directs + count_indirects ),
    share_directs = count_directs / ( count_cooperators + count_defectors + count_directs + count_indirects ),
    share_indirects = count_indirects / ( count_cooperators + count_defectors + count_directs + count_indirects )
  ) %>%
  select(-starts_with("count_")) %>%
  pivot_longer(cols = starts_with("share_"), names_to = "strategy", values_to = "survivor_count") %>%
  mutate(strategy =
           case_when(
             strategy == "share_cooperators" ~ "cooperators",
             strategy == "share_defectors" ~ "defectors",
             strategy == "share_directs" ~ "direct-reciprocators",
             strategy == "share_indirects" ~ "indirect-reciprocators",
           )
  )

# 3. Draw main evolution plots --------------------------------------------


# 3.1 Plot cooperation rates and share of surviving strategies in time (with money, selected parameter values)

p_money_small = df_money_small %>% 
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
  stat_summary(
    aes(y=survivor_count, color = strategy), 
    fun.data = "median_hilow", 
    geom = "line",
    linewidth = 0.75
  ) +
  stat_summary(
    aes(y=survivor_count, fill = strategy), 
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
    y = "Proportion (0-1)",
    color = "Share of survivors by strategy",
    fill = "Share of survivors by strategy",
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    #title = "With money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; total population = 500)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_money_large = df_money_large %>% 
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
  stat_summary(
    aes(y=survivor_count, color = strategy), 
    fun.data = "median_hilow", 
    geom = "line",
    linewidth = 0.75
  ) +
  stat_summary(
    aes(y=survivor_count, fill = strategy), 
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
    y = "Proportion (0-1)",
    color = "Share of survivors by strategy",
    fill = "Share of survivors by strategy",
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    #title = "With money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; total population = 500)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_money_small
p_money_large


# 3.2 Plot cooperation rates and share of surviving strategies in time (without money, selected parameter values) 

p_no_money_small = df_no_money_small %>%
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
  stat_summary(
    aes(y=survivor_count, color = strategy),
    fun.data = "median_hilow",
    geom = "line",
    linewidth = 0.75
  ) +
  stat_summary(
    aes(y=survivor_count, fill = strategy),
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
    y = "Proportion (0-1)",
    color = "Share of survivors by strategy",
    fill = "Share of survivors by strategy",
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    #title = "Without money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; population = 500)"
  ) +
  facet_grid( ~ bc_ratio, labeller = label_both) +
  theme_minimal()


p_no_money_large = df_no_money_large %>%
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
  stat_summary(
    aes(y=survivor_count, color = strategy),
    fun.data = "median_hilow",
    geom = "line",
    linewidth = 0.75
  ) +
  stat_summary(
    aes(y=survivor_count, fill = strategy),
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
    y = "Proportion (0-1)",
    color = "Share of survivors by strategy",
    fill = "Share of survivors by strategy",
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    #title = "Without money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; population = 500)"
  ) +
  facet_grid( ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_no_money_large

# 5. Generate visualizations -------------------------------------------------
p_money_small
p_no_money_large
p_money_large
p_no_money_large