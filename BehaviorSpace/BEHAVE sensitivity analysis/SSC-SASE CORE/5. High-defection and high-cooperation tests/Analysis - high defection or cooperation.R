##### R script: Money and Generalized Cooperation #####
## Supplementary Materials file. Evolution and cooperation rate charts.
## Simulated data with variation of initial number of cooperators and defectors (money and no-money scenarios).
## 100 repetitions, 10k steps.
## PENDING: early test data with defection only, 10 repetitions

# 1. Import libraries and packages -------------------------------------------
library(here)
library(tidyverse)
library(janitor)
here()

# 2. Import and process data -------------------------------------------------
# 2.1 Simulation data with money: -----

df_money_high_defection <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "5. High-defection and high-cooperation tests", "money-reciprocity 3.0 - BehaviorSpace FINAL-SM - HIGH-DEFECTION - 100x5 10000s fast.csv") %>% 
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

df_money_high_cooperation <-
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "5. High-defection and high-cooperation tests", "money-reciprocity 3.0 - BehaviorSpace FINAL-SM - HIGH-COOPERATION - 100x5 10000s fast.csv") %>% 
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

df_money_high_CD <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "5. High-defection and high-cooperation tests", "money-reciprocity 3.0 - BehaviorSpace FINAL-SM - HIGH-COOP-DEF - 100x5 10000s 10r fast.csv") %>% 
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


# 2.2 Simulation data without money: -----

df_no_money_high_defection <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "5. High-defection and high-cooperation tests", "money-reciprocity 3.0 - BehaviorSpace FINAL-SM - HIGH-DEFECTION - NOMONEY 10000s 10r.csv") %>% 
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

df_no_money_high_cooperation <-
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "5. High-defection and high-cooperation tests", "money-reciprocity 3.0 - BehaviorSpace FINAL-SM - HIGH-COOPERATION - NOMONEY 10000s 10r.csv") %>% 
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

df_no_money_high_CD <-
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "5. High-defection and high-cooperation tests", "money-reciprocity 3.0 - BehaviorSpace FINAL-SM - HIGH-COOP-DEF - NOMONEY - 10000s.csv") %>% 
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




# 2. Draw main evolution plots --------------------------------------------
# 2.1 Plot cooperation rates and share of surviving strategies in time (with money, selected parameter values)-----

p_money_high_defection = df_money_high_defection %>% 
  filter(n_defect == 400) %>%  #########REDO FOR 100r
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
    title = "High-defection scenario with money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; total population = 800)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_money_high_defection

p_money_high_cooperation = df_money_high_cooperation %>% 
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
    title = "High-cooperation scenario with money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; total population = 800)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_money_high_cooperation

p_money_high_CD = df_money_high_CD %>% 
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
    title = "High-defection and high-cooperation scenario with money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; total population = 1100)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_money_high_CD

# 2.2 Plot cooperation rates and share of surviving strategies in time (no money, selected parameter values) -----

p_no_money_high_defection = df_no_money_high_defection %>% 
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
    title = "High-defection scenario with money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; total population = 800)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_no_money_high_defection

p_no_money_high_cooperation = df_no_money_high_cooperation %>% 
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
    title = "High-cooperation scenario with money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; total population = 800)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_no_money_high_cooperation

p_no_money_high_CD = df_no_money_high_CD %>% 
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
    title = "High-defection and high-cooperation scenario with money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; total population = 1100)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_no_money_high_CD


# 5. Generate visualizations -------------------------------------------------
p_money_high_defection
p_no_money_high_defection
p_money_high_cooperation
p_no_money_high_cooperation
p_money_high_CD
p_no_money_high_CD