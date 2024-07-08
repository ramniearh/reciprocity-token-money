# 1. Import libraries and packages -------------------------------------------
library(here)
library(tidyverse)
library(janitor)
here()

# 2. Import and process data -------------------------------------------------

# 2.1 Simulation data with with money:
df_core_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "money-reciprocity 3.0 - BehaviorSpace reference - fine-liquidity fine-tuned money 100x5 10k-steps 100r nodebt-table.csv") %>% 
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>% 
  #filter(x_step %% 1000 == 0) %>%  ###
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with(("x_sum"))) %>% ##
  select(-starts_with("fitness")) %>%  ##
  rename(
    step = x_step, 
    run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    liquidity = initial_money ####ATTENTION
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio),
    ##liquidity = initial_money - debt_threshold ######ATTENTION
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
df_core_no_money <-
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "money-reciprocity 3.0 - BehaviorSpace reference - fine-liquidity no-money 100x5 10k-steps 100r.csv") %>%
  read.csv(skip = 6) %>%
  clean_names() %>%
  as_tibble()  %>%
  select(-count_moneys) %>% 
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with(("x_sum"))) %>% ##
  select(-starts_with("fitness")) %>%  ##
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
             #strategy == "share_moneys" ~ "money-users",
           )
  )




# 3. Draw main evolution plots --------------------------------------------

# 3.1 Plot cooperation rates and share of surviving strategies in time (with money, selected parameter values)

p_money = df_core_money %>% 
  filter(bc_ratio %in% c(1.5, 2, 5, 10)) %>%
  filter(liquidity %in% c(0.25, 0.5, 1, 5, 100)) %>% 
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
    shape = "Cooperation rate on round",
    linetype = "Cooperation rate on round",
    title = "With money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; population = 500)**"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_money

# 3.2 Plot cooperation rates and share of surviving strategies in time (without money, selected parameter values)

p_no_money = df_core_no_money %>%
  filter(debt_threshold == 0) %>%  ##$S»CHECK
  filter(step %% 500 == 0) %>%
  filter(bc_ratio %in% c(1.5, 2, 5, 10)) %>%
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
    shape = "Cooperation rate on round",
    linetype = "Cooperation rate on round",
    title = "Without money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; population = 500)"
  ) +
  facet_grid( ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_no_money


# 3.3 Plot cooperation rates and share of surviving strategies in time (with money, all simulated parameter values, axis labels removed)
p_complete <- df_core_money %>% 
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
    color = "Strategy share",
    fill = "Strategy share",
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    title = "Evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; population = 500)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_void()

# 3.4 Plot cooperation rates and share of surviving strategies in time (without money, all simulated parameter values, axis labels removed)
p_complete_no <- df_core_no_money %>% 
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
    color = "Strategy share",
    fill = "Strategy share",
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    title = "Evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; population = 500)"
  ) +
  facet_grid( ~ bc_ratio, labeller = label_both) +
  theme_void()

p_complete
p_complete_no

# 4. Draw auxiliary plots -------------------------------------------------

# 4.1 Plotcooperation rates (and strategy money prevalence) at end of run, in relation to liquidity, for a fixed value of benefit/cost ratio (10)

p_liquidity_10 <- df_core_money %>% 
  filter(step == 10000) %>% 
  filter(bc_ratio == 10) %>% 
  mutate(liquidity = as.factor(liquidity)) %>% 
  ggplot(aes(x=liquidity)) +
  geom_boxplot(aes(y=cooperation_rate)) +
  #geom_boxplot(aes(y=survivor_count, color = strategy)) +
  #facet_grid(bc_ratio~step) +
  labs(
    x = "Liquidity",
    y = "Proportion (0-1)",
    #color = "Share of survivors by strategy",
    #fill = "Share of survivors by strategy",
    #shape = "Cooperation rate on round",
    #linetype = "Cooperation rate on round",
    title = "Cooperation rates at end of run (100 repetitions, benefit-to-cost ratio = 10, rates collected at 10000 simulation steps)"
  ) + 
  theme_minimal()


# 5. Generate visualizations -------------------------------------------------
p_money
p_no_money
p_liquidity_10
p_complete  
p_complete_no