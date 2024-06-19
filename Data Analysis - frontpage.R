library(here)
library(tidyverse)
library(janitor)
here()

# Import ant treat data - with money:

df_money_fine <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "12. high-defection tests", "money-reciprocity 3.0 - BehaviorSpace reference - fine-tuned money - high defection 10k steps 25r-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>% 
  #filter(x_step %% 1000 == 0) %>%  ###
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with(("x_sum"))) %>% ##
  select(-starts_with("fitness")) %>%  ##
  rename(
    step = x_step, 
    run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio)
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


## Plot cooperation rates and share of surviving strategies in time (integrated - final version)?

df_high_defect_money %>% 
  filter(n_defect == 1000) %>%  #########*
  filter(step %% 1000 == 0) %>%
  ggplot(aes(x=step)) +
  ylim(0, 1) +
  stat_summary(
    aes(y = cooperation_rate, shape = ""),
    fun.data = "median_hilow",
    geom = "point",
    size = 0.5,
    alpha = 0.8
  ) +
  stat_summary(
    aes(y = cooperation_rate, linetype = ""),
    fun.data = "median_hilow",
    geom = "errorbar",
    alpha = 0.2
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
  labs(
    y = "Proportion (0-1)",
    color = "Strategy share",
    fill = "Strategy share",
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    title = "High-defection: evolution of surviving strategies and cooperation rates (25 repetitions, population = 400 (100 per strategy) + 1000* defectors)"
  ) +
  facet_grid(initial_money ~ bc_ratio, labeller = label_both) +
  theme_minimal()


library(here)
library(tidyverse)
library(janitor)
here()

# Import ant treat data - no money:

df_high_defect_no_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "12. high-defection tests", "money-reciprocity 3.0 - BehaviorSpace reference fine-tuned - without money - high defection 10k steps 25r.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>% 
  #filter(x_step %% 1000 == 0) %>%  ###
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with(("x_sum"))) %>% ##
  select(-starts_with("fitness")) %>%  ##
  rename(
    step = x_step, 
    run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio)
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


## Plot cooperation rates and share of surviving strategies in time (integrated - final version)?

df_high_defect_no_money %>% 
  #filter(n_defect == 1000) %>%  #########*
  filter(step %% 1000 == 0) %>%
  ggplot(aes(x=step)) +
  ylim(0, 1) +
  stat_summary(
    aes(y = cooperation_rate, shape = ""),
    fun.data = "median_hilow",
    geom = "point",
    size = 0.5,
    alpha = 0.8
  ) +
  stat_summary(
    aes(y = cooperation_rate, linetype = ""),
    fun.data = "median_hilow",
    geom = "errorbar",
    alpha = 0.2
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
  labs(
    y = "Proportion (0-1)",
    color = "Strategy share",
    fill = "Strategy share",
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    title = "High-defection without money: evolution of surviving strategies and cooperation rates (25 repetitions, population = 300 (100 per strategy) + varying* defectors)"
  ) +
  facet_grid(n_defect ~ bc_ratio, labeller = label_both) +
  theme_minimal()

