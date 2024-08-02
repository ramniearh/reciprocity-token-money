##### R script: Money and Generalized Cooperation #####
## Main file. Evolution and cooperation rate charts
## Simulated data for money and no-money scenarios, with  variation of liquidity and benefit/cost parameters


# 1. Import libraries and packages -------------------------------------------
library(here)
library(tidyverse)
library(janitor)

here() # (Validation: this should be the folder containing this R script and the data files for this scenario)


# 2. Import and process data -------------------------------------------------

# 2.1 Simulation data with with money:
df_core_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "0. Reference analysis", "money-reciprocity 3.0 - BehaviorSpace reference - with money.csv") %>% 
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
df_core_no_money <-
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "0. Reference analysis", "money-reciprocity 3.0 - BehaviorSpace reference - without money.csv") %>% 
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
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    #title = "With money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; total population = 500)"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_money


# 3.2 Plot cooperation rates and share of surviving strategies in time (without money, selected parameter values)

p_no_money = df_core_no_money %>%
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
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    #title = "Without money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; population = 500)"
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

p_complete 

# 3.4 Plot cooperation rates and share of surviving strategies in time (without money, all simulated parameter values, axis labels removed)
p_complete_no_money <- df_core_no_money %>% 
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
p_complete_no_money




# 4. Draw end-of-run plots -------------------------------------------------

# 4.1 Plot cooperation rates at end of run, in relation to liquidity, for a fixed value of benefit/cost ratio (5)

p_liquidity_5_end <- df_core_money %>% 
  filter(step == 10000) %>% 
  filter(bc_ratio == 5) %>% 
  mutate(liquidity = as.factor(liquidity)) %>% 
  ggplot(aes(x=liquidity)) +
  geom_boxplot(aes(y=cooperation_rate), fill = "#7153a1") +
  labs(
    x = "Liquidity",
    y = "Cooperation rates"
    #title = "Cooperation rates at 10000 simulation steps (100 repetitions, benefit-to-cost ratio = 5)"
  ) + 
  theme_minimal()

p_liquidity_5

# 4.2 Plot summarized cooperation rates and strategy money prevalence at end of run for all liquidity and benefit-to-cost ratios
p_complete_end <- df_core_money %>% 
  filter(step == 10000) %>% 
  filter(!liquidity %in% c(0, .05, 0.25, 0.75, 3, 20, 10000)) %>% 
  filter(bc_ratio != 1) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio),
    liquidity = as.factor(liquidity)
  ) %>% 
  pivot_wider(names_from = strategy, values_from = survivor_count) %>% 
  mutate(most_prevalent = case_when(
    cooperators > 0.5 ~ "cooperators",
    defectors > 0.5 ~ "defectors",
    `direct-reciprocators` > 0.5 ~ "direct-reciprocators",
    `indirect-reciprocators` > 0.5 ~ "indirect-reciprocators",
    `money-users` > 0.5 ~ "money-users",
    TRUE ~ "none above 50%"
  )) %>% 
  group_by(bc_ratio, liquidity) %>% 
  summarize(mean_cooperation_rate = mean(cooperation_rate), sd_cooperation_rate = sd(cooperation_rate), most_prevalent = names(which.max(table(most_prevalent))) ) %>% 
  ggplot(aes(x=bc_ratio, y = liquidity)) + 
  geom_tile(aes(alpha = mean_cooperation_rate, fill = most_prevalent), lwd = 1, color = "black") +
  coord_fixed() +
  scale_alpha(range = c(0.3, 1)) +
  geom_text(aes(label = sprintf("%.2f", mean_cooperation_rate), alpha = mean_cooperation_rate), nudge_y = 0.05, size = 4) +
  geom_text(aes(label = sprintf("%.2f", sd_cooperation_rate), alpha = mean_cooperation_rate), nudge_y = -0.15, size = 3) +
  scale_fill_manual(
    values = c("defectors"="#ABA300","money-users"="#7153a1","none above 50%" = "darkgrey")) +
  labs(
    x = "Benefit-to-cost ratio",
    y = "Liquidity",
    fill = "Most commonly prevalent strategy",
    alpha = "Mean cooperation rate"
    #title = "With money: cooperation rates and most common winning strategies at end of run (Mean and SD over 100 repetitions; total population = 500)"
  ) +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove the background panel
  )

p_complete_end

#geom_point(aes(color = most_prevalent, shape = most_prevalent), size = 5, position = position_nudge(x = 0.3, y = -0.3))

# geom_tile(aes(
#   fill = most_prevalent,
# ), lwd = 1, color = "black" ) +
#   coord_fixed() +
#   scale_color_manual(
#     values = c("defectors"="#ABA300","money-users"="#C77CFF","none above 50%" = "darkgrey")) +
#   theme(panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank(),  # Remove minor grid lines
#         panel.background = element_blank(),  # Remove the background panel
#   )

#  scale_fill_gradient(low = "#154c79", high = "#791600") +


# 5. Generate visualizations -------------------------------------------------
p_money
p_no_money
p_liquidity_5
p_complete
p_complete_no_money
p_complete_end