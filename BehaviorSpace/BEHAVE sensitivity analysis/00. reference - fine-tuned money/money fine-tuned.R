library(here)
library(tidyverse)
library(janitor)
here()

# REVIEW ROLE OF debt_threshold

#################### NEW, INTEGRATED PLOTS

# Import ant treat data - with money:

df_fine_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "00. reference - fine-tuned money", "money-reciprocity 3.0 - MAIN fine-tuned 100x5 10k-steps 50r.csv") %>% 
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


## Plot cooperation rates and share of surviving strategies in time (integrated - final version)?

p_complete <- df_fine_money %>% 
  filter(debt_threshold == 0) %>%  ######
  filter(step %% 500 == 0) %>% 
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
  labs(
    y = "Proportion (0-1)",
    color = "Strategy share",
    fill = "Strategy share",
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    title = "Evolution of surviving strategies and cooperation rates (Median and IQR over 50 repetitions; population = 500)**"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()


p_complete

### EXTENDED ABSTRACT PLOTS BELOW:

p_money = df_fine_money %>% 
  filter(debt_threshold == 0) %>%  ######
  filter(step %% 500 == 0) %>% 
  filter(bc_ratio %in% c(1, 2, 5, 20)) %>%
  filter(liquidity %in% c(0, 0.5, 1, 5, 1000)) %>% 
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
  labs(
    y = "Proportion (0-1)",
    color = "Share of survivors by strategy",
    fill = "Share of survivors by strategy",
    shape = "Cooperation rate on round",
    linetype = "Cooperation rate on round",
    title = "With money: evolution of surviving strategies and cooperation rates (Median and IQR over 50 repetitions; population = 500)**"
  ) +
  facet_grid(liquidity ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_money

# Import ant treat data - with money:

df_fine_no_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "00. reference - fine-tuned money", "money-reciprocity 3.0 - BehaviorSpace reference - fine-tuned without money 125x4 10k-steps 50r.csv") %>% 
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
    liq = initial_money
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

# Plot

p_no_money = df_fine_no_money %>% 
  filter(debt_threshold == 0) %>%  ######
  filter(step %% 500 == 0) %>% 
  filter(bc_ratio %in% c(1, 2, 5, 20)) %>%
  #filter(liquidity %in% c(0, 0.5, 1, 5, 1000)) %>% 
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
  labs(
    y = "Proportion (0-1)",
    color = "Share of survivors by strategy",
    fill = "Share of survivors by strategy",
    shape = "Cooperation rate on round",
    linetype = "Cooperation rate on round",
    title = "Without money: evolution of surviving strategies and cooperation rates (Median and IQR over 50 repetitions; population = 500)**"
  ) +
  facet_grid( ~ bc_ratio, labeller = label_both) +
  theme_minimal()

p_no_money


## analyse evolution of cooperation rate and money prevalence at end of run, in relation to liquidity
## bc=10

p_liquidity_all <- df_fine_money %>% 
  filter(debt_threshold == 0) %>%  ######
  filter(step == 10000) %>% 
  #filter(bc_ratio == 2) %>% 
  mutate(liquidity = as.factor(liquidity)) %>% 
  ggplot(aes(x=liquidity)) +
  geom_boxplot(aes(y=cooperation_rate)) +
  geom_boxplot(aes(y=survivor_count, color = strategy)) +
  facet_grid(bc_ratio~step) +
  ggtitle("Cooperation rates (and share of survivinb strategies) at 10000 steps (50 repetitions,bc_ratio = ?)") +
  theme_minimal()

p_liquidity_all


p_final_cooperation_tile <- df_fine_money %>% 
  filter(debt_threshold == 0) %>%  ######
  filter(step == 10000) %>% 
  mutate(liquidity = as.factor(liquidity)) %>% 
  group_by(liquidity, bc_ratio) %>% #view()
  summarise(mean(cooperation_rate), sd(cooperation_rate)) %>% #view()
  rename(mean_final_cooperation_rate = "mean(cooperation_rate)") %>% 
  ggplot(aes(x=liquidity, y=bc_ratio)) +
  geom_tile(aes(fill=mean_final_cooperation_rate)) +
  ggtitle("Mean? Cooperation rate at 10000 steps (50 repetitions)") +
  theme_minimal()

p_final_money_share_tile <- df_fine_money %>% 
  filter(debt_threshold == 0) %>%  ######
  filter(step == 10000) %>% 
  mutate(liquidity = as.factor(liquidity)) %>% 
  filter(strategy == "money-users") %>% 
  group_by(liquidity, bc_ratio) %>% #view()
  summarise(mean(survivor_count)) %>% #view()
  rename(mean_final_money_prevalence = "mean(survivor_count)") %>% 
  ggplot(aes(x=liquidity, y=bc_ratio)) +
  geom_tile(aes(fill=mean_final_money_prevalence)) +
  ggtitle("Mean? MoneyPrev  at 10000 steps (50 repetitions)") +
  theme_minimal()

p_final_cooperation_tile
p_final_money_share_tile

?geom_tile
#################### OLD#################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### #################### 
#################### #################### #################### #################### 
#################### OLD#################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### #################### 
#################### #################### #################### #################### 
#################### OLD#################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### #################### 
#################### #################### #################### #################### 
#################### OLD#################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### #################### 
#################### #################### #################### #################### 
#################### OLD#################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### ####################
#################### #################### #################### #################### 
#################### #################### #################### #################### 
library(here)
library(tidyverse)
library(janitor)
here()

# Import ant treat data - no money:

df_fine_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "00. reference - fine-tuned money", "money-reciprocity 3.0 - MAIN fine-tuned 100x5 20k-steps 50r.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>% 
  #filter(x_step %% 1000 == 0) %>%  ###
  #select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with("n_"), -starts_with(("x_sum"))) %>%
  #select(-starts_with("fitness")) %>% 
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    average_memory_length = x_sum_length_memory_of_turtles_count_turtles,
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio)
  ) %>% 
  mutate(
    share_cooperators = count_cooperators / 500,
    share_defectors = count_defectors / 500,
    share_directs = count_directs / 500,
    share_indirects = count_indirects / 500,
    share_moneys = count_moneys / 500
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
           ))


## Plot cooperation rates and share of surviving strategies in time (integrated - final version)?

df_fine_money %>% 
  filter(bc_ratio %in% c(1.1, 2, 5, 100) ) %>%  
  filter(initial_money %in% c(0, 0.5, 5, 10000)) %>% 
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
    title = ""
  ) +
  facet_grid(initial_money ~ bc_ratio, labeller = label_both) +
  theme_minimal()












####################
  
  
# Import ant treat data:


df_fine_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "00. reference - fine-tuned money", "money-reciprocity 3.0 - MAIN fine-tuned 100x5 20k-steps 50r.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>% 
  #filter(x_step %% 1000 == 0) %>%  ###
  #select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with("n_"), -starts_with(("x_sum"))) %>%
  #select(-starts_with("fitness")) %>% 
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    average_memory_length = x_sum_length_memory_of_turtles_count_turtles,
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio)
    ) %>% 
  mutate(
    share_cooperators = count_cooperators / 500,
    share_defectors = count_defectors / 500,
    share_directs = count_directs / 500,
    share_indirects = count_indirects / 500,
    share_moneys = count_moneys / 500
  )


## first-version mashup plots

df_fine_money %>% 
  filter(debt_threshold == 0) %>% 
  #filter(step %% 1000 == 0) %>% 
  filter(bc_ratio %in% c(1.1, 5) ) %>%  
  filter(initial_money %in% c(0, 10)) %>% 
  pivot_longer(cols = starts_with("share_"), names_to = "strategy", values_to = "survivor_count") %>% 
  ggplot(aes(x=step, y=survivor_count, color = strategy, fill = strategy)) +
  ylim(0,1) +
  stat_summary(
    aes(y = cooperation_rate),
    fun.data = "mean_se",
    geom = "errorbar",
    color="black"
  ) +
  stat_summary(
    fun.data = "mean_se", 
    geom = "line",
    linewidth = 0.75
  ) +
  stat_summary(
    fun.data = "mean_se", 
    geom = "ribbon", 
    alpha = 0.2
  ) +
  facet_grid(initial_money ~ bc_ratio, labeller = label_both) +
  theme_minimal() +
  ggtitle("Evolution of surviving strategies and cooperation rates (50 repetitions, population = 500)*")

?geom_boxplot

stat_boxplot(
  aes(y = cooperation_rate),
  #fun.data = "mean_se",
  geom = "boxplot",
  fill = "black",
  size = 0.1,
  alpha = 0.1
)
?mean_se

# debt threshold x money validation:
df_fine_money %>% 
  filter(debt_threshold == 0) %>%
  filter(step %in% c(1000, 4000, 7000, 10000)) %>% 
  filter(bc_ratio %in% c(1, 1.1, 1.5, 2, 5, 20, 50, 100) ) %>%  
  filter(initial_money %in% c(0, 10, 100, 1000, 10000)) %>% 
  ggplot(aes(group=step, x=step, y=cooperation_rate)) +
  ylim(0,1) +
  geom_boxplot() + 
  facet_grid(initial_money ~ bc_ratio, labeller = label_both)+
  ggtitle("Cooperation rates for different benefit-to-cost ratios (boxplot distribution, 50 repetitions, population = 500)")





############## #TODO
## Review no-evolutionary-updating mechanics

# broader views

df_fine_money %>% 
  filter(debt_threshold == 0) %>% 
  filter(step %in% c(5000)) %>% 
  filter(bc_ratio %in% c(1.1, 2, 3, 10, 50)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) +
  geom_boxplot() + 
  facet_grid(~initial_money, labeller = label_both)+
  ggtitle("Cooperation rates at 5000 steps for different benefit-to-cost ratios (boxplot distribution across 100* repetitions)")


df_fine_money %>% 
  filter(debt_threshold == 0) %>% 
  filter(step %% 1000 == 0) %>% 
  filter(bc_ratio %in% c(50)) %>% 
  pivot_longer(cols = starts_with("count_"), names_to = "strategy", values_to = "survivor_count") %>% 
  ggplot(aes(x=step, y=survivor_count, color = strategy, fill = strategy)) +
  stat_summary(
    fun.data = "mean_sdl", 
    geom = "line",
    linewidth = 0.75
  ) +
  stat_summary(
    fun.data = "mean_sdl", 
    geom = "ribbon", 
    alpha = 0.2
  ) +
  facet_grid(~initial_money, labeller = label_both) +
  ggtitle("Evolution of surviving strategies for benefit/cost ratio of 50 (mean and 2x standard deviations across 100* repetitions).")









######### NO-MONEY -- AND TESTING REORGANIZED CHARTS WITH 0-to-1
df_fine_no_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "00. reference - fine-tuned money", "money-reciprocity 3.0 - BehaviorSpace reference - fine-tuned without money 125x4 20k-steps 50r.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>% 
  #filter(x_step %% 1000 == 0) %>%  ###
  #select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with("n_"), -starts_with(("x_sum"))) %>%
  #select(-starts_with("fitness")) %>% 
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    average_memory_length = x_sum_length_memory_of_turtles_count_turtles,
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio)
  ) %>% 
  mutate(
    share_cooperators = count_cooperators / 500,
    share_defectors = count_defectors / 500,
    share_directs = count_directs / 500,
    share_indirects = count_indirects / 500,
    share_moneys = count_moneys / 500
  )



df_fine_no_money %>% 
  #filter(debt_threshold == 0) %>% 
  filter(bc_ratio %in% c(1.1, 3, 10, 100)) %>% 
  filter(step %in% c(0, 2500, 5000, 7500, 10000)) %>% 
  ggplot(aes(group = step, x=step, y=cooperation_rate)) +
  geom_boxplot() + 
  ylim(0,1) +
  facet_grid(~bc_ratio, labeller = label_both)+
  ggtitle("Cooperation rates at selected time steps (no money, 50 repetitions)")


df_fine_no_money %>% 
  #filter(debt_threshold == 0) %>% 
  filter(bc_ratio %in% c(1.1, 3, 10, 100)) %>% 
  pivot_longer(cols = starts_with("share_"), names_to = "strategy", values_to = "survivor_count") %>% 
  ggplot(aes(x=step, y=survivor_count, color = strategy, fill = strategy)) +
  stat_summary(
    fun.data = "mean_sdl", 
    geom = "line"
  ) +
  stat_summary(
    fun.data = "mean_sdl", 
    geom = "ribbon", 
    alpha = 0.2
  ) +
  facet_grid(~bc_ratio, labeller = label_both) +
  ggtitle("Evolution of surviving strategies (mean and 2x standard deviations, no money, 50 repetitions).")



##REVIEW DEBT THRESHOLD -10











# cooperation
df_money_no_evol %>% 
  filter(step %in% c(10000)) %>% 
  filter(n_coop == 100 & n_defect == 100) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) +
  geom_boxplot() + 
  ylim(0,1) +
  facet_grid(initial_money ~ quid_pro_quo, labeller = label_both)

# fitness
df_money_no_evol %>% 
  filter(bc_ratio == 5) %>% 
  filter(n_coop == 100 & n_defect == 100) %>% 
  pivot_longer(starts_with("fitness"), names_to = "strategy", values_to = "fitness") %>% 
  ggplot(aes(x=step, y=fitness, color=strategy, fill=strategy)) +
  stat_summary(
    fun.data = "mean_sdl", 
    geom = "line"
  ) +
  stat_summary(
    fun.data = "mean_sdl", 
    geom = "ribbon", 
    alpha = 0.2
  ) +
  facet_grid(initial_money ~ quid_pro_quo, labeller = label_both)

# balances
df_money_no_evol %>% 
  filter(bc_ratio == 5) %>% 
  filter(n_coop == 0 & n_defect == 100) %>% 
  pivot_longer(starts_with("sum_balance"), names_to = "strategy", values_to = "balances") %>% 
  ggplot(aes(x=step, y=balances, color=strategy, fill=strategy)) +
  stat_summary(
    fun.data = "mean_sdl", 
    geom = "line"
  ) +
  stat_summary(
    fun.data = "mean_sdl", 
    geom = "ribbon", 
    alpha = 0.2
  ) +
  facet_grid(initial_money ~ quid_pro_quo, labeller = label_both)


# scores
df_money_no_evol %>% 
  filter(bc_ratio == 5) %>% 
  filter(n_coop == 0 & n_defect == 100) %>% 
  pivot_longer(starts_with("sum_score"), names_to = "strategy", values_to = "scores") %>% 
  ggplot(aes(x=step, y=scores, color=strategy, fill=strategy)) +
  stat_summary(
    fun.data = "mean_sdl", 
    geom = "line"
  ) +
  stat_summary(
    fun.data = "mean_sdl", 
    geom = "ribbon", 
    alpha = 0.2
  ) +
  facet_grid(initial_money ~ quid_pro_quo, labeller = label_both)










############### PERMANENT VERSION (UNDER CONSTRUCTION)
# rate_money_only <- 
#   df_money_evol %>% 
#   filter(step %in% c(10000)) %>% 
#   filter(n_coop == 0 & n_defect == 0) %>% 
#   ggplot(aes(x=bc_ratio, y=cooperation_rate)) +
#   geom_boxplot() + 
#   facet_grid(initial_money ~ quid_pro_quo, labeller = label_both)+
#   ggtitle("Cooperation rates for different benefit-to-cost ratios (boxplot distribution across 100 repetitions. At 10000 steps)")
# 
# count_money_only <- 
#   df_money_evol %>% 
#   filter(n_coop == 0& n_defect == 0) %>% 
#   filter(bc_ratio == 5 ) %>% 
#   pivot_longer(cols = starts_with("count_"), names_to = "strategy", values_to = "survivor_count") %>% 
#   ggplot(aes(x=step, y=survivor_count, color = strategy, fill = strategy)) +
#   stat_summary(
#     fun.data = "mean_sdl", 
#     geom = "line"
#   ) +
#   stat_summary(
#     fun.data = "mean_sdl", 
#     geom = "ribbon", 
#     alpha = 0.2
#   ) +
#   facet_grid(initial_money ~ quid_pro_quo, labeller = label_both) +
#   ggtitle("Evolution of surviving strategies (mean and 2x standard deviations across 100 repetitions). Benefit/cost ratio = 5.")
# 
# rate_coop_def <- df_money_evol %>% 
#   filter(step %in% c(10000)) %>% 
#   filter(n_coop == 100 & n_defect == 100) %>% 
#   ggplot(aes(x=bc_ratio, y=cooperation_rate)) +
#   geom_boxplot() + 
#   facet_grid(initial_money ~ quid_pro_quo, labeller = label_both)+
#   ggtitle("Cooperation rates for different benefit-to-cost ratios (boxplot distribution across 100 repetitions. At 10000 steps)")
# 
# count_coop_dev <- df_money_evol %>% 
#   filter(n_coop == 100 & n_defect == 100) %>% 
#   filter(bc_ratio == 5 ) %>% 
#   pivot_longer(cols = starts_with("count_"), names_to = "strategy", values_to = "survivor_count") %>% 
#   ggplot(aes(x=step, y=survivor_count, color = strategy, fill = strategy)) +
#   stat_summary(
#     fun.data = "mean_sdl", 
#     geom = "line"
#   ) +
#   stat_summary(
#     fun.data = "mean_sdl", 
#     geom = "ribbon", 
#     alpha = 0.2
#   ) +
#   facet_grid(initial_money ~ quid_pro_quo, labeller = label_both) +
#   ggtitle("Evolution of surviving strategies (mean and 2x standard deviations across 100 repetitions). Benefit/cost ratio = 5.")
# 
# rate_coop_no_dev <- 
#   df_money_evol %>% 
#   filter(step %in% c(10000)) %>% 
#   filter(n_coop == 100 & n_defect == 0) %>% 
#   ggplot(aes(x=bc_ratio, y=cooperation_rate)) +
#   geom_boxplot() + 
#   facet_grid(initial_money ~ quid_pro_quo, labeller = label_both)+
#   ggtitle("Cooperation rates for different benefit-to-cost ratios (boxplot distribution across 100 repetitions. At 10000 steps)")
# 
# count_coop_no_dev <- 
#   df_money_evol %>% 
#   filter(n_coop == 100 & n_defect == 0) %>% 
#   filter(bc_ratio == 5 ) %>% 
#   pivot_longer(cols = starts_with("count_"), names_to = "strategy", values_to = "survivor_count") %>% 
#   ggplot(aes(x=step, y=survivor_count, color = strategy, fill = strategy)) +
#   stat_summary(
#     fun.data = "mean_sdl", 
#     geom = "line"
#   ) +
#   stat_summary(
#     fun.data = "mean_sdl", 
#     geom = "ribbon", 
#     alpha = 0.2
#   ) +
#   facet_grid(initial_money ~ quid_pro_quo, labeller = label_both) +
#   ggtitle("Evolution of surviving strategies (mean and 2x standard deviations across 100 repetitions). Benefit/cost ratio = 5.")
# 
# rate_no_coop_dev <- 
#   df_money_evol %>% 
#   filter(step %in% c(10000)) %>% 
#   filter(n_coop == 0 & n_defect == 100) %>% 
#   ggplot(aes(x=bc_ratio, y=cooperation_rate)) +
#   geom_boxplot() + 
#   facet_grid(initial_money ~ quid_pro_quo, labeller = label_both)+
#   ggtitle("Cooperation rates for different benefit-to-cost ratios (boxplot distribution across 100 repetitions. At 10000 steps)")
# 
# count_no_coop_dev <- 
#   df_money_evol %>% 
#   filter(n_coop == 0 & n_defect == 100) %>% 
#   filter(bc_ratio == 5 ) %>% 
#   pivot_longer(cols = starts_with("count_"), names_to = "strategy", values_to = "survivor_count") %>% 
#   ggplot(aes(x=step, y=survivor_count, color = strategy, fill = strategy)) +
#   stat_summary(
#     fun.data = "mean_sdl", 
#     geom = "line"
#   ) +
#   stat_summary(
#     fun.data = "mean_sdl", 
#     geom = "ribbon", 
#     alpha = 0.2
#   ) +
#   facet_grid(initial_money ~ quid_pro_quo, labeller = label_both) +
#   ggtitle("Evolution of surviving strategies (mean and 2x standard deviations across 100 repetitions). Benefit/cost ratio = 5.")
# 
# rate_coop_def
# count_coop_dev
# rate_coop_no_dev
# count_coop_no_dev
# rate_no_coop_dev
# count_no_coop_dev
# rate_money_only
# count_money_only
