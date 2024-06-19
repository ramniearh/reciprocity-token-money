library(here)
library(tidyverse)
library(janitor)
here()

# REVIEW ROLE OF debt_threshold


library(here)
library(tidyverse)
library(janitor)
here()

# Import ant treat data - with money:

df_analytical <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "extra 2. analytical money-cooperation-defection results", "money-reciprocity 3.0 - BehaviorSpace analytical - reference - 10k steps 10r-table.csv") %>% 
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>%
  #select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with(("x_sum"))) %>% ##
  #select(-starts_with("fitness")) %>%  ##
  rename(
    step = x_step, 
    run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    #liquidity = initial_money
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio),
    evolutionary_updating = as.logical(evolutionary_updating)
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

# Summarize fitness per round, without evol upd
df_analytical %>% 
  #filter(step %in% c(1000, 5000, 10000)) %>% 
  filter(bc_ratio != 100) %>% 
  filter(initial_money != 2) %>% 
  filter(initial_money != 0.25) %>% 
  filter(n_coop == 0 & n_defect == 0) %>% 
  #filter(evolutionary_updating == F) %>%  ######
  select(-fitness_directs_this_round, -fitness_indirects_this_round) %>% 
  pivot_longer(starts_with("fitness"), names_to = "strategy_", values_to = "fitness") %>% 
  ggplot(aes(x = bc_ratio, y = fitness, color = strategy_)) +
  geom_boxplot() +
  facet_grid(evolutionary_updating~initial_money, labeller = label_both)
#+
 # facet_wrap(~step)

df_analytical %>% 
  #filter(step %in% c(1000, 5000, 10000)) %>% 
  filter(bc_ratio != 100) %>% 
  filter(initial_money != 2) %>% 
  filter(initial_money != 0.25) %>% 
  #filter(n_coop == 0 & n_defect == 0) %>% 
  #filter(evolutionary_updating == F) %>%  ######
select(-fitness_directs_this_round, -fitness_indirects_this_round) %>% 
  pivot_longer(starts_with("fitness"), names_to = "strategy_", values_to = "fitness") %>% 
  group_by(evolutionary_updating, strategy_, initial_money, bc_ratio, n_coop, n_defect, n_money) %>% 
  summarise(mean(fitness), sd(fitness)) %>% write.csv()
  

?write.csv
# 
# df_analytical %>% 
#   filter(step %in% c(5000)) %>% 
#   filter(bc_ratio != 100) %>% 
#   filter(n_coop == 0 & n_defect == 100) %>% 
#   filter(evolutionary_updating == F) %>%  ######
#   select(-fitness_directs_this_round, -fitness_indirects_this_round) %>% 
#   pivot_longer(starts_with("fitness"), names_to = "strategy_", values_to = "fitness") %>% 
#   group_by(strategy_, bc_ratio, initial_money) %>% 
#   summarise(mean(fitness), sd(fitness)) %>% view()

## Plot cooperation rates and share of surviving strategies in time (integrated - final version)?

p_analytical_money_coop_defect <- df_analytical %>% 
  filter(evolutionary_updating == T) %>%  ######
  filter(step <= 2500) %>% 
  filter(n_coop == 100 & n_defect == 100) %>% 
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
    title = "Evolution of surviving strategies and cooperation rates (Money and defectors)"
  ) +
  facet_grid(liq ~ bc_ratio, labeller = label_both) +
  theme_minimal()


p_analytical_money_coop_defect

p_analytical_money

p_analytical_money_coop

p_analytical_money_defect


#############
# fitness
p_analytical_no_evol <- df_analytical %>% 
  filter(evolutionary_updating == F) %>%  ######
  filter(step == 1000) %>% 
  select(-strategy, -survivor_count) %>% 
  filter(n_coop == 100 & n_defect == 100) %>% 
  pivot_longer(starts_with("fitness"), names_to = "strategy", values_to = "fitness") %>% 
  ggplot(aes(x=step, y=fitness, color=strategy, fill=strategy)) +
  stat_summary(
    fun.data = "median_hilow", 
    geom = "line"
  ) +
  stat_summary(
    fun.data = "median_hilow", 
    geom = "ribbon", 
    alpha = 0.2
  ) +
  facet_grid(liq ~ bc_ratio, labeller = label_both)


p_analytical_no_evol









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








