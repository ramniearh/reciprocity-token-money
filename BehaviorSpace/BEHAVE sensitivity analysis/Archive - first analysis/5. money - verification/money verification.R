library(here)
library(tidyverse)
library(janitor)

# Import ant treat data:


## verification test:
## evol_upd T/F
## COOPS AND/OR DEFECTORS
## QUIDPROQUO TRUE/FALSE
## ALSO MONEY-ON-MONEY ONLY! (?)


df_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "5. money - verification", "money-reciprocity 3.0 - BehaviorSpace money verification-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>% 
  filter(x_step %% 1000 == 0) %>%  ###
  #select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with("n_"), -starts_with(("x_sum"))) %>%
  #select(-starts_with("fitness")) %>% 
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    average_memory_length = x_sum_length_memory_of_turtles_count_turtles,
  ) %>% 
  select(-count_directs, -count_indirects) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio),
    evolutionary_updating = as.logical(evolutionary_updating),
    quid_pro_quo = as.logical(quid_pro_quo)
    ) %>% 
  select(
    -fitness_directs_this_round,
    -fitness_indirects_this_round,
    -sum_score_of_directs,
    -sum_score_of_indirects,
    -sum_balance_of_directs,
    -sum_balance_of_indirects
  )

df_money_evol <- df_money %>% 
  filter(evolutionary_updating == T) 

df_money_no_evol <- df_money %>% 
  filter(evolutionary_updating == F) %>% 
  select(-starts_with("count"))

# Population size reality check:
df_money_no_evol %>% filter(step == 0) %>% 
  filter(
    n_coop == 0,
    n_defect == 0,
    n_money == 100
    ) %>% 
  filter(
    initial_money == 1,
    quid_pro_quo == F
  ) %>% 
  filter(bc_ratio == 1) %>% 
  structure()

# Evaluate effect of quid-pro-quo and blind interdependence (populations with coop and def)
# Evaluate first and second-order defection
# coop-only
# defect-only
# both
df_money_evol %>% 
  filter(step %in% c(10000)) %>% 
  filter(n_coop == 100 & n_defect == 100) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) +
  geom_boxplot() + 
  facet_grid(initial_money ~ quid_pro_quo, labeller = label_both)+
  ggtitle("Cooperation rates for different benefit-to-cost ratios (boxplot distribution across 100 repetitions. At 10000 steps)")

df_money_evol %>% 
  filter(n_coop == 100 & n_defect == 100) %>% 
  filter(bc_ratio == 5) %>% 
  pivot_longer(cols = starts_with("count_"), names_to = "strategy", values_to = "survivor_count") %>% 
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
  facet_grid(initial_money ~ quid_pro_quo, labeller = label_both) +
  ggtitle("Evolution of surviving strategies (mean and 2x standard deviations across 100 repetitions). Benefit/cost ratio = 5.")

## Review no-evolutionary-updating mechanics

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
