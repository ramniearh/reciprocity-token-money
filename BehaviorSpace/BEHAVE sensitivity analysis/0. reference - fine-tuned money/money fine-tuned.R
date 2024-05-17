library(here)
library(tidyverse)
library(janitor)

# Import ant treat data:


df_fine_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "0. reference - fine-tuned money", "money-reciprocity 3.0 - BehaviorSpace money verification - complete 100x5 20k-steps 100r-table.csv") %>%
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
    ) 

df_fine_money %>% 
  filter(debt_threshold == 0) %>% 
  filter(step %in% c(1000, 5000, 10000, 20000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) +
  geom_boxplot() + 
  facet_grid(initial_money ~ step, labeller = label_both)+
  ggtitle("Cooperation rates for different benefit-to-cost ratios (boxplot distribution across 50 repetitions)")

df_fine_money %>% 
  filter(debt_threshold == 0) %>% 
  filter(step %% 1000 == 0) %>% 
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
  facet_grid(initial_money ~ bc_ratio, labeller = label_both) +
  ggtitle("Evolution of surviving strategies (mean and 2x standard deviations across 50 repetitions).")

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
  ggtitle("Cooperation rates at 5000 steps for different benefit-to-cost ratios (boxplot distribution across 50 repetitions)")


df_fine_money %>% 
  filter(debt_threshold == 0) %>% 
  filter(step %% 1000 == 0) %>% 
  filter(bc_ratio %in% c(50)) %>% 
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
  facet_grid(~initial_money, labeller = label_both) +
  ggtitle("Evolution of surviving strategies for benefit/cost ratio of 50 (mean and 2x standard deviations across 50 repetitions).")









######### NO-MONEY
df_fine_no_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "0. reference - fine-tuned money", "money-reciprocity 3.0 - BehaviorSpace reference - fine-tuned without money 100x5 20k-steps 50r.csv") %>%
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
  ) 



df_fine_no_money %>% 
  #filter(debt_threshold == 0) %>% 
  filter(step %in% c(1000, 5000, 10000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) +
  geom_boxplot() + 
  ylim(0,1) +
  facet_grid(~step, labeller = label_both)+
  ggtitle("Cooperation rates at selected time steps for different benefit-to-cost ratios (no money, 100 repetitions)")


df_fine_no_money %>% 
  #filter(debt_threshold == 0) %>% 
  filter(step %% 1000 == 0) %>% 
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
  facet_grid(~bc_ratio, labeller = label_both) +
  ggtitle("No money: evolution of surviving strategies (mean and 2x standard deviations across 100 repetitions).")



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
