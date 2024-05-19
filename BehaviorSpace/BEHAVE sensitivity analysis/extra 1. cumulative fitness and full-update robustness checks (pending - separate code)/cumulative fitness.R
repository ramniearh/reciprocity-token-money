library(here)
library(tidyverse)
library(janitor)
here()

# SEPARATE MODEL CODE
# removed +1 nowak corrector
# added some initial fitness to avoid negative values
# testing only for higher b/c ratios to avoid negative values

# Import ant treat data:

df_cumulative <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "extra 1. cumulative fitness and full-update robustness checks (pending - separate code)", "mr3.0 cumulative fitness 100x5 20k-steps 10rep-table.csv") %>%
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
    bc_ratio = as.factor(bc_ratio),
    cumulative_fitness = as.logical(cumulative_fitness)
    ) 

df_cumulative %>% 
  filter(step %in% c(1000, 5000, 10000, 20000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) +
  geom_boxplot() + 
  ylim(0,1) +
  facet_grid(initial_money~step, labeller = label_both)+
  ggtitle("Cooperation rates for different benefit-to-cost ratios")

df_cumulative %>% 
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
  facet_grid(bc_ratio~initial_money, labeller = label_both) +
  ggtitle("Evolution of surviving strategies")

