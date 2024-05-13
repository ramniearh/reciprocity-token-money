library(here)
library(tidyverse)
library(janitor)

#TOO-COARSE TESTS - go more in-depth, think first

# Import ant treat data:

df_direct <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "direct reciprocity - verification", "money-reciprocity 3.0 - BehaviorSpace direct reciprocity verification-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  select(-quid_pro_quo, -initial_money,-debt_threshold) %>%
  #select(-initial_reputation,-reputation_threshold) %>% 
  select(
    -starts_with("sum_bal"),
    -starts_with("sum_sco") 
    ) %>%
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    average_memory_length = x_sum_length_memory_of_turtles_count_turtles,
  ) %>% 
  mutate(
    evolutionary_updating = as.logical(evolutionary_updating),
    forgiveness = as.logical(forgiveness)
  )

# Analysis:
## filter for with/without evol upd 
## filter for with/without coops
## evaluate effect of forgiveness and memory size

# Summarize cooperation rate data at end of run
df_direct_evol <- df_direct %>% 
  filter(evolutionary_updating == T) %>% 
  mutate(bc_ratio = as.factor(bc_ratio))

df_direct_evol %>% 
  group_by(memory_size, forgiveness) %>% 
  summarise(mean(cooperation_rate), sd(cooperation_rate), median(cooperation_rate))

# Plot cooperation rates x benefit/cost ratios at selected time steps
df_direct_evol %>% 
  filter(step %in% c(5000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
  geom_boxplot() + 
  ggtitle("Direct Reciprocity: cooperation rates for different benefit/cost ratios at 5000 time steps")
  
df_direct_evol %>% 
  filter(step %in% c(100, 1000, 5000, 10000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate,)) + 
  geom_boxplot() + 
  facet_grid(n_coop~step, labeller = label_both) +
  ggtitle("Direct Reciprocity: cooperation rates for different benefit/cost ratios at selected time steps for liquidity = 1 (baseline value)")
  
df_direct_evol %>% 
  filter(step %in% c(1000)) %>% 
  filter(n_coop == 0) %>% #TESTING
  ggplot(aes(x=bc_ratio, y=cooperation_rate,)) + 
  geom_boxplot() + 
  facet_grid(forgiveness~memory_size, labeller = label_both) +
  ggtitle("Direct Reciprocity: forgiveness and memory effects (at 1000 steps, no cooperators)")


# Plot distribution of cooperation rates at selected time steps and benefit/cost ratios
df_direct_evol %>%
  mutate(step = as.factor(step)) %>% 
  filter(step %in% c(5000), bc_ratio != 1) %>% 
  ggplot(aes(x=cooperation_rate, fill = bc_ratio)) +
  geom_histogram(position = "identity", alpha = 0.3) +
  ggtitle("Direct Reciprocity: distribution of cooperation rates at 5000 steps")

# 
df_direct_evol %>%
  mutate(step = as.factor(step)) %>% 
  filter(step %in% c(1000, 5000, 10000)) %>% 
  ggplot(aes(x=cooperation_rate, fill = step)) +
  geom_histogram(position = "identity", alpha = 0.3) +
  facet_wrap( ~ bc_ratio, labeller = label_both) +
  ggtitle("Direct Reciprocity: distribution of cooperation rates at selected time steps")

  
# geom_bar()# Plot strategy counts at end of run
summary_df_direct_evol <- df_direct_evol %>% 
  filter(step < 10000) %>% 
  filter(n_coop == 100) %>%  ######### TESTING
  pivot_longer(starts_with("count"), names_to = "strategy", values_to = "survivors") %>% 
  mutate(
    strategy = case_when(
      strategy == "count_cooperators" ~ "cooperators",
      strategy == "count_defectors" ~ "defectors",
      strategy == "count_directs" ~ "direct reciprocators",
      strategy == "count_indirects" ~ "indirect reciprocators",
      strategy == "count_moneys" ~ "money users",
      )
  ) %>% 
  group_by(step, strategy, forgiveness, memory_size) %>%
  summarise(
    mean_survivors = mean(survivors),
    sd_survivors = sd(survivors)
    ) 

summary_df_direct_evol %>% 
  filter(bc_ratio == 5) %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
  facet_grid(forgiveness ~ memory_size, labeller = label_both) +
  ggtitle("Driect Reciprocity: evolution of surviving strategies by memory/forgiv (b/c ratio = 5, no cooperators, pop=200)")
