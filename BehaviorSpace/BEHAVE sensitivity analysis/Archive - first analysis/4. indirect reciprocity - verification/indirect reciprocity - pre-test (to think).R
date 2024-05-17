library(here)
library(tidyverse)
library(janitor)

#TOO-COARSE TESTS - go more in-depth, think first

# Import ant treat data:

df_indirect <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "indirect reciprocity - verification", "money-reciprocity 3.0 - BehaviorSpace indirect reciprocity verification-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  select(-quid_pro_quo, -initial_money,-debt_threshold) %>%
  select(
    -starts_with("sum_bal"),
    -starts_with("sum_sco") 
    ) %>%
  rename(
    step = x_step, run_number = x_run_number, bc_ratio = benefit_to_cost_ratio
  ) %>% 
  mutate(
    evolutionary_updating = as.logical(evolutionary_updating)
  )

# Analysis:
## filter for with/without evol upd 
## filter for with/without coops
## evaluate effect of reputation

# Summarize cooperation rate data at end of run
df_indirect_evol <- df_indirect %>% 
  filter(evolutionary_updating == T) %>% 
  mutate(bc_ratio = as.factor(bc_ratio))

df_indirect_evol %>% 
  group_by(initial_reputation, reputation_threshold) %>% 
  summarise(mean(cooperation_rate), sd(cooperation_rate), median(cooperation_rate))

# Plot cooperation rates x benefit/cost ratios at selected time steps
df_indirect_evol %>% 
  filter(step %in% c(5000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
  geom_boxplot() + 
  ggtitle("Indirect Reciprocity: cooperation rates for different benefit/cost ratios at 5000 time steps")
  
df_indirect_evol %>% 
  filter(step %in% c(100, 1000, 5000, 10000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate,)) + 
  geom_boxplot() + 
  facet_grid(n_coop~step, labeller = label_both) +
  ggtitle("Indirect Reciprocity: cooperation rates for different benefit/cost ratios at selected time steps for liquidity = 1 (baseline value)")
  
df_indirect_evol %>% 
  filter(step %in% c(500, 2000, 5000)) %>% 
  filter(n_coop == 100, initial_reputation == 0) %>% #TESTING
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
  geom_boxplot() + 
  facet_grid(step ~ reputation_threshold, labeller = label_both) +
  ggtitle("Indirect Reciprocity: threshold effects (at selected time steps, with cooperators, initial reputation = 0)")


# Plot distribution of cooperation rates at selected time steps and benefit/cost ratios
df_indirect_evol %>%
  mutate(step = as.factor(step)) %>% 
  filter(step %in% c(5000), bc_ratio != 1) %>% 
  ggplot(aes(x=cooperation_rate, fill = bc_ratio)) +
  geom_histogram(position = "identity", alpha = 0.3) +
  ggtitle("Indirect Reciprocity: distribution of cooperation rates at 5000 steps")

# 
df_indirect_evol %>%
  mutate(step = as.factor(step)) %>% 
  filter(step %in% c(1000, 5000, 10000)) %>% 
  ggplot(aes(x=cooperation_rate, fill = step)) +
  geom_histogram(position = "identity", alpha = 0.3) +
  facet_wrap( ~ bc_ratio, labeller = label_both) +
  ggtitle("Direct Reciprocity: distribution of cooperation rates at selected time steps")

  
# geom_bar()# Plot strategy counts at end of run
summary_df_indirect_evol <- df_indirect_evol %>% 
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
  group_by(step, strategy, bc_ratio, reputation_threshold, initial_reputation) %>%
  summarise(
    mean_survivors = mean(survivors),
    sd_survivors = sd(survivors)
    ) 

summary_df_indirect_evol %>% 
  filter(bc_ratio != 1, initial_reputation == 0) %>%  ## REVIEW
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
  facet_grid(bc_ratio ~ reputation_threshold, labeller = label_both) +
  ggtitle("Indirect Reciprocity: evolution of surviving strategies by threshold (nowith cooperators, pop=300)")
