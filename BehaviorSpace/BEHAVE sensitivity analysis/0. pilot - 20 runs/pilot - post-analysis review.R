library(here)
library(tidyverse)
library(janitor)

# Import ant treat data:

df_pilot <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "0. pilot - 20 runs", "20x5 baseline run - BEHAVE money-reciprocity 3.0 experiment-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  select(-quid_pro_quo) %>%
  select(-initial_reputation,-reputation_threshold, -debt_threshold) %>% 
  #select(-forgiveness,-full_memory) %>% 
  select(-evolutionary_updating) %>% 
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with("n_"), -starts_with(("x_sum"))) %>% ##confirm?
  select(-starts_with("fitness")) %>% 
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
  ) %>% 
  mutate(bc_ratio = as.factor(bc_ratio)) %>% 
  mutate( money_prevalence =
            count_moneys / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects)
  ) 

# Analysis:

# Summarize cooperation rate data at end of run
df_pilot %>%
  group_by(step, bc_ratio, initial_money) %>% 
  summarise(mean(cooperation_rate), sd(cooperation_rate), mean(money_prevalence))

df_pilot %>% 
  filter(step %in% c(1000, 5000, 10000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) + ##color = bc_ratio!?!
  geom_boxplot() +
  #scale_color_manual(values = c("red", "blue", "green", "yellow")) +
  facet_grid(initial_money~step, labeller = label_both) +
  ggtitle("Five strategies, small populations: Cooperation rates for different benefit/cost ratios. Selected time steps and liquidity values.")
  
  
# geom_bar()# Plot strategy counts at end of run
summary_df_pilot <- df_pilot %>% 
  filter(step < 20000) %>% 
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
  group_by(bc_ratio, step, strategy, initial_money) %>%
  summarise(
    mean_survivors = mean(survivors),
    sd_survivors = sd(survivors)
    ) 

summary_df_pilot %>% 
  filter(bc_ratio %in% c(1, 2, 5, 20)) %>% 
  filter(step %% 500 == 0) %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
  facet_grid(bc_ratio ~ initial_money, labeller = label_both) +
  ggtitle("Five strategies, small populations: evolution of surviving strategies by initial liquidity (pop=100)")



# count repetitions:
  df_pilot %>% filter(bc_ratio == 1 & initial_money == 1 & step == 0) %>% view()
  