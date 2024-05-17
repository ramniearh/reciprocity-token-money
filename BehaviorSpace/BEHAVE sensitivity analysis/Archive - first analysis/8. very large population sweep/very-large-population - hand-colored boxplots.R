library(here)
library(tidyverse)
library(janitor)

# Import ant treat data:

df_very_large_pop <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "8. very large population sweep", "money-reciprocity 3.0 - BehaviorSpace large population - sensitivity analysis full sweep 300x5 20k-steps 100r-table.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  select(-quid_pro_quo) %>%
  select(-initial_reputation,-reputation_threshold) %>% 
  select(-forgiveness,-full_memory) %>% 
  select(-evolutionary_updating) %>% 
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with("n_"), -starts_with(("x_sum"))) %>% ##confirm?
  select(-starts_with("fitness")) %>% 
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
  ) %>% 
  mutate(bc_ratio = as.factor(bc_ratio)) %>% 
  mutate( money_prevalence =count_moneys / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects),
          direct_prevalence =count_directs / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects),
          indirect_prevalence =count_indirects / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects),
          defector_prevalence =count_defectors / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects),
          cooperator_prevalence =count_cooperators / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects),
          
  ) 

df_no_money_very_large_pop <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "8. very large population sweep", "money-reciprocity 3.0 - BehaviorSpace large population - sensitivity analysis full sweep 375x4 20k-steps 100r.csv") %>%
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble() %>% 
  select(-quid_pro_quo) %>%
  select(-initial_reputation,-reputation_threshold) %>% 
  select(-forgiveness,-full_memory) %>% 
  select(-evolutionary_updating) %>% 
  select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with("n_"), -starts_with(("x_sum"))) %>% ##confirm?
  select(-starts_with("fitness")) %>% 
  rename(
    step = x_step, run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
  ) %>% 
  mutate(bc_ratio = as.factor(bc_ratio)) %>% 
  mutate( money_prevalence =count_moneys / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects),
          direct_prevalence =count_directs / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects),
          indirect_prevalence =count_indirects / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects),
          defector_prevalence =count_defectors / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects),
          cooperator_prevalence =count_cooperators / ( count_moneys + count_defectors + count_cooperators + count_directs + count_indirects)
  ) %>% 
  mutate(half_winner = case_when(
    money_prevalence > 0.5 ~ "money",
    indirect_prevalence > 0.5 ~ "indirect",
    direct_prevalence > 0.5 ~ "direct",
    defector_prevalence > 0.5 ~ "defector",
    cooperator_prevalence > 0.5 ~ "cooperator",
#    else ~ "mixed"
  ))

# Analysis:

# Summarize cooperation rate data at end of run
df_very_large_pop %>%
  group_by(step, bc_ratio) %>% 
  summarise(mean(cooperation_rate), sd(cooperation_rate), mean(money_prevalence))

df_no_money_very_large_pop %>%
  group_by(step, bc_ratio) %>% 
  summarise(mean(cooperation_rate), sd(cooperation_rate), mean(money_prevalence))

df_very_large_pop %>% filter(step %in% c(1000, 5000, 10000, 20000)) %>% ggplot(aes(y=cooperation_rate,x=money_prevalence, color = bc_ratio)) + geom_point() + facet_grid(step~initial_money, labeller = label_both) + theme_minimal()

# Plot cooperation rates x benefit/cost ratios at selected time steps
##########hand-code color!
# df_very_large_pop %>% 
#   group_by(step, bc_ratio, initial_money) %>% 
#   summarise(
#     mean(money_prevalence), sd(money_prevalence), 
#     mean(direct_prevalence), sd(direct_prevalence), 
#     mean(indirect_prevalence), sd(direct_prevalence), 
#     mean(defector_prevalence), sd(direct_prevalence), 
#     mean(cooperator_prevalence), sd(direct_prevalence))  %>% 
#   ggplot(aes(x=bc_ratio, y=))

df_very_large_pop %>% 
  filter(step %in% c(1000, 5000, 10000, 20000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
  geom_boxplot() +
  #scale_color_manual(values = c("red", "blue", "green", "yellow")) +
  facet_grid(initial_money~step, labeller = label_both) +
  ggtitle("Cooperation rates for different benefit/cost ratios and initial money holdings at selected time steps.")
  
df_no_money_very_large_pop %>% 
  filter(step %in% c(1000, 5000, 10000, 20000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
  geom_boxplot() +
  #scale_color_manual(values = c("red", "blue", "green", "yellow")) +
  facet_grid(~step) +
  ggtitle("Cooperation rates for different benefit/cost ratios at selected time steps.")



# Plot surviving strategy evolution
summary_df_very_large_pop <- df_very_large_pop %>% 
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

summary_df_very_large_pop %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
  facet_grid(bc_ratio ~ initial_money, labeller = label_both) +
  ggtitle("Evolution of surviving strategies for different initial liquidity and benefit/cost ratios.") 


summary_df_no_money_very_large_pop <- df_no_money_very_large_pop %>% 
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

summary_df_no_money_very_large_pop %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
  facet_grid(~bc_ratio, labeller = label_both) +
  ggtitle("Evolution of surviving strategies for different initial liquidity and benefit/cost ratios.")


## Population comparison charts: all bc_ratios, reference liquidity, with and without money


df_very_large_pop %>% 
  filter(initial_money == 1 | initial_money == 5) %>% 
  filter(step %in% c(1000, 5000, 10000, 20000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
  geom_boxplot() +
  #scale_color_manual(values = c("red", "blue", "green", "yellow")) +
  facet_grid(initial_money~step, labeller = label_both) +
  ggtitle("Cooperation rates for different benefit/cost ratios. Selected time steps and liquidity values (including money, pop = 1500)")

df_no_money_very_large_pop %>% 
  filter(step %in% c(1000, 5000, 10000, 20000)) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
  geom_boxplot() +
  #scale_color_manual(values = c("red", "blue", "green", "yellow")) +
  facet_grid(~step) +
  ggtitle("Cooperation rates for different benefit/cost ratios. Selected time steps and liquidity values (no money, pop = 1500)")



summary_df_no_money_very_large_pop %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
  facet_wrap(~ bc_ratio, labeller = label_both) +
  ggtitle("Evolution of surviving strategies for different benefit/cost ratios (no money, pop = 1500)")

summary_df_very_large_pop %>% 
  filter(initial_money == 1 | initial_money == 5) %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) +
  facet_grid(initial_money ~ bc_ratio, labeller = label_both) +
  ggtitle("Evolution of surviving strategies for different initial liquidity and benefit/cost ratios (including money, liquidity = 5, pop = 1500)")




# PPT presentation of representative result (bc = 5, 20000 time steps, initial_money = 5)
summary_df_very_large_pop %>% 
  filter(initial_money == 5, bc_ratio == 5) %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  ylim(0,1500) +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) 

summary_df_no_money_very_large_pop %>% 
  filter(bc_ratio == 5) %>% 
  ggplot(aes(x=step, y=mean_survivors, color = strategy)) +
  geom_line() +
  ylim(0,1500) +
  geom_ribbon(aes(ymin = mean_survivors - 1.96 * sd_survivors, ymax = mean_survivors + 1.96 * sd_survivors, fill = strategy), alpha = 0.2) 

df_very_large_pop %>% 
  filter(step == 5000 | step == 20000, initial_money == 5) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
  geom_boxplot() +
  ylim(0,1) +
  facet_wrap(~step)
#scale_color_manual(values = c("red", "blue", "green", "yellow")) 

df_no_money_very_large_pop %>% 
  filter(step == 5000 | step == 20000) %>% 
  ggplot(aes(x=bc_ratio, y=cooperation_rate)) + 
  geom_boxplot() +
  ylim(0,1) +
  facet_wrap(~step)