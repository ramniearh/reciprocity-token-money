##### R script: Money and Generalized Cooperation #####
## Auxiliary plot for Interdependence and Self-enforcement analysis. Evolution and cooperation rate charts
## Simulated data for reputation-to-money scenario, with  variation of liquidity and benefit/cost parameters. With and without evolutionary updating
# ["N-ind-mon" 100]
# ["N-coop" 0 100]
# ["N-defect" 0 100]
# ["self-enforcement?" true false]
# ["interdependence?" true false]
# ["evolutionary-updating?" true false]
# ["benefit-to-cost-ratio" 1 2 10 100]
# ["initial-balance-scores" 0 1 5 1000]
# ["threshold" 0]


# 1. Import libraries and packages -------------------------------------------
library(here)
library(tidyverse)
library(janitor)
here()



# 2. Import and process data -------------------------------------------------

# 2.1 Simulation data for the reputation-to-money scenario:
df_interself_money <- 
  here("BehaviorSpace", "BEHAVE sensitivity analysis", "SSC-SASE CORE", "3. Interdependence and self-enforcement", "money-reciprocity 3.0 - interself experiment - CORE INTERSELF-table.csv") %>% 
  read.csv(skip = 6) %>% 
  clean_names() %>%
  as_tibble()  %>% 
  #select(-starts_with("sum_bal"),-starts_with("sum_sco"), -starts_with(("x_sum"))) %>%
  #select(-starts_with("fitness")) %>%
  rename(
    step = x_step, 
    run_number = x_run_number,
    bc_ratio = benefit_to_cost_ratio,
    liquidity = initial_balance_scores 
  ) %>% 
  mutate(
    bc_ratio = as.factor(bc_ratio),
    evolutionary_updating = as.logical(evolutionary_updating),
    interdependence = as.logical(interdependence),
    self_enforcement = as.logical(self_enforcement)
  ) %>% 
  mutate(
    share_cooperators = count_cooperators / ( count_cooperators + count_defectors + count_ind_mons ),
    share_defectors = count_defectors /  ( count_cooperators + count_defectors + count_ind_mons ),
    share_interself_moneys = count_ind_mons /  ( count_cooperators + count_defectors + count_ind_mons )
  ) %>% 
  select(-starts_with("count_")) %>% 
  mutate(
    stage = case_when(
     interdependence == FALSE & self_enforcement == FALSE ~ "0. no interdependence or self-enforcement (reputation equivalent)", 
     interdependence == TRUE & self_enforcement == FALSE ~ "1. adding interdependence (notional stage)",
     interdependence == TRUE & self_enforcement == TRUE ~ "2. with interdependence and self-enforcement (our money mechanism)",
     interdependence == FALSE & self_enforcement == TRUE ~ "NO_SCENARIO",
    )
  ) %>% 
  filter(stage != "NO_SCENARIO") %>% 
  mutate(
    population_setup = case_when(
      n_coop == 100 & n_defect == 100 ~ "2. against cooperators and defectors", 
      n_coop == 0 & n_defect == 100 ~ "0. against defectors only", 
      n_coop == 100 & n_defect == 0 ~ "1. against cooperators only", 
      n_coop == 0 & n_defect == 0 ~ "ONLY MONEY", 
    )
  ) %>% 
  filter(population_setup != "ONLY MONEY") %>% 
  mutate(
    stage = as.factor(stage),
    population_setup = as.factor(population_setup)
  ) %>%
  pivot_longer(cols = starts_with("share_"), names_to = "strategy", values_to = "survivor_count") %>% 
  mutate(strategy =
           case_when(
             strategy == "share_cooperators" ~ "cooperators",
             strategy == "share_defectors" ~ "defectors",
             strategy == "share_directs" ~ "direct-reciprocators",
             strategy == "share_indirects" ~ "indirect-reciprocators",
             strategy == "share_interself_moneys" ~ "reputation-or-money-users",
           )
  )


# 3. Draw main evolution plots --------------------------------------------


# 3.1 Plot cooperation rates and share of surviving strategies in time (with money, selected parameter values)
  # with coops and defectors
  # interdependence true/false
  # self-enforcement true/false
  # NOTE: excess data with self-enforcement = TRUE and interdependence = FALSE
    ## also with COOP 0-100 DEF 0-100
  
p_interself = df_interself_money %>% 
  filter(evolutionary_updating == TRUE) %>% 
  filter(bc_ratio == 2 & liquidity == 1 ) %>% 
  #filter(n_coop == 100 & n_defect == 0) %>% 
  #filter(interdependence == TRUE & self_enforcement == TRUE) %>% 
  filter(survivor_count > 0) %>% 
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
                                "reputation-or-money-users"="#C77CFF"
  )) +
  scale_fill_manual(values = c("cooperators" = "#F8766D",
                               "defectors"="#ABA300",
                               "reputation-or-money-users"="#C77CFF"
  )) +
  labs(
    x = "Simulation time step",
    y = "Proportion (0-1)",
    color = "Share of survivors by strategy",
    fill = "Share of survivors by strategy",
    shape = "Cooperation rate",
    linetype = "Cooperation rate",
    title = "From reputation to money: evolution of surviving strategies and cooperation rates (Median and IQR over 100 repetitions; total population = 200 (300); benefit-to-cost ratio = 2, liquidity = 1)"
  ) +
  #facet_grid(liquidity ~ bc_ratio, labeller = label_both) #+
  #facet_grid(interdependence ~ self_enforcement, labeller = label_both) #+
  #facet_grid(n_coop ~ n_defect, labeller = label_both) #+
  facet_grid(population_setup ~ stage) #+ theme_minimal()

p_interself




# 4. Generate visualizations -------------------------------------------------
p_money
