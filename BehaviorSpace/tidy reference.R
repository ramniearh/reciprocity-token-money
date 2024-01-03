
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
library(tidyverse)
library(here)
library(janitor)
library(kableExtra)

data <-
  here("The Dissemination of Culture experiment-table.csv") %>%
  read_csv(skip = 6) %>%
  clean_names()

data %>%
  group_by(number_of_features, number_of_traits) %>%
  summarise(number_of_regions = mean(number_of_regions)) %>%
  pivot_wider(names_from = number_of_traits, values_from = number_of_regions) %>%
  rename("Number of Features" = "number_of_features") %>%
  kbl() %>%
  kable_styling() %>%
  add_header_above(c(" " = 1, "Traits per Feature" = 3))

data %>%
  ggplot(aes(as_factor(number_of_traits), number_of_regions)) +
  facet_wrap(~number_of_features, labeller = label_both) +
  geom_boxplot()