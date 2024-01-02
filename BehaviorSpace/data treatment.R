library(tidyverse)
library(here)

BStable <- here("population 100 cost2 benefit 5 replacement 0.01", "token-reciprocity exp complete-p100-c2b5-e1 streamlined-table.csv")
data <- read.csv(BStable, skip = 6)
structure(data)
view(data)
