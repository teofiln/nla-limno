library(tidyverse)
library(here)

first <- read_csv("2017/nla_2017_profile-data.csv", show_col_types = FALSE)
second <- read_csv("2022/nla2022_profile_wide.csv", show_col_types = FALSE)


sum(first$SITE_ID %in% second$SITE_ID)
