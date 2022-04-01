## code to prepare `DATASET` dataset goes here

library(tidyverse)
real_dfs =
  read_csv(here::here("data", "combined_dfs_public.csv"))

usethis::use_data(real_dfs, compress = "xz")
