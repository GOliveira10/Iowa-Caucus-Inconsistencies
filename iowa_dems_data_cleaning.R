library(tidyverse)

iowa_data <- read_csv("iowadems_resumts_0205_8PM.csv")

new_names <- paste(names(iowa_data), iowa_data[1,], sep = "_")

new_names <- new_names %>% stringr::str_replace_all(" ", "_") %>% 
  stringr::str_replace_all("1_", "") %>% 
  stringr::str_replace_all("2_", "") %>% 
  stringr::str_replace_all("Final_Expression", "final") %>% 
  stringr::str_replace_all("First_Expression", "first")

iowa_data <- iowa_data[-(1:2),]
names(iowa_data) <- new_names

iowa_data <- iowa_data %>% 
  rename(County = candidate_subhead, precinct = XNA) %>% 
  filter(precinct != "Total")

iowa_data <- iowa_data %>% 
  pivot_longer(cols = -c(County, precinct), names_to = "cand_round", values_to = "result") %>% 
  separate(cand_round, into = c("candidate", "round"))

iowa_data %>% 
  write_csv("cleaned_iowa_dems_data.csv")

d <- read_csv("Delegate Apportionment_Page 1_Pivot table.csv")
d <- d %>% 
  rename(precinct = `Precinct Short Name`,
         precinct_delegates = `Delegates to County Convention`,
         sde = `State Delegate Equivalents`)

left_join(iowa_data, d)



iowa_names <- read_tsv("iowadems_resumts_0205_8PM.csv", n_max = 0) %>% names()
iowa_names2 <- read_tsv("iowadems_resumts_0205_8PM.csv", n_max = 0, skip = 1) %>% names()
iowa_names2
