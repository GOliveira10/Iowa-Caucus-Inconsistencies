library(rvest)
library(tidyverse)
library(httr)

# For questions contact @MCulshawMaurer on twitter or @MCMaurer on GitHub

# pull the data in and get the relevant results
precincts <- GET("https://int.nyt.com/applications/elections/2020/data/api/2020-02-03/precincts/IowaDemPrecinctsSFTP-latest.json") %>%
  content()
precincts <- precincts$precincts %>% 
  map_if(negate(is.list), function(x) flatten_chr(x)) %>% enframe()

# making the results into something legible
results <- precincts$value %>% 
  map(unlist) %>% 
  map(map, as.character) %>% 
  map(bind_rows) %>% 
  bind_rows() %>% 
  rename(County = locality_name)

# reading in the delegate counts per precinct, which are used for determining the viability threshold for a precinct
d <- read_csv("Delegate Apportionment_Page 1_Pivot table.csv")
d <- d %>% 
  rename(precinct = `Precinct Short Name`,
         precinct_delegates = `Delegates to County Convention`,
         sde = `State Delegate Equivalents`)

results <- left_join(results, d)

# clean the data up so candidates are listed nicely, the right columns are numeric and such, and then at the end, set the viability threshold based on the official guidelines here: https://acc99235-748f-4706-80f5-4b87384c1fb7.filesusr.com/ugd/5af8f4_3abefbb734444842ae1abf985876cce8.pdf
results <- results %>% 
  pivot_longer(cols = -c(1:7, 50:54), names_to = "result_type", values_to = "result") %>% 
  mutate(result_type = stringr::str_remove_all(result_type, "results_")) %>% 
  separate(col = result_type, into = c("round", "candidate")) %>% 
  map_at(vars(votes, votes_align1, votes_alignfinal, result), as.numeric) %>% 
  as_tibble() %>% 
  select(-c(locality_fips, geo_id, locality_type)) %>% 
  filter(round != "results") %>% 
  mutate(is_complete = as.logical(is_complete)) %>% 
  mutate(precinct_full = paste(precinct, precinct_id, sep = "_")) %>% 
  pivot_wider(names_from = round, values_from = result) %>% 
  mutate(viability_threshold = case_when(
    precinct_delegates >= 4 ~ 0.15,
    precinct_delegates == 3 ~ (1/6),
    precinct_delegates == 2 ~ 0.25,
    precinct_delegates == 1 ~ 0.5,
    TRUE ~ NA_real_
  ))

# figure out cases where candidates are ABOVE the viability threshold in round 1 and BELOW in the final round
results <- results %>% 
  mutate(viable1 = (align1 / votes_align1) > viability_threshold,
         viablefinal = (alignfinal / votes_alignfinal) > viability_threshold,
         weird = (viable1 & !viablefinal)) %>% 
  pivot_longer(cols = c("align1", "alignfinal"), names_to = "round", values_to = "result")

results %>% 
  filter(weird)

# find precincts where the weird results happened, ignoring cases of uncommitted (because this category isn't subject to the viability thresholds)

viable_non_precincts <- results %>% 
  filter(weird, candidate != "uncommitted") %>% 
  .$precinct_full
viable_non_precincts

# now plot the data as a paired stacked barchart, and outline all the weird occurrences in red
g <- results %>% 
  filter(precinct_full %in% viable_non_precincts, candidate != "uncommitted") %>% 
  ggplot(aes(x = round, y = result, group = candidate, fill = candidate)) +
  geom_bar(stat = "identity", aes(color = weird)) +
  facet_wrap(~ precinct_full, scales = "free_y") +
  scale_fill_viridis_d() +
  scale_color_manual(values = c(NA, "red")) +
  MCMsBasics::minimal_ggplot_theme()

ggsave(plot = g, filename = "~/iowa_2020_results_viablenon_precinct.jpg", width = 16, height = 16)
