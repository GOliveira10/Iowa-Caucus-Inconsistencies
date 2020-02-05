library(rvest)
library(tidyverse)
library(tidycensus)
library(httr)
library(reshape2)

source("./theme_powerbi.R")

precincts <- GET("https://int.nyt.com/applications/elections/2020/data/api/2020-02-03/precincts/IowaDemPrecinctsSFTP-latest.json") %>%
  content()


precincts <- precincts$precincts %>% 
  map_if(negate(is.list), function(x) flatten_chr(x)) %>% 
  enframe()


results <- precincts$value %>% map("results") %>% bind_rows()
results_first_alignment <- precincts$value %>% map("results_align1") %>% bind_rows()
results_final_alignment <- precincts$value %>% map("results_alignfinal") %>% bind_rows()

## Ugly but doing it on the fly
precinct_details <- precincts$value %>% 
  map(function(x) keep(x, negate(is.list))) %>% 
  map(bind_rows) %>% map(sapply, as.character) %>% 
  map(bind_rows) %>% bind_rows()

precinct_details <- precinct_details %>% 
  mutate(precinct = paste0(precinct, "-", precinct_id))


precinct_names <- precinct_details %>% 
  select(precinct, is_complete)


## Combine first alignment results with precinct details
first_alignment_results <- results_first_alignment %>% bind_cols(precinct_names) %>%
  melt(id.vars = c("precinct", "is_complete")) %>% rename(candidate = variable, votes_first = value) %>%
  group_by(precinct) %>% mutate(total_first = sum(votes_first)) %>%
  group_by(precinct, candidate) %>% mutate(percent_first = votes_first/total_first) %>%
  mutate(candidate_not_viable_first = ifelse(percent_first < .15, TRUE, FALSE)) %>%
  group_by(candidate) %>%
  ungroup() 


## Same same with final results
final_alignment_results <- results_final_alignment %>% bind_cols(precinct_names)  %>%
  melt(id.vars = c("precinct", "is_complete")) %>% rename(candidate = variable, votes_final = value) %>%
  group_by(precinct) %>% mutate(total_final = sum(votes_final)) %>%
  group_by(precinct, candidate) %>% mutate(percent_final = votes_final/total_final) %>%
  mutate(candidate_viable_final = ifelse(percent_final > .15, TRUE, FALSE)) %>%
  ungroup() 


results_full <- first_alignment_results %>%
  left_join(final_alignment_results, by = c("precinct", "candidate", "is_complete")) %>%
  mutate(change_from_first = votes_final - votes_first)


viability <- results_full %>% filter(is_complete == "TRUE") %>%
  select(precinct, candidate, candidate_not_viable_first) %>%
  mutate(candidate = paste0(candidate, "_not_viable_after_first")) %>%
  dcast(precinct ~ candidate)


change_between_rounds <- results_full %>% 
  filter(is_complete == "TRUE") %>%
  select(precinct, candidate, change_from_first) %>%
  mutate(candidate = paste0(candidate, "_change")) %>%
  dcast(precinct~candidate)


change_between_rounds %>%
  inner_join(viability, by = "precinct") %>%
  select(-precinct) %>% cor() %>% 
  as_tibble(rownames = "corr") %>%
  filter(str_detect(corr, "change")) %>% 
  select(contains("viable"), corr) %>%
  column_to_rownames(var = "corr") %>% 
  as.matrix() %>% ggcorrplot::ggcorrplot(lab = TRUE, insig = "blank", colors = c("red3", "white", "green3")) +
  ggtitle("Correlations Between Non-Viable Candidates and Second Round Gains") +
  theme_powerbi()


## Coded few conditions that might be of interest.
results_full %>% 
  group_by(precinct) %>% 
  mutate(unviable_to_viable = ifelse((candidate_not_viable_first & candidate_viable_final & 
                                        !candidate %in% c("other", "uncommitted")), TRUE, FALSE),
         viable_to_unviable = ifelse((!candidate_not_viable_first & !candidate_viable_final & 
                                        !candidate %in% c("other", "uncommitted")), TRUE, FALSE),
         zero_votes_to_nonzero_votes = ifelse((votes_first == 0 & votes_final > 0), TRUE, FALSE),
         viable_to_zero_votes = ifelse((!candidate_not_viable_first & votes_final == 0 & 
                                          !candidate %in% c("other", "uncommitted")), TRUE, FALSE)) %>%
  group_by(precinct) %>% arrange(precinct, candidate) %>% 
  mutate(mickey_mouse_shit = ifelse((zero_votes_to_nonzero_votes|viable_to_zero_votes), TRUE, FALSE)) %>%
                  group_by(precinct) %>%  
  mutate(precincts_with_mms = max(mickey_mouse_shit)) %>% 
  filter(precincts_with_mms == 1) %>% 
  arrange(precinct, candidate) %>% 
  select(viable_to_zero_votes, zero_votes_to_nonzero_votes, viable_to_unviable,
         unviable_to_viable, everything()) %>% 
  filter(viable_to_zero_votes & !candidate %in% c("other", "uncommitted")) %>% view()


