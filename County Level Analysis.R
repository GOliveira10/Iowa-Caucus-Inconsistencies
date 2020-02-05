
## Some earlier work looking at county by county breakdowns

url <- 'https://www.nytimes.com/interactive/2020/02/04/us/elections/results-iowa-caucus.html'


iowa_data <- url %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="ia-17275-2020-02-03-county"]/table') %>%
  html_table() %>% flatten_dfc()



iowa_data <- iowa_data %>%
  mutate(`Rpt.` = str_remove_all(`Rpt.`, "%"))


iowa_data[,-1] <- sapply(iowa_data[,-1], as.double) %>% as_tibble()

iowa_data <- iowa_data %>% 
  mutate(`Rpt.` = `Rpt.`/100)

iowa_melt <- iowa_data %>% 
  melt(id.vars = c("County", "Rpt.")) %>%
  group_by(County) %>%
  mutate(Leader = paste0(variable[value == max(value)], collapse = "/")) 

iowa_melt$Leader <- as.factor(iowa_melt$Leader)

iowa_melt <- iowa_melt %>%
  mutate(Leader = as_factor(Leader)) %>%
  rename(Candidate = variable) %>%
  mutate(Leader = fct_reorder(Leader, value, .fun = median))

## Distribution of precincts left to report by county leader
iowa_melt %>% drop_na() %>% filter(`Rpt.` > 0)  %>%
  ggplot(aes(x=Leader, y = `Rpt.`, fill = Leader)) + geom_violin() + 
  ggtitle("Reporting Percentage By Leader") + 
  scale_y_continuous(labels = scales::percent) + 
  theme_powerbi() + scale_fill_powerbi() 


library(tidycensus)

# You'll need to go get a census api key for this
# census_api_key(api_key, install = TRUE)


us_county_income <- get_acs(geography = "county", variables = "B19013_001", 
                            shift_geo = FALSE, geometry = FALSE)


iowa_county_income <- us_county_income %>% 
  select(-GEOID, -variable, -moe) %>%
  separate(NAME, into = c("County", "State"), sep = ", ") %>%
  mutate(County = trimws(str_remove_all(County, " County"))) %>%
  filter(State == "Iowa") %>%
  rename(`Median Household Income` = estimate)

## County level vote percentages by median household income. Not too much of interest here.
iowa_data %>%
  inner_join(iowa_county_income, by = "County") %>%
  select(-`Rpt.`, -State) %>%
  melt(id.vars = c("Median Household Income", "County")) %>%
  group_by(County) %>%
  mutate(TotalDels = sum(value)) %>%
  mutate(`Percent of Delegates` = value/TotalDels,) %>%
  ggplot(aes(x=`Median Household Income`, y = `Percent of Delegates`, color = variable)) +
  geom_point() + geom_smooth(method = "lm") + 
  facet_wrap(~variable) + theme_powerbi() + scale_color_powerbi() +
  ggtitle("Share of Delegates by County Median Household Income") + 
  labs(color = "Candidate") + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::dollar)


