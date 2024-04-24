library(tidyverse)
library(readr)
library(dplyr)
library(car)
library(ggplot2)


# read in files
wb_groups <- read_csv("WB_groups.csv",col_select = c(1:3))
gdp <- read_csv("GDP.csv",skip = 4)
population <- read_csv("population.csv",skip = 4)

# Question 1: GDP per capita = GDP/population for each country/year pair
# data transformation: 
gdp_long <- gdp %>% pivot_longer(cols = -c("Country Name", "Country Code","Indicator Name","Indicator Code"), names_to = 'year', values_to = 'GDP') %>% drop_na() %>% subset(., select = -c(`Indicator Name`,`Indicator Code`))
pop_long <- population %>% pivot_longer(cols = -c("Country Name", "Country Code","Indicator Name","Indicator Code"), names_to = 'year', values_to = 'Population') %>% drop_na() %>% subset(., select = -c(`Indicator Name`,`Indicator Code`))
gdp_per_cap <- gdp_long %>% inner_join(pop_long, by = c("Country Name", "Country Code","year")) %>% 
  mutate(`GDP per capita` = `GDP`/`Population`)

# Question 2: regional GDP with weighted average
# Detected error in WB_groups.csv, filter for AFW and AFE regions before merging
regional_groups <- wb_groups %>% filter(GroupCode %in% c("AFW", "AFE"))
gdp_af <- gdp_per_cap %>% inner_join(regional_groups, by = c("Country Code" = "CountryCode"))
# Calculate regional population and regional GDP
gdp_regional <- gdp_af %>% group_by(year, GroupCode) %>% 
                            summarise(RegionalPop = sum(Population),RegionalGDP = sum(GDP)) %>% 
                            mutate(`Avg GDP per capita` = `RegionalGDP`/`RegionalPop`)

# Visulization using ggplot
plot_q2 <- ggplot(data = gdp_regional, aes(x = year, y = `Avg GDP per capita`, group = GroupCode, color = GroupCode)) + 
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Average GDP per capita for AFW and AFE regions, 1960-2022")
plot_q2

# Question 3: Please see ReadMe.md







