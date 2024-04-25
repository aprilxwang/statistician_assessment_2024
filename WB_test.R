library(tidyverse)
library(janitor)
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
gdp_af$year <- as.numeric(gdp_af$year)

# take a look at summary stats and data coverage, based on the data coverage, I decided
# to only use data after 1984 (inclusive) for visualizations.
# For further explanation, please see README.md
summary(gdp_af)
gdp_af %>% tabyl(year, GroupName)

gdp_af_sub <- gdp_af %>% subset(year >= 1984)

# Calculate regional population and regional GDP
gdp_regional <- gdp_af_sub %>% group_by(year, GroupCode) %>% 
  summarise(RegionalPop = sum(Population),RegionalGDP = sum(GDP)) %>% 
  mutate(`Avg GDP per capita` = `RegionalGDP`/`RegionalPop`)

# Visualization - trends 1960-2022
plot_trend <- ggplot(data = gdp_regional, aes(x = year, y = `Avg GDP per capita`, group = GroupCode, color = GroupCode)) + 
  geom_line() + scale_x_continuous(breaks=seq(1984,2022,1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = '',title = "Average GDP per capita for AFW and AFE regions, 1984-2022")
plot_trend

# Visualization - difference between afe and afw
gdp_diff <- gdp_regional %>% group_by(year) %>% summarise(GDP_diff = diff(`Avg GDP per capita`))
plot_diff <- ggplot(data = gdp_diff, aes(x = year, y = GDP_diff)) + 
  geom_bar(stat = "identity") + scale_x_continuous(breaks=seq(1984, 2022, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = '', y = "Difference in Average GDP (AFW-AFE)", title = "Average GDP per capita Differences between AFW and AFE regions, 1984-2022",
       subtitle = "Difference = GDP value of AFW - GDP value of AFE")
plot_diff

# Visualization for AFW
plot_afw <- ggplot() + 
  geom_line(data = subset(gdp_af, GroupCode == "AFW"), aes(x = year, y = `GDP per capita`, group = `Country Code`, color = `Country Code`)) + 
  geom_line(data = subset(gdp_regional, GroupCode == "AFW"), aes(x = year, y = `Avg GDP per capita`), color = 'black', size = 0.8) +
  geom_text(data = subset(gdp_regional, GroupCode == "AFW" & year == 2022), aes(label = 'AFW Region', x = 2025, y = `Avg GDP per capita`), colour = "black") + 
  scale_y_continuous(breaks=seq(0,20000,2000)) + scale_x_continuous(breaks=seq(1960, 2022, 1)) +
  labs(x = '', y = "GDP per capita", title = "GDP per capita in AFW and Regional Average, 1960-2022") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_afw

# Question 3: Please see ReadMe.md







