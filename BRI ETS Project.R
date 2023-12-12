################################################################################
##
## [ INST ] Columbia SIPA
## [ CLAS ] Carbon Pricing
## [ PROJ ] Expanding Chinese ETS to BRI Project
## [ FILE ] Chinese ETS BRI Project
## [ AUTH ] < Ardhi Wardhana >
## [ INIT ] < Dec 12, 2023 >
##
################################################################################



library(writexl)
library(tidyverse)
library(ggplot2)
library(readxl)
library(assertive)
library(dplyr)

#load data set
bri <- read_xlsx("China Overseas Finance Inventory Database Version 2.2 - October 2023.xlsx")

#check data set
str(bri, attr = FALSE)

#1. How many countries? How many pojects each country?
unique(bri$country)


# Create a bar chart showing the number of projects in each country
ggplot(bri, aes(x = country)) +
  geom_bar(fill = "skyblue", color = "black", stat = "count") +
  labs(title = "Number of Projects in Each Country",
       x = "Country",
       y = "Number of Projects") +
  theme_minimal()

#debt - Debt
bri <- bri %>% 
  mutate(investment_type = ifelse(investment_type=="debt", "Debt", investment_type))


# Create a summary table with the count and sum of installed capacity for each country
project_summary <- bri %>%
  group_by(country) %>%
  summarise(number_of_projects = n(),
            total_installed_capacity = sum(installed_capacity, na.rm = TRUE),
            total_investment = sum(total_investment_amount, na.rm = TRUE),
            total_debt = sum(debt_investment_amount, na.rm = TRUE)) %>% 
  mutate(total_equity = total_investment - total_debt)




#2. How much is the equity and debt?
#3. How much is the loan?

bri %>% group_by(investment_type) %>% 
  summarise(n()) %>% 
  print()

# Create a bar chart showing the number of debt and equity
ggplot(bri, aes(x = investment_type)) +
  geom_bar(fill = "skyblue", color = "black", stat = "count") +
  labs(title = "Number of Projects in Each Type of Financing",
       x = "Investment Type",
       y = "Number of Projects") +
  theme_minimal()


ggplot(bri, aes(x = investment_type, fill = investment_type)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5)) +
  labs(title = "Number of Projects in Each Type of Financing",
       x = "Investment Type",
       y = "Number of Projects") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")



#percentage of debt in BRI power sector and total investment
sum(project_summary$total_debt)/sum(project_summary$total_investment)
sum(project_summary$total_investment)

#visualize
ggplot(project_summary, aes(x = number_of_projects, y = total_investment, label = country)) +
  geom_point(color = "blue", size = 3) +
  geom_text(hjust = 0.5, vjust = -0.5) +
  labs(title = "Scatter Plot: Number of Projects vs Total Investment",
       x = "Number of Projects",
       y = "Total Investment") +
  theme_minimal()


# Exclude specific countries
countries_to_exclude <- c("Indonesia", "Vietnam", "Pakistan", "Laos")

# Create a summary table excluding specified countries
project_summary2 <- project_summary %>%
  filter(!country %in% countries_to_exclude)

#visualize
ggplot(project_summary2, aes(x = number_of_projects, y = total_investment, label = country)) +
  geom_point(color = 'blue') +
  geom_text(hjust = 0.5, vjust = -0.5) +
  scale_size_continuous(name = "Total Equity", labels = scales::dollar_format(scale = 1e-6)) +  # Customize size legend
  labs(title = "Scatter Plot: Number of Projects vs Total Investment",
       x = "Number of Projects",
       y = "Total Investment") +
  theme_minimal()

# Commissioned project
project_summary3 <- bri %>%
  group_by(commissioning_year) %>%
  summarise(number_of_projects = n(),
            total_installed_capacity = sum(installed_capacity, na.rm = TRUE),
            total_investment = sum(total_investment_amount, na.rm = TRUE),
            total_debt = sum(debt_investment_amount, na.rm = TRUE)) %>% 
  mutate(total_equity = total_investment - total_debt)


#4. How many projects of fossil fuel? 
#5. How much emission?

# Create a summary table with the count and sum of installed capacity for each technology
tech_summary <- bri %>%
  group_by(primary_fuel) %>%
  summarise(number_of_projects = n(),
            total_installed_capacity = sum(installed_capacity, na.rm = TRUE),
            total_investment = sum(total_investment_amount, na.rm = TRUE),
            total_debt = sum(debt_investment_amount, na.rm = TRUE)) %>% 
  mutate(total_equity = total_investment - total_debt,
         emission_factor = case_when(
           primary_fuel == "biomass" ~ 326.42,
           primary_fuel == "coal" ~ 328.46,
           primary_fuel == "gas" ~ 181.22, # Specify the emission factor for "gas",
           primary_fuel == "oil" ~ 253.2,
           primary_fuel == "waste" ~ 316.4,
           TRUE ~ 0
         ),
         ghg_emission = emission_factor*total_installed_capacity)

sum(tech_summary$ghg_emission)


#Total fossil fuel projects
fossil_fuel <- tech_summary %>% 
  filter(primary_fuel %in% c("biomass", "coal", "gas", "oil", "waste"))

sum(fossil_fuel$number_of_projects)
sum(fossil_fuel$total_installed_capacity)
sum(fossil_fuel$total_investment)

#Total RE project
renewable_energy <- tech_summary %>% 
  filter(!primary_fuel %in% c("biomass", "coal", "gas", "oil", "waste", "unknown"))

sum(renewable_energy$number_of_projects)
sum(renewable_energy$total_installed_capacity)
sum(renewable_energy$total_investment)



# Sort by commissioning year
tech_summary2 <- bri %>%
  group_by(commissioning_year) %>%
  filter(primary_fuel %in% c("biomass", "coal", "gas", "oil", "waste")) %>%
  #filter(commissioning_year >= 2000 & (commissioning_year <= 2023)) %>% 
  mutate(
    emission_factor = case_when(
      primary_fuel == "biomass" ~ 326.42,
      primary_fuel == "coal" ~ 328.46,
      primary_fuel == "gas" ~ 181.22,
      primary_fuel == "oil" ~ 253.2,
      primary_fuel == "waste" ~ 316.4,
      TRUE ~ 0
    ),
    ghg_emission = emission_factor * installed_capacity
  ) %>%
  select(commissioning_year, ghg_emission)


#Total project until 2023
summarise(n())

#Total of emission in 2025
sum(tech_summary2$ghg_emission)


#Total of emission in 2023
sum(tech_summary2$ghg_emission)




#Visualization of Additional Yearly GHG Emissions from BRI Power Projects
ggplot(tech_summary2, aes(x = as.factor(commissioning_year), y = ghg_emission)) +
  geom_bar(stat = "identity", fill = "dark grey") +
  labs(title = "Additional Yearly GHG Emissions from BRI Power Projects",
       x = "Commissioning Year",
       y = "GHG Emission") +
  theme_minimal()



#How many countries with fossil fuel projects?
# Sort by commissioning year
country_summary <- bri %>%
  group_by(country) %>%
  filter(primary_fuel %in% c("coal", "gas", "oil")) %>%
  summarise(n())

###############################################################
# Filter data for the specified years (2000-2023)
debt_summary <- bri %>%
  filter(commissioning_year >= 2000 & commissioning_year <= 2025) %>% 
  group_by(country, primary_fuel) %>%
  summarise(total_investment = sum(total_investment_amount, na.rm = TRUE)) %>%
  ungroup()

# Get the top ten countries with the highest total investment
top_countries <- debt_summary %>%
  group_by(country) %>%
  summarise(total_investment = sum(total_investment, na.rm = TRUE)) %>%
  slice_max(order_by = total_investment, n = 10)

# Plot the bar chart with sky blue color and black outline
ggplot(top_countries, aes(x = reorder(country, -total_investment), y = total_investment)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Top 10 Countries with Highest Investment in Power Sector from BRI (2000-2025)",
       x = "Country",
       y = "Total Debt Investment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#number of power plants by fuel source and investment type from 2000 to 2023
# Filter data for the specified years (2000-2023)


# Filter data for the specified years (2000-2023)
power_plants_summary <- bri %>%
  filter(commissioning_year >= 2000 & commissioning_year <= 2023) %>%
  group_by(primary_fuel, investment_type) %>%
  summarise(number_of_power_plants = n()) %>%
  ungroup()

# Order primary_fuel by total number of power plants in descending order
order_by_power_plants <- power_plants_summary %>%
  group_by(primary_fuel) %>%
  summarise(total_power_plants = sum(number_of_power_plants)) %>%
  arrange(desc(total_power_plants)) %>%
  pull(primary_fuel)

# Plot the stacked bar chart with black borders, ordered by total power plants
ggplot(power_plants_summary, aes(x = reorder(primary_fuel, -number_of_power_plants), 
                                 y = number_of_power_plants, 
                                 fill = investment_type)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.5) +
  geom_bar(stat = "identity", position = "stack", linewidth = 0.5) +
  labs(title = "Number of Power Plants by Fuel Source and Investment Type (2000-2023)",
       x = "Fuel Source",
       y = "Number of Power Plants",
       fill = "Investment Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = order_by_power_plants)+
  scale_fill_brewer(palette = "Set2")  
