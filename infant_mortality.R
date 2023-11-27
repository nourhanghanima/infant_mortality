library(readr)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(readxl)



mortality_data <- read_csv("/Users/nourhanghanima/downloads/T1.csv", skip = 1)

#tidying up data
mortality_data <- mortality_data |>
                  rename(Country = "Country Name") |>
                  pivot_longer("1960":"2021", names_to = "year", values_to = "mortality_rate")

mortality_data$year <- as.numeric(mortality_data$year)

#including a column for continent 
mortality_data <- mortality_data |>
  mutate(Continent = case_when(
    Country %in% c("Algeria", "Angola",  "Benin", "Botswana", "Burkina Faso",
                   "Burundi","Cabo Verde/Cape Verde","Cameroon", "Central African Republic",
                   "Chad","Comoros", "Congo", "Democratic Republic of the Congo", "Djibouti",
                   "Egypt", "Equatorial Guinea", "Eritrea","Eswatini", "Ethiopia","Gabon","Gambia",
                   "Ghana", "Guinea", "Guinea-Bissau","Ivory Coast", "Kenya", "Lesotho", "Liberia",
                   "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco",
                   "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Brunei Darussalam", "Sao Tome and Principe",
                   "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan",
                   "Sudan", "Tanzania", "Togo", "Tunisia","Uganda", "Zambia", "Zimbabwe", "Cote d'Ivoire", "Congo, Rep.",
                   "Cabo Verde", "Egypt, Arab Rep.", "Gambia, The") ~ "Africa",
   Country %in% c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei",
                 "Cambodia", "China", "East Timor", "Georgia", "India", "Indonesia", "Iran", "Iraq",
                 "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgygstan", "Laos", "Lebanon",
                 "Malaysia", "Maldives", "Mongolia", "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan",
                 "Palestine", "Philippines", "Qatar", "Russia", "Vietnam", "Saudi Arabia", "Singapore",
                 "South Korea", "Sri Lanka", "Syria", "Syrian Arab Republic", "Taiwan", "Tajikistan", "Thailand", "Turkey",
                 "Turkiye","Turkmenistan", "United Arab Emirates", "Uzbekistan", "Yemen", "Korea, Rep.") ~ "Asia",
  Country %in% c("Albania", "Andorra", "Austria", "Belarus","Belgium", "Bosnia and Herzegovina", "Bulgaria",
                 "Croatia", "Cyprus","Czech Republic","Denmark", "Estonia", "Finland", "France", "Georgia",
                 "Germany","Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein",
                 "Lithuania", "Luxembourg", "North Macedonia", "Malta", "Moldova", "Monaco", "Montenegro", 
                 "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia",
                 "Slovakia", "Slovenia", "Spain","Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom",
                 "Vatican City") ~ "Europe",
  Country %in% c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Canada", "Costa Rica", 
                 "Cuba", "Dominica", "Dominican Republic", "El Salvador", "Grenada", "Guatemala", 
                 "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Saint Kitts and Necis",
                 "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad and Tobago", "United States", 
                 "St. Lucia", "Turks and Caicos Islands", "St. Vincent and the Grenadines", "British Virgin Islands") ~ "North America",
  Country %in% c("Argentina", "Bolivia","Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", 
              "Peru", "Suriname", "Trinidad and Tobago", "Uruguay", "Venezuela", "Venezuela, RB" ) ~ "South America",
  Country %in% c("Australia", "Fiji", "Kiribati", "Marshall Islands", "Federated States of Micronesia",
                 "Nauru", "New Zealand","Palau",  "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga",
                 "Tuvalu", "Vanuatu", "Micronesia, Fed. Sts.") ~ "Oceania"
  ))


#Creating plot of infant mortality by year, showing a separate line graph for each country 

mortality_plot_countries <- mortality_data |> 
  ggplot( aes(x=year, y=mortality_rate, group=Country, color = Continent)) +
  geom_line() + 
  theme_bw () +
  xlab("Years") +
  ylab("Mortality Rate Per 1000 Live Births")
  scale_x_continuous(breaks = seq(1960, 2020, 5), limits = c(1960, 2020)) 
mortality_plot_countries



mortality_plot_continents <- mortality_data |> 
  filter(year %in% c(1960, 2000, 2020)) |>
  ggplot(aes(year, mortality_rate, col = Continent)) +
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks = seq(1960, 2020, 30), limits=c(1960, 2020)) +
  facet_grid(year~Continent)
mortality_plot_continents

mortality_data$mortality_rate <- as.numeric(mortality_data$mortality_rate)

mortality_data <- mortality_data |> 
  filter(Country %in% c("Sierra Leone","Finland","Japan","Norway", "Singapore")) |>
  mutate(label = Country) |>
  
  mortality_data |>
  ggplot(aes(x = year, y = mortality_rate, group = Country, color = Country)) +
  geom_line() 


gdp_data <- read_csv("/Users/nourhanghanima/downloads/GDP Data World Bank/fc317218-35b2-4fa4-a763-e06db0661e80_Data.csv", show_col_types = FALSE)
gdp_data <- gdp_data |>
  pivot_longer("1990 [YR1990]": "2022 [YR2022]", names_to = "year", values_to = "GDP") |>
  rename(Country = "Country Name")

gdp_data$year <- as.numeric(substr(gdp_data$year, 0, 4))

public_expenditure <- read_excel("/Users/nourhanghanima/downloads/imf-dm-export-20231122.xls")
public_expenditure <- public_expenditure |>
  mutate(across(2:223, as.numeric))

public_expenditure <- public_expenditure |>
  mutate(across(where(is.character), ~na_if(., "no data"))) |>
  pivot_longer("1800": "2021", names_to = "year", values_to="public_spending") |>
  rename(Country = "Government expenditure, percent of GDP (% of GDP)") 
public_expenditure$year <- as.numeric(public_expenditure$year)

fertility_rate <- read_csv("/Users/nourhanghanima/downloads/fertility_data.csv", show_col_types = FALSE)
fertility_rate <- fertility_rate |>
  pivot_longer("1960 [YR1960]": "2022 [YR2022]", names_to = "year", values_to = "fertility_rate") |>
  rename(Country = "Country Name")
fertility_rate$year  <- as.numeric(substr(fertility_rate$year, 0, 4))

hiv_data <- read_csv("/Users/nourhanghanima/downloads/hiv_data.csv")
hiv_data <- hiv_data |>
  mutate(across(where(is.character), ~na_if(., ".."))) |>
  pivot_longer("1960 [YR1960]": "2022 [YR2022]", names_to = "year", values_to = "hiv_rate") |>
  rename(Country = "Country Name")
hiv_data$year <- as.numeric(substr(hiv_data$year, 0, 4))

list_df = list(mortality_data,hiv_data,public_expenditure, fertility_rate, gdp_data)
df2 <- list_df %>% reduce(inner_join, by=c("Country", "year"))
df2 <- df2 |>
  select(Country, year, GDP, hiv_rate, fertility_rate, public_spending, mortality_rate) |>
  drop_na()
df2 <- df2 |>
  mutate(across(3:7, as.numeric))

mortality_model <- lm(mortality_rate ~ hiv_rate + fertility_rate + GDP + public_spending, data= df2)
summary(mortality_model)

plot(mortality_model)


