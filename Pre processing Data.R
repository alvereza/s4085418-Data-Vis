
library(dplyr)
library(tidyr)

data <- read.csv("Life Expectancy Data.csv", stringsAsFactors = FALSE)

names(data) <- gsub(" ", "", names(data))
names(data) <- gsub("-", "", names(data))
names(data) <- gsub("/", "", names(data))
names(data) <- tolower(names(data))


# Check for missing population data
data$population_missing <- is.na(data$population) | data$population == 0

# Calculate infant mortality rate (per 1000 live births)
data$infant_mortality_rate <- ifelse(
  !is.na(data$infantdeaths) & !is.na(data$population) & data$population > 0,
  (data$infantdeaths / data$population) * 1000,
  NA
)

# Calculate under-5 mortality rate (per 1000 live births)
data$under5_mortality_rate <- ifelse(
  !is.na(data$underfivedeaths) & !is.na(data$population) & data$population > 0,
  (data$underfivedeaths / data$population) * 1000,
  NA
)

# Calculate health expenditure per capita
data$health_exp_per_capita <- ifelse(
  !is.na(data$percentageexpenditure) & !is.na(data$gdp) & data$gdp > 0,
  (data$percentageexpenditure / 100) * data$gdp,
  NA
)

# Normalize key indicators and create composite score
data <- data %>%
  mutate(
    # Normalize each indicator to 0-1 scale
    norm_life_exp = (lifeexpectancy - min(lifeexpectancy, na.rm = TRUE)) / 
      (max(lifeexpectancy, na.rm = TRUE) - min(lifeexpectancy, na.rm = TRUE)),
    norm_gdp = (gdp - min(gdp, na.rm = TRUE)) / 
      (max(gdp, na.rm = TRUE) - min(gdp, na.rm = TRUE)),
    norm_schooling = (schooling - min(schooling, na.rm = TRUE)) / 
      (max(schooling, na.rm = TRUE) - min(schooling, na.rm = TRUE)),
    norm_income_comp = (incomecompositionofresources - min(incomecompositionofresources, na.rm = TRUE)) / 
      (max(incomecompositionofresources, na.rm = TRUE) - min(incomecompositionofresources, na.rm = TRUE)),
    
    # Calculate development score as average of normalized indicators
    development_score = rowMeans(
      cbind(norm_life_exp, norm_gdp, norm_schooling, norm_income_comp), 
      na.rm = TRUE
    )
  ) %>%
  select(-starts_with("norm_")) 


# BMI categories based on WHO standards
data$bmi_category <- cut(
  data$bmi,
  breaks = c(-Inf, 18.5, 25, 30, Inf),
  labels = c("Underweight", "Normal", "Overweight", "Obese"),
  include.lowest = TRUE
)
data$bmi_category[is.na(data$bmi)] <- "Unknown"

# Year groups for temporal analysis
data$year_group <- cut(
  data$year,
  breaks = c(1999, 2005, 2010, 2015),
  labels = c("2000-2005", "2006-2010", "2011-2015"),
  include.lowest = TRUE
)

# Life expectancy categories
data$life_exp_category <- cut(
  data$lifeexpectancy,
  breaks = c(-Inf, 60, 70, 80, Inf),
  labels = c("Low (<60)", "Medium (60-70)", "High (70-80)", "Very High (>80)"),
  include.lowest = TRUE
)

# Create a simplified region mapping based on country names
data$region <- case_when(
  data$country %in% c("United States of America", "Canada", "Mexico", 
                      "Brazil", "Argentina", "Chile", "Colombia", "Peru", 
                      "Venezuela", "Ecuador", "Bolivia", "Paraguay", "Uruguay",
                      "Guatemala", "Cuba", "Haiti", "Dominican Republic", 
                      "El Salvador", "Honduras", "Nicaragua", "Costa Rica", 
                      "Panama", "Jamaica", "Trinidad and Tobago", "Barbados",
                      "Saint Lucia", "Saint Vincent and the Grenadines", 
                      "Grenada", "Antigua and Barbuda", "Guyana", "Suriname",
                      "Belize", "Bahamas") ~ "Americas",
  
  data$country %in% c("United Kingdom", "Germany", "France", "Italy", 
                      "Spain", "Poland", "Romania", "Netherlands", "Belgium",
                      "Greece", "Portugal", "Czech Republic", "Hungary", 
                      "Sweden", "Austria", "Switzerland", "Bulgaria", "Denmark",
                      "Finland", "Slovakia", "Norway", "Ireland", "Croatia",
                      "Bosnia and Herzegovina", "Albania", "Lithuania", "Slovenia",
                      "Latvia", "Estonia", "Macedonia", "Moldova", "Luxembourg",
                      "Malta", "Iceland", "Montenegro", "Serbia", "Cyprus") ~ "Europe",
  
  data$country %in% c("China", "India", "Indonesia", "Pakistan", "Bangladesh",
                      "Japan", "Philippines", "Vietnam", "Turkey", "Iran",
                      "Thailand", "Myanmar", "South Korea", "Iraq", "Afghanistan",
                      "Saudi Arabia", "Uzbekistan", "Malaysia", "Nepal", "Yemen",
                      "North Korea", "Sri Lanka", "Cambodia", "Jordan", "Azerbaijan",
                      "United Arab Emirates", "Tajikistan", "Israel", "Laos",
                      "Lebanon", "Singapore", "Oman", "Kuwait", "Georgia", 
                      "Mongolia", "Armenia", "Qatar", "Bahrain", "Timor-Leste",
                      "Maldives", "Brunei", "Bhutan") ~ "Asia",
  
  data$country %in% c("Nigeria", "Ethiopia", "Egypt", "Democratic Republic of Congo",
                      "South Africa", "Kenya", "Uganda", "Algeria", "Sudan",
                      "Morocco", "Angola", "Ghana", "Mozambique", "Madagascar",
                      "Cameroon", "Ivory Coast", "Niger", "Burkina Faso", "Mali",
                      "Malawi", "Zambia", "Senegal", "Somalia", "Chad", "Zimbabwe",
                      "Guinea", "Rwanda", "Benin", "Tunisia", "Burundi", "South Sudan",
                      "Togo", "Sierra Leone", "Libya", "Liberia", "Mauritania",
                      "Eritrea", "Gambia", "Botswana", "Namibia", "Gabon",
                      "Lesotho", "Guinea-Bissau", "Equatorial Guinea", "Mauritius",
                      "Swaziland", "Djibouti", "Comoros", "Cape Verde", 
                      "Sao Tome and Principe", "Seychelles") ~ "Africa",
  
  data$country %in% c("Australia", "Papua New Guinea", "New Zealand", "Fiji",
                      "Solomon Islands", "Vanuatu", "Samoa", "Kiribati",
                      "Micronesia", "Tonga", "Palau", "Cook Islands", "Nauru",
                      "Tuvalu", "Marshall Islands") ~ "Oceania",
  
  TRUE ~ "Other"
)

# Ensure numeric columns are properly typed
numeric_cols <- c("lifeexpectancy", "adultmortality", "infantdeaths", "alcohol",
                  "percentageexpenditure", "hepatitisb", "measles", "bmi",
                  "underfivedeaths", "polio", "totalexpenditure", "diphtheria",
                  "hivaids", "gdp", "population", "thinness119years", 
                  "thinness59years", "incomecompositionofresources", "schooling",
                  "infant_mortality_rate", "under5_mortality_rate", 
                  "health_exp_per_capita", "development_score")

for(col in numeric_cols) {
  if(col %in% names(data)) {
    data[[col]] <- as.numeric(data[[col]])
  }
}

# Ensure integer columns
data$adultmortality <- as.integer(data$adultmortality)
data$infantdeaths <- as.integer(data$infantdeaths)
data$measles <- as.integer(data$measles)
data$underfivedeaths <- as.integer(data$underfivedeaths)


data <- data %>%
  select(
    country, year, status, lifeexpectancy, adultmortality, infantdeaths,
    alcohol, percentageexpenditure, hepatitisb, measles, bmi, underfivedeaths,
    polio, totalexpenditure, diphtheria, hivaids, gdp, population,
    thinness119years, thinness59years, incomecompositionofresources, schooling,
    population_missing, infant_mortality_rate, under5_mortality_rate,
    health_exp_per_capita, development_score, bmi_category, year_group,
    life_exp_category, region
  )

write.csv(data, "lifeexpectancy_processed.csv", row.names = FALSE)
