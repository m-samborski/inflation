library(tidyverse)

inflation_data <- read_csv("API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_4701153.csv", 
                           skip = 4, name_repair = "universal")

clean_data <- inflation_data %>%
  pivot_longer(cols = 5:67, names_to = "Year", values_to = "Inflation.Rate", values_drop_na = TRUE) %>%
  select(Country.Name, Year, Inflation.Rate) %>% mutate(Year = as.numeric(str_sub(Year,4,7)))


compare_inflation <- function(country1, country2, start_year = 1960, end_year = 2021) {
  #chart <- clean_data %>% filter(Year >= start_year & Year <=end_year) %>% 
    #filter(Country.Name %in% c(country1, country2)) %>% 
    #ggplot(aes(Year, Inflation.Rate, colour = Country.Name)) +
    #geom_smooth(method = loess, formula = y~x) + scale_y_log10()
  
  cov_data <-  clean_data %>% filter(Year >= start_year & Year <= end_year) %>% 
    filter(Country.Name %in% c(country1, country2)) %>% 
    pivot_wider(names_from = Country.Name, values_from = Inflation.Rate) %>%
    drop_na(c(country1, country2)) %>%
    mutate(covariance = (pull(.,var = 2)-mean(pull(.,var = 2)))*
             (pull(.,var = 3)-mean(pull(.,var = 3))))
  covarianceab <- mean(cov_data$covariance)/(sd(pull(cov_data,2))*
                                               sd(pull(cov_data,3)))
  print(covarianceab)
  #print(chart)
}


compare_inflation("Poland", "Brazil", 1995)

countries <- unique(clean_data$Country.Name)

corelations <- sapply(countries, compare_inflation, country2 = "Poland", start_year = 2000)

create_correlation_table <- function(country, start = 1960, end = 2021){
  countries <- clean_data %>% filter(Year >= start & Year <= end) %>% 
    filter(Country.Name !=country)
  countries <- unique(countries$Country.Name) 
  corelations <- sapply(countries, compare_inflation, country2 = country, start_year = start, 
                        end_year = end)
  correlation_table <- tibble(countries, corelations)
  correlation_table <- drop_na(correlation_table, corelations)
  print(correlation_table)
}

table_poland <- create_correlation_table("Poland", start = 2000)

which.max(table_poland$corelations)
which.min(table_poland$corelations)



