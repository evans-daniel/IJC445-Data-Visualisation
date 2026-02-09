source("master.R")
source("funs.R")

# Import Yearly Operator stat Data 

exracted_yr_op_data_list <- read_all_sheets(filename = "raw_XL_data/Extracted Yearly Operator Data XL.xlsx")

# Sheets 1:8 are not in proper format, 9 and 10 (investment) are 

yearly_op_untidy_list <- exracted_yr_op_data_list[1:8]

rail_investment_data_list <- exracted_yr_op_data_list[c(9, 10)]



# Clean 

yearly_op_df <- clean_wide_data(yearly_op_untidy_list) %>% 
  reduce(~ left_join(.x, .y, by = c("year_commencing", "operator"))) %>% 
  mutate(year_commencing = as.numeric(year_commencing))%>% 
  rename("fte" = full_time_equivalent_fte_employees) %>% 
  filter(year_commencing >= 2021 & 
           operator != "Wrexham and Shropshire Railway Company" & # Not in PPM ORR dataset 
           operator != "Lumo") # Started Oct 2021, missing data 



# Merge the investment data frames, then clean these

yearly_investment_df <- reduce(rail_investment_data_list, ~ left_join(.x, .y, b = c("YearCommencing"))) %>% 
  clean_names() %>% 
  arrange(-desc(year_commencing)) %>% 
  mutate(across(everything(), ~ as.character(.x)),
         across(everything(), ~ na_if(na_if(.x, "[z]"), "[z]"))) %>% 
  mutate(across(everything(), ~ as.numeric(.x))) %>% 
  rename_with(.cols = 2:6, ~ paste0("priv_", .x, recycle0 = TRUE)) %>% 
  rename_with(.cols = 7:19, ~ paste0("gov_", .x, recycle0 = TRUE)) %>% 
  dplyr::select(-gov_operational_funding_freight_grants, -gov_total_operational_funding, -gov_enhancements_funding_high_speed_2, -gov_enhancements_funding_east_west_rail, -gov_enhancements_funding_core_valley_lines, -gov_total_enhancements_funding, -gov_total_government_support, -gov_enhancements_funding_crossrail) %>% # Totals are wrong, not looking at freight, HS2, EW Rail, CVL not yet running
  mutate(gov_total_investment = rowSums(across(starts_with("gov")), na.rm = TRUE))




# Calculate a rolling average period for each investment column:

rolling_investment <- yearly_investment_df %>% 
  mutate(across(-1, list(
    roll_1 = ~ rollsum(.x, k = 2, fill = NA, align = "right"),
    roll_3 = ~ rollsum(.x, k = 3, fill = NA, align = "right"),
    roll_5 = ~ rollsum(.x, k = 5, fill = NA, align = "right"),
    roll_10 = ~ rollsum(.x, k = 10, fill = NA, align = "right")),
    .names = "{.col}_{.fn}")) %>% 
  filter(year_commencing >= 2021)


# Combine all yearly data to final yearly df 

final_yearly_df <- left_join(yearly_op_df, rolling_investment, by = "year_commencing") %>% 
  arrange(-desc(year_commencing)) %>% 
  rename_with(.cols = 3:ncol(.), ~ paste0("yrly_", .x, recycle0 = TRUE)) 


###############################################################################

# Import quarterly data and clean:

extracted_q_op_data <- read_all_sheets(filename = "raw_XL_data/Extracted Quarterly Op Data XL.xlsx") # List of tibbles

wide_q_dfs_list <- extracted_q_op_data[c(1, 2, 3)]
q_cancellations <- as.data.frame(extracted_q_op_data[4]) %>%
  clean_names() %>%
  rename_with(~ str_remove(., "df4_"), .cols = everything())

final_quarterly_df <- 
  clean_wide_data(wide_q_dfs_list) %>% 
  reduce(~ left_join(.x, .y, b = c("time_period", "operator"))) %>% 
  left_join(q_cancellations, by = c("time_period", "operator")) %>% 
  mutate(month_year = str_replace(time_period, "^([[:alpha:]]{3}).*([[:digit:]]{4})", "\\1 \\2"),
         full_date = my(month_year),
         yQ = as.yearqtr(full_date),
         year_commencing = lubridate::year(yQ)) %>% 
  dplyr::select(-time_period, - month_year, -full_date) %>% 
  dplyr::select(year_commencing, yQ, everything()) %>% 
  rename("trains_planned" = number_of_trains_planned) %>% 
  filter(year_commencing >= 2021 & 
           operator != "Wrexham and Shropshire Railway Company" & # Not in PPM ORR dataset 
           operator != "Lumo" & # Started Oct 2021, missing data 
           operator != "West Midlands Trains") # Missing 18 rows 


# Join in operators to new variable "region" 

region_operator_list <- list(
  lse = c("c2c", "Chiltern Railways", "Elizabeth line", "Govia Thameslink Railway", "London Overground", "Southeastern", "South Western Railway", "Heathrow Express"),
  longdistance = c("Avanti West Coast", "CrossCountry", "Grand Central", "Hull Trains"),
  regional = c("Caledonian Sleeper", "Merseyrail", "Northern Trains", "ScotRail", "TfW Rail", "TransPennine Express"), 
  multi = c("Greater Anglia", "East Midlands Railway", "Great Western Railway", "London North Eastern Railway")
)

regions_op_df <- enframe(region_operator_list, name = "region", value = "operator") %>% unnest(cols = c(operator))


# Final Full Dataset 

trains_full_data <- left_join(final_quarterly_df, final_yearly_df, by = c("year_commencing", "operator")) %>% 
  left_join(regions_op_df, by = "operator") %>% 
  relocate(region, .after = 3) %>% 
  relocate(ppm, .after = 4) %>% 
  mutate(across(-c(1:5), as.numeric)) %>% 
  filter(year_commencing < 2025) %>% 
  filter(!(operator == "Hull Trains" & 
             yQ == "2021 Q1" & 
             yQ == "2021 Q2")) %>% 
  filter(!(operator == "Grand Central" &
             !yQ == "2021 Q1")) %>% 
  filter(!is.na(trains_planned)) %>% # Dropping Hul Trains and Grand central years, and NA trained planned - not missing at random
  mutate(p_journeys_per_fte = (p_journey_millions/yrly_fte)*1000000,
         p_km_per_fte = (p_km_billions/yrly_fte)*1000000000,
         trains_planned_per_fte = trains_planned/yrly_fte, 
         p_journeys_per_station = (p_journey_millions/yrly_stations_managed)*1000000,
         p_km_per_station = (p_km_billions/yrly_stations_managed)*1000000000,
         trains_planned_per_station = trains_planned/yrly_stations_managed) %>% 
  select(-yrly_passenger_journeys, -yrly_passenger_k_ms, -yrly_cancellations_percentage, -yrly_on_time_percentage)

trains_full_data[sapply(trains_full_data, is.infinite)] <- NA  # marking any inf as NA

trains <- trains_full_data %>% na.omit() %>% 
  mutate(time_id = row_number()) %>% 
  relocate(time_id, .before = 1)