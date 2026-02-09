# Function to read excel file names, output in list

read_all_sheets <- function(filename){
  all_sheets <- excel_sheets(filename)
  output_list <- map(all_sheets, ~ as.tibble(read_xlsx(filename, sheet = .x)))
  names(output_list) <- paste0("df", seq_along(output_list))
  output_list
}


# Function to clean the list of data files and save as a list of clean dfs

clean_wide_data <- function(df_list){
  map(df_list, function(df){
    measure_name <- make_clean_names(paste0(df[[1,2]]))
    df %>% 
      mutate(across(everything(), ~ as.character(.x)),
             across(everything(), ~ na_if(na_if(.x, "[z]"), "[x]"))) %>% 
      pivot_longer(cols = 3:ncol(df), names_to = "operator", values_to = measure_name) %>% 
      clean_names() %>% 
      dplyr::select(-measure) %>% 
      mutate(across(all_of(measure_name), as.numeric))
  }
  )
}

# All fonts in plots

set_fonts <- function(){
  theme(plot.title = element_text(face = "bold", family = "Times New Roman", size = 14),
        plot.subtitle = element_text(face = "italic", family = "Times New Roman", size = 10),
        legend.title = element_text(face = "bold", size = 8, family = "Times New Roman"),
        legend.text = element_text(size = 6, family = "Times New Roman"),
        axis.title = element_text( size = 12, family = "Times New Roman"),
        axis.text.x = element_text(size = 10, family = "Times New Roman"),
        axis.text.y = element_text(size = 10, family = "Times New Roman"),
        strip.text = element_text(size = 10, family = "Times New Roman"))
}

# Extra option to ensure full decimals are displayed
options(scipen = 999)
