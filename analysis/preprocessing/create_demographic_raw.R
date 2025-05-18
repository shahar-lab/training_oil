process_all_demographics <- function(input_dir = "data/empirical_data/data_collected/demographic",
                                     output_dir = "data/empirical_data/data_raw") {
  library(tidyverse)
  
  # Read and standardize all CSVs
  demographic <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE) %>%
    map_dfr(~ read_csv(.x, show_col_types = FALSE) %>%
              mutate(Age = as.numeric(Age))) %>%
    group_by(`Participant id`) %>%
    filter(Status == "APPROVED") %>%
    ungroup%>%
    rename(prolific_id = `Participant id`) %>%
    select(prolific_id, Sex, Age)
  
  # Summary stats
  age <- mean(demographic$Age, na.rm = TRUE)
  sd_age <- sd(demographic$Age, na.rm = TRUE)
  range_age <- range(demographic$Age, na.rm = TRUE)
  sex <- table(demographic$Sex)
  
  print(glue::glue("Age: M = {round(age, 2)}, SD = {round(sd_age, 2)}, Range = {range_age[1]}–{range_age[2]}"))
  print(sex)
  
  # Save combined demographic file
  save(demographic, file = file.path(output_dir, "demographic.rdata"))
  write.csv(demographic, file = file.path(output_dir, "demographic.csv"), row.names = FALSE)
  
return(demographic)
}


# Run it
demographic=process_all_demographics()
