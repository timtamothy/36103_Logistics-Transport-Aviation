# 1. # 1. Load your packages. These were used: ----
library(tidyverse)
library(here)
library(future.apply)
library(dplyr)
library(jsonlite)
library(data.table)
library(feather)
library(janitor)

# 2. Load the file names (Put all your files you want to merge in one folder) ----
                # (then point the path to that folder)
files <- list.files(path = here('Folder Name'), full.names = TRUE)


# 3. Load the files inside the folder one bye one. ----
output <- files %>% 
  map_df(~{
    fread(.x)
     })

View(output)

# 3a. Load your original dataset ----
maindf <- read_feather(here('file.feather'))


# 4. Make a new feather of all collated data ----
write_feather(output, here('file2.feather'))



# 5. Join your new dataset with a different one ----
joined_df <- left_join(maindf, output, by = c('column_from_maindf', 'column_from_output'))



# 6. Check your new dataframe ----
status(joined_df)

# 6a. Can you remove Na values here?
joined_df <- drop_na(joined_df)



# 7. Save your newly joined file ----
write_feather(joined_df, here('joinedfile.feather'))

