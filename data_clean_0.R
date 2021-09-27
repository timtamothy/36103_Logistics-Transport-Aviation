# The data clean workflow

# 1. Load your packages. These were used: ----
library(tidyverse)
library(feather)
library(here)
library(janitor)
library(future.apply)

# 2. Load your data. Load your feather file. ----
df <- read_feather(here('file.feather'))


# 3. Clean your column names ----
clean_df <- clean_names(df)

  # check your columns
    colnames(clean_df)
    status(clean_df)
    
# 4. Remove unused columns ----
    
clean_df_2 <- clean_df %>% 
  select('columns', 'you',
         'want', 'to',
         'keep')

  # check your columns
    colnames(clean_df_2)
    status(clean_df_2)
    
  # Status showed you your column types!
  # So,

# 5. Clean your column types. If numeric is wrong, change to factor ----
    
wrong_columns <- c('this', 'is', 'a', 'list', 'of',
                   'wrong', 'columns')

clean_df_2[wrong_columns] <- future_lapply(clean_df_2[wrong_columns], factor)
  #this is the code:
  #columns that are wrong <- change type to (wrong columns, factor function)

  #check your columns
  status(clean_df_2)
  
  
# 6. If you want to, remove Na values ----

clean_df_2 <- drop_na(clean_df_2)
  

# 7. If you need to, separate columns with functions ---- 
clean_df_2_separated_columns <- clean_df_2 %>% 
  separate(column_name, c('new', 'columns'))

  
# 8. Write your new clean column to save as a new feather file ----
write_feather(clean_df_2, 'path')