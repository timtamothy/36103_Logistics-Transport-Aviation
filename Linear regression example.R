mlm_final <- read_feather("C:/Users/tsetc/Downloads/mlm_dataset_3.feather")

#filter variables/ subset it 


#Trim data + min-max transformation 
mlm_final$id <- seq.int(nrow(mlm_final))

num_col <- select_if(mlm_final, is.numeric)

num_col <- num_col %>% drop_na()

#Optional for trim data 
for (i in 1:9) {
  if (max(num_col[,i]) > (mean(unlist(num_col[,i])) + 4* sd(unlist(num_col[,i])))) {
    num_col <- num_col %>% arrange(num_col[,i])
    num_col <- num_col[round(nrow(num_col) *0.01, digits = 0):round(nrow(num_col)* (1-0.01) , digits = 0),]
  }
}

min_max_norm <- function(x){
  (x-min(x)) / (max(x) - min(x))
}
num_col_norm <- as.data.frame(future_lapply(num_col, min_max_norm))

cat_col <- mlm_final %>% select_if(negate(is.numeric))
cat_col$id <- seq.int(nrow(mlm_final))

mlm_final <- left_join(num_col_norm, cat_col, by = 'id')

#dummy variable 
dmy <- dummyVars(" ~ . ", data=mlm_final, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = mlm_final))

#fit data 
mlm1 <- lm(dep_delay ~ ., data = dat_transformed)
