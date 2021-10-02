library(corrplot)
library(feather)
library(readr)

#dummy variable
dmy <- dummyVars(" ~ . ", data= small, fullRank = T)
point_bis <- data.frame(predict(dmy, newdata = small))

result <- cor(point_bis)

result.csv <- data.frame(result)

write_csv(result.csv, 'correlation_test.csv')

