install.packages("readxl")
library("readxl")
my_data <- read_excel("/Users/Henry/Desktop/Project_2/Data/XLS/df_time_dataset.xls")
my_data
names(my_data)

model <- lm( `Search AD`~ `Referral Volumn`+`Search Volumn`+`Direct Volumn`+is_weekend,data = my_data)
summary(model)$coefficient
summary(model)
coef(model)
confint(model)

