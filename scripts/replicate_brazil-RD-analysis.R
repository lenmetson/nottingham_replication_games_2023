


df <- haven::read_dta(here::here("original_materials", "brazil-RD-data (1).dta")) # download manually downloaded stata file 

# Automatically downloaded

file.rename(here::here("original_materials", "brazil-RD-data.tab"),
here::here("original_materials", "brazil-RD-data_renamed.dta"))

df2 <- haven::read_dta(here::here("original_materials", "brazil-RD-data_renamed.dta"))

summary(df)
summary(df2)

# We see they're the same, but really populated by NAs