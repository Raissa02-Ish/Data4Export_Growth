#Setting Prophet 
Category_Export<- read_xlsx("www/Category_Export_2020.xlsx")
Category_Export<- Category_Export %>%
  pivot_longer(
    cols = -Category,
    names_to = "Quarter",
    values_to = "Export_Value"
  )
#Changing to Date format
Category_Export <- Category_Export %>%
  mutate(ds = case_when(
    str_detect(Quarter, "Q1") ~ paste0(substr(Quarter, 1, 4), "-01-01"),
    str_detect(Quarter, "Q2") ~ paste0(substr(Quarter, 1, 4), "-04-01"),
    str_detect(Quarter, "Q3") ~ paste0(substr(Quarter, 1, 4), "-07-01"),
    str_detect(Quarter, "Q4") ~ paste0(substr(Quarter, 1, 4), "-10-01")
  )) %>%
  mutate(ds = as.POSIXct(ds, tz = "UTC"))
#Using prophet
# Forecast function for one category
forecast_category <- function(df, category_name) {
  df_cat <- df %>%
    filter(Category == category_name) %>%
    select(ds, y = Export_Value) %>%
    mutate(ds = as.POSIXct(ds, tz = "UTC"))  # ✅ enforce tz
  
  m <- prophet(df_cat, n.changepoints = 5)
  future <- make_future_dataframe(m, periods = 6, freq = "quarter")
  forecast <- predict(m, future)
  
  forecast %>%
    select(ds, yhat) %>%
    mutate(
      ds = as.POSIXct(ds, tz = "UTC"),  # ✅ ensure same tz for join/mutate
      Category = category_name,
      type = ifelse(ds <= max(df_cat$ds), "Observed", "Forecast")
    )
}
# Apply to all categories
categories <- unique(Category_Export$Category)

forecast_df <- map_df(categories, ~ forecast_category(Category_Export, .x))
#Selecting top 5
top5_Categories <- Category_Export %>%
  group_by(Category) %>%
  summarise(total = sum(Export_Value)) %>%
  arrange(desc(total)) %>%
  slice(1:5) %>%
  pull(Category)

plot_df <- forecast_df %>% filter(Category %in% top5_Categories)
forecast_only <- plot_df %>%
  filter(type == "Forecast")
#Formatting plot_df
plot_df <- plot_df %>%
  mutate(
    ds = as.character(format(ds, "%Y-Q%q")),  # convert quarters to string labels
    yhat = round(yhat, 2),                    # round values to 2 decimals
    type = factor(type, levels = c("Observed", "Forecast"))
  )

write.csv(plot_df, "plot_df.csv")
