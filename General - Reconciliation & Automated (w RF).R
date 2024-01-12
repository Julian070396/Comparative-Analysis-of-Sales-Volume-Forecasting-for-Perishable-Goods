library(fabletools) #  Provides tools, helpers and data structures for developing models and time series functions for 'fable' and extension packages.
library(fable) # provides common forecasting methods for tsibble, such as naive, ARIMA and ETS.
library(tsibble) # provides a data infrastructure for tidy temporal data with wrangling tools.
library(feasts) # provides support for visualizing data and extracting time series features.
library(slider)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra) # for arranging multi-panel plots
library(rlang)
library(readr)  # for library (rlang) saving csv
library(scales) # for changing time x-axis format
library(openxlsx)
library(stringr)

# Step 1: Get dataset with proper time index

general <- data.frame(read.csv("inputs/General_Level.csv", stringsAsFactors = FALSE))

# Transform the data

transformed_data <- general %>%
  select(SKU = SKU.Nbr, Sales, Sales_Qty = Sales.Qty, Date) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) # Convert Date to YYYY-MM-DD format

# Convert to a tsibble and handle missing data
data_ts <- transformed_data %>%
  as_tsibble(index = Date, key = SKU) %>%
  fill_gaps(.full = TRUE) %>%  # Fills gaps in the time series
  mutate(Sales = replace_na(Sales, 0),  # Replace NA in Sales with 0
         Sales_Qty = replace_na(Sales_Qty, 0))  # Replace NA in Sales_Qty with 0

# Step 2: Split data into training and validation sets

# Split data into training and validation sets
train_data <- data_ts %>%
  filter(Date < as.Date("2023-01-01"))

validation_data <- data_ts %>%
  filter(Date >= as.Date("2023-01-01"))

# Calculate the number of unique dates in the validation set
h <- validation_data %>%
  distinct(Date) %>%  # Select distinct dates
  tally() %>%  # Count the number of rows (unique dates)
  .$n  # Extract the count

# 3. Forecasting Directly at SKU Level

sku_channel_fit <- train_data %>%
  model(
    naive = NAIVE(Sales_Qty),
    snaive = SNAIVE(Sales_Qty ~ lag(52)),
    ets = ETS(Sales_Qty),
    reg = TSLM(Sales_Qty ~ trend() + season(52)),
    fourier_reg_26 = TSLM(Sales_Qty ~ trend() + fourier(period = 52, K = 26)),
    fourier_reg_13 = TSLM(Sales_Qty ~ trend() + fourier(period = 52, K = 13)),
    fourier_reg_4 = TSLM(Sales_Qty ~ trend() + fourier(period = 52, K = 4)),
    arima = ARIMA(Sales_Qty)
  )

#Roll Forward Forecast
#linear_fc <- sku_channel_fit %>% forecast(h = h)

# Initialize an empty dataframe to store results
linear_fc <- data.frame()

# Loop over unique SKUs
unique_skus <- unique(data_ts$SKU)

for (sku in unique_skus) {
  # Filter data for the current SKU
  sku_data <- data_ts %>% filter(SKU == sku)
  
  # Calculate the training period size for the current SKU
  training_period_size <- train_data %>% filter(SKU == sku)
  
  # Stretch the tsibble for the current SKU with the calculated .init
  sku_stretch <- stretch_tsibble(sku_data, .init = nrow(training_period_size), .step = 1)
  
  # Estimate a model for the current SKU
  fit.roll <- sku_stretch |>
    model(
      naive = NAIVE(Sales_Qty),
      snaive = SNAIVE(Sales_Qty ~ lag(52)),
      ets = ETS(Sales_Qty),
      reg = TSLM(Sales_Qty ~ trend() + season(52)),
      fourier_reg_26 = TSLM(Sales_Qty ~ trend() + fourier(period = 52, K = 26)),
      fourier_reg_13 = TSLM(Sales_Qty ~ trend() + fourier(period = 52, K = 13)),
      fourier_reg_4 = TSLM(Sales_Qty ~ trend() + fourier(period = 52, K = 4)),
      arima = ARIMA(Sales_Qty)
    )
  
  # Generate forecasts for the current SKU
  fc.roll <- fit.roll |>
    forecast(h = 1) %>%
    mutate(SKU = sku)  # Add SKU column to the forecasts
  
  # Append the forecasts for the current SKU to the linear_fc dataframe
  linear_fc <- bind_rows(linear_fc, fc.roll)
}

x <- validation_data %>%
  distinct(Date) %>%  # Select distinct dates
  tally() %>%  # Count the number of rows (unique dates)
  .$n  # Extract the count

linear_fc <- linear_fc %>% filter(.id <= x)

# Delete the .id column
linear_fc  <- select(linear_fc, -starts_with(".id"))

# Get training errors using augment()

augmented_train_linear <- augment(sku_channel_fit)

#Convert to data frames and get one dataset for Linear Aggregation

#Training Period "General-Training"

augmented_train_linear_df <- as_tibble(augmented_train_linear)

augmented_train_linear_df <- augmented_train_linear_df %>%
  filter(!is.na(.fitted)) %>%  # Drop rows where .fitted is NA
  mutate(Status = "General-Training")  # Add a new column Status with value "Linear-Training"

augmented_train_linear_df <- augmented_train_linear_df %>%
  group_by(SKU, .model, Date, Status) %>%
  summarize(Sales_Qty = sum(Sales_Qty), 
            forecast = sum(.fitted), 
            error = sum(.resid), #In case that is .innov here I can replace the value
            .groups = 'drop') # Drop the grouping structure after summarizing

augmented_train_linear_df <- augmented_train_linear_df %>%
  left_join(transformed_data, by = c("SKU", "Date"))%>%
  rename(Sales_Qty = Sales_Qty.x) %>%
  select(-Sales_Qty.y)

#Valid Period "General-Validation"

linear_fc_df <- as_tibble(linear_fc)

linear_fc_df <- linear_fc_df %>%
  filter(!is.na(.mean)) %>%  # Drop rows where .mean is NA
  mutate(Status = "General-Validation")  # Add a new column Status with value "Linear-Training"

linear_fc_df <- linear_fc_df %>%
  group_by(SKU, .model, Date, Status) %>%
  summarize(forecast = sum(.mean), 
            .groups = 'drop') # Drop the grouping structure after summarizing

#Getting the Sales_Qty and calculate error

linear_fc_df <- linear_fc_df %>%
  left_join(transformed_data, by = c("SKU", "Date"))%>%
  mutate(error = Sales_Qty - forecast)

# Now Combine the datasets

final_df <- bind_rows(linear_fc_df, augmented_train_linear_df) %>%
  mutate(Method = "General")%>%
  rename(Sales_Retailer = Sales)

### START CALCULATING THE COST FUNCTION

#Calculating Price Retail

final_df <- final_df %>%
  mutate(Price_Retail = ifelse(Sales_Qty != 0, Sales_Retailer / Sales_Qty, 0))

#Setting Margin Retailer

final_df <- final_df %>%
  mutate(Margin_Retailer = .2)  # Replace with the value you want

#Setting Price Supplier that will be the cost for the retailer 

final_df <- final_df %>%
  mutate(`Price_Supplier(Cost Retailer)` = Price_Retail  - (Price_Retail  * Margin_Retailer))

#Setting Margin Supplier

final_df <- final_df %>%
  mutate(Margin_Supplier = .15)  # Replace with the value you want


#Setting Cost of the supplier based on the margin that they put

final_df <- final_df %>%
  mutate(`Cost Supplier` = `Price_Supplier(Cost Retailer)`  - (`Price_Supplier(Cost Retailer)`  * Margin_Supplier))

#Conditional Over or Under

final_df <- final_df %>%
  mutate(Under_Over = ifelse(error >= 0, "Under", "Over"))

#Supplier Perspective

final_df <- final_df %>%
  mutate(Perspective_supplier = case_when(
    Under_Over == "Over" ~ `Price_Supplier(Cost Retailer)` * error,
    Under_Over == "Under" ~ (`Price_Supplier(Cost Retailer)` - `Cost Supplier`) * error,
    TRUE ~ 0  # Default to 0 in case of any other values or errors
  ))


#Retailer Perspective

final_df <- final_df %>%
  mutate(Perspective_retailer = case_when(
    Under_Over == "Over" ~ 0,
    Under_Over == "Under" ~ (Price_Retail - `Price_Supplier(Cost Retailer)`) * error,
    TRUE ~ 0  # Default to 0 in case of any other values or errors
  ))

# Dropping rows with NA in Sales_Retailer
final_df <- final_df %>%
  filter(!is.na(Sales_Retailer))

final_df_General <- final_df %>%
  mutate(Level = "General_Level")  # Add a new column Status with value "Linear-Training"

#Plot the charts

# Assuming your dataframe is named final_df
unique_skus <- unique(final_df$SKU)

for(sku in unique_skus) {
  subset_data <- final_df %>%
    filter(SKU == sku) %>%
    mutate(Date = as.Date(Date))  # Ensure Date is in the correct format
  
  plot <- ggplot(subset_data, aes(x = Date)) +
    geom_line(aes(y = Sales_Qty, colour = "Actual Sales")) +
    geom_line(aes(y = forecast, colour = "Forecast"), linetype = "dashed", size = 0.75) +
    geom_vline(xintercept = as.numeric(as.Date("2023-01-01")), linetype = "longdash", color = "grey") +
    facet_grid(.model ~ Method) +  # Creates a separate plot for each model and method combination
    ggtitle(paste("SKU:", sku)) +
    xlab("Date") +
    ylab("Quantity") +
    labs(colour = "Legend") +  # Custom legend title
    scale_color_manual(values = c("Actual Sales" = "blue", "Forecast" = "salmon"))  # Custom colors
  
  print(plot)
}

# Saving the dataframe as a CSV file

write.csv(final_df_General, "final_df_General.csv", row.names = FALSE)
