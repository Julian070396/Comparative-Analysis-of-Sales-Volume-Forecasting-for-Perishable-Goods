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

general <- read.csv("inputs/Weekdays_Weekend_Level.csv", stringsAsFactors = FALSE)

data_pl <- general %>%
  rename(Week = Week.and.Year, 
         SKU = SKU.Nbr, 
         Purchase_Channel = Weekdays_Weekend, 
         Date = Date, 
         Sales_Qty = Sales.Qty) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"), # Change date format
         Week = as.integer(str_sub(Week, -2))) # Extract week number

#Data aggregate to General Level

data_gl <- data_pl %>%
  group_by(SKU,Date) %>%
  summarize(Sales_Qty = sum(Sales_Qty),
            Sales = sum(Sales),
            .groups = 'drop') # Drop the grouping structure after summarizing

# Convert to a tsibble and handle missing data
data_pl_ts <- data_pl %>%
  as_tsibble(index = Date, key = c(SKU, Purchase_Channel)) %>%
  fill_gaps(.full = TRUE) %>%  # Fills gaps in the time series
  mutate(Sales = replace_na(Sales, 0),  # Replace NA in Sales with 0
         Sales_Qty = replace_na(Sales_Qty, 0))  # Replace NA in Sales_Qty with 0


# Step 2: Split data into training and validation sets

# Split data into training and validation sets
train_data <- data_pl_ts %>%
  filter(Date < as.Date("2023-01-01"))

validation_data <- data_pl_ts %>%
  filter(Date >= as.Date("2023-01-01"))

# Calculate the number of unique dates in the validation set
h <- validation_data %>%
  distinct(Date) %>%  # Select distinct dates
  tally() %>%  # Count the number of rows (unique dates)
  .$n  # Extract the count

# 3. Forecasting at SKU/Purchase Channel Level with Linear Aggregation

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

# Initialize an empty dataframe to store results
linear_fc <- data.frame()

# Loop over unique combinations of SKUs and Purchase Channels
unique_combinations <- unique(data_pl_ts[, c("SKU", "Purchase_Channel")])

for (i in seq_len(nrow(unique_combinations))) {
  current_combination <- unique_combinations[i, ]
  sku <- current_combination$SKU
  channel <- current_combination$Purchase_Channel
  
  # Filter data for the current SKU and Purchase Channel
  sku_channel_data <- data_pl_ts %>% filter(SKU == sku, Purchase_Channel == channel)
  
  # Calculate the training period size for the current combination
  training_period_size <- train_data %>% filter(SKU == sku, Purchase_Channel == channel)
  
  # Stretch the tsibble for the current combination with the calculated .init
  sku_channel_stretch <- stretch_tsibble(sku_channel_data, .init = nrow(training_period_size), .step = 1)
  
  # Estimate a model for the current combination
  fit.roll <- sku_channel_stretch |>
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
  
  # Generate forecasts for the current combination
  fc.roll <- fit.roll |>
    forecast(h = 1) %>%
    mutate(SKU = sku, Purchase_Channel = channel)  # Add SKU and Purchase Channel columns to the forecasts
  
  # Append the forecasts for the current combination to the linear_fc dataframe
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

#Training Period "Linear-Training"

augmented_train_linear_df <- as_tibble(augmented_train_linear)

###HERE I REPLICATE THE SERIE TO USE LATER FOR "Reconciliation-Training"

augmented_train_reconciliation_df <- augmented_train_linear_df %>%
  filter(!is.na(.fitted)) %>%  # Drop rows where .fitted is NA
  mutate(Status = "Reconciliation-Training")  # Add a new column Status with value "Linear-Training"

######

#We continue with the flow
augmented_train_linear_df <- augmented_train_linear_df %>%
  filter(!is.na(.fitted)) %>%  # Drop rows where .fitted is NA
  mutate(Status = "Linear-Training")  # Add a new column Status with value "Linear-Training"

augmented_train_linear_df <- augmented_train_linear_df %>%
  group_by(SKU, .model, Date, Status) %>%
  summarize(Sales_Qty = sum(Sales_Qty), 
            forecast = sum(.fitted), 
            error = sum(.resid), #In case that is .innov here I can replace the value
            .groups = 'drop') # Drop the grouping structure after summarizing

augmented_train_linear_df <- augmented_train_linear_df %>%
  left_join(data_gl, by = c("SKU", "Date"))%>%
  rename(Sales_Qty = Sales_Qty.x) %>%
  select(-Sales_Qty.y)


#Valid Period "Linear - Validation"

linear_fc_df <- as_tibble(linear_fc)

linear_fc_df <- linear_fc_df %>%
  filter(!is.na(.mean)) %>%  # Drop rows where .mean is NA
  mutate(Status = "Linear-Validation")  # Add a new column Status with value "Linear-Training"

linear_fc_df <- linear_fc_df %>%
  group_by(SKU, .model, Date, Status) %>%
  summarize(forecast = sum(.mean), 
            .groups = 'drop') # Drop the grouping structure after summarizing

#Getting the Sales_Qty and calculate error

linear_fc_df <- linear_fc_df %>%
  left_join(data_gl, by = c("SKU", "Date"))%>%
  mutate(error = Sales_Qty - forecast)

# Now Combine the datasets

linear_results_purchase <- bind_rows(linear_fc_df, augmented_train_linear_df) %>%
  mutate(Method = "Linear")


# 4. Forecasting at SKU/Purchase Channel Level with Reconciliation

# Reconciliation - Validation

# Define a subset of reconciliation methods

reconciliation_methods <- c("wls_var", "mint_cov", "mint_shrink","ols", "wls_struct")

# Prepare the dataset for reconciliation
data_agg <- aggregate_key(train_data, SKU / Purchase_Channel, Sales_Qty = sum(Sales_Qty))
fit_agg <- data_agg %>%
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

## Loop through each reconciliation method with error handling
reconciled_forecasts_list <- list()
for (method in reconciliation_methods) {
  try({
    reconciled_fit <- fit_agg %>%
      reconcile(
        naive = min_trace(naive, method = method),
        snaive = min_trace(snaive, method = method),
        ets = min_trace(ets, method = method),
        reg = min_trace(reg, method = method),
        arima = min_trace(arima, method = method)
      )
    reconciled_forecasts <- reconciled_fit %>% forecast(h = h)
    reconciled_forecasts_list[[method]] <- reconciled_forecasts
  }, silent = TRUE)
}

# Initialize an empty dataframe
combined_forecasts_df <- data.frame()

# Loop through the list and combine
for (method in names(reconciled_forecasts_list)) {
  # Add a column for the method name
  method_df <- as_tibble(reconciled_forecasts_list[[method]]) %>%
    mutate(Method = method)
  
  # Combine with the main dataframe
  combined_forecasts_df <- bind_rows(combined_forecasts_df, method_df)
}

# Assuming your dataframe is named combined_forecasts_df
combined_forecasts_df <- combined_forecasts_df %>%
  filter(Purchase_Channel != "<aggregated>")%>%
  mutate(.mean = if_else(.mean < 0, 0, .mean))  %>%
  select(-Sales_Qty) %>%  # Drop the Sales_Qty column
  rename(forecast = .mean)%>%  # Rename .mean to Qty # Set negative .mean values to 0
  mutate(Status = "Reconciliation-Validation")  # Add a new column Status with value "Linear-Training"
  
#Grouping the values
reconciliation_fc_df <- combined_forecasts_df %>%
  group_by(SKU, .model, Date, Status, Method) %>%
  summarize(forecast = sum(forecast), 
            .groups = 'drop') # Drop the grouping structure after summarizing

#Getting the Sales_Qty and calculate error

# Convert SKU in reconciliation_fc_df to integer
reconciliation_fc_df <- reconciliation_fc_df %>%
  mutate(SKU = as.integer(as.character(SKU)))

reconciliation_fc_df <- reconciliation_fc_df %>%
  left_join(data_gl, by = c("SKU", "Date"))%>%
  mutate(error = Sales_Qty - forecast)

#HERE I WILL GET THE TRAINING PERIOD AND AGGREGATION WILL BE WITH THE DISTRIBUTION GOTTEN IN THE FORECAST


##Getting the average per purchase channel/model based on the distribution of the valid period

# First, calculate the total forecast for each .model and SKU
total_forecast_per_model_sku <- combined_forecasts_df %>%
  group_by(SKU, .model, Method) %>%
  summarise(Total_Forecast_Model_SKU = sum(forecast))

# Now, join this back to the original data and calculate Avg Weight
weight <- combined_forecasts_df %>%
  left_join(total_forecast_per_model_sku, by = c("SKU", ".model", "Method")) %>%
  group_by(SKU, .model, Purchase_Channel, Method) %>%
  summarise(
    total_forecast_Model_SKU_PL = sum(forecast), # Average of forecasts for each group
    Total_Forecast_Model_SKU = unique(Total_Forecast_Model_SKU) # Total forecast for the model and SKU
  ) %>%
  mutate(Avg_Weight = total_forecast_Model_SKU_PL / Total_Forecast_Model_SKU)


#Here I get back the serie that I created before for the reconciliation

augmented_train_reconciliation_df

#Now I Create the matrix that brings the different reconciliations with all the values of training

# Ensure SKU columns are of the same type (character)
augmented_train_reconciliation_df$SKU <- as.character(augmented_train_reconciliation_df$SKU)
weight$SKU <- as.character(weight$SKU)
reconciliation_fc_df$SKU <- as.character(reconciliation_fc_df$SKU)

# Using left_join to keep all rows from augmented_train_reconciliation_df and bring Avg_Weight
augmented_train_reconciliation_df <- augmented_train_reconciliation_df %>%
  left_join(weight, by = c("SKU", ".model", "Purchase_Channel"))


# Step 1: Total Aggregation Fitted
augmented_train_reconciliation_df <- augmented_train_reconciliation_df %>%
  group_by(SKU, .model, Date, Method) %>%
  mutate(Total_Aggregation_Fitted = sum(.fitted)) %>%
  ungroup()

# Step 2: Share_fitted
augmented_train_reconciliation_df <- augmented_train_reconciliation_df %>%
  mutate(Share_fitted = ifelse(Total_Aggregation_Fitted == 0, 0, .fitted / Total_Aggregation_Fitted))

# Step 3: Same Base Difference
augmented_train_reconciliation_df <- augmented_train_reconciliation_df %>%
  #mutate(Same_Base_Difference = ifelse(Share_fitted == 0, 0, (Avg_Weight / Share_fitted) - 1)) #Here Only chance the distribution
  mutate(Same_Base_Difference = ifelse(Share_fitted == 0, 0, Avg_Weight - Share_fitted)) #Here I calculate new base

# Step 4: Fitted_Extra
augmented_train_reconciliation_df <- augmented_train_reconciliation_df %>%
  mutate(Fitted_Extra = Same_Base_Difference * .fitted)

# Step 5: fitted_weight
augmented_train_reconciliation_df <- augmented_train_reconciliation_df %>%
  mutate(fitted_weight = .fitted + Fitted_Extra)


# Aggregating the dataset
reconciliation_training_df <- augmented_train_reconciliation_df %>%
  group_by(SKU, .model, Date, Status, Method) %>%
  summarize(
    forecast = sum(fitted_weight, na.rm = TRUE),
    Sales_Qty = sum(Sales_Qty, na.rm = TRUE),
    error = Sales_Qty - forecast,
    Sales = 0 # Setting Sales to NA
  )


reconciliation_results_purchase <- bind_rows(reconciliation_training_df, reconciliation_fc_df)


#Creating a final data set

linear_results_purchase #This one is the final dataset for linear aggregation (Considering Train and Validation)
linear_results_purchase$SKU <- as.character(linear_results_purchase$SKU)

reconciliation_results_purchase #Here comes the reconciliation methods applying for each model only for validation


final_df <- bind_rows(linear_results_purchase, reconciliation_results_purchase)

# Joining the datasets

data_gl$SKU <- as.character(data_gl$SKU)

final_df <- final_df %>%
  left_join(data_gl, by = c("SKU", "Date")) %>%
  rename(Sales_Qty = Sales_Qty.y, Sales_Retailer = Sales.y) %>%
  select(-Sales_Qty.x, -Sales.x)


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

final_df_WWL <- final_df %>%
  mutate(Level = "Weekdays_Weekend_Level")  # Add a new column Status with value "Linear-Training"

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

write.csv(final_df_WWL, "final_df_WWL.csv", row.names = FALSE)
