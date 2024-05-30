library(tidyverse)
library(FinCal)


# The following provides a function to calculate rental and homeownership affordability.

localities_df <- data.frame(
  locality = c("Locality1", "Locality2", "Locality3"),
  median_income = c(60000, 75000, 50000)
)

calc_affordable_rent <- function(data, input_col){
  
  data |> 
    mutate(affordable_rent := (0.3*!!sym(input_col))/12)
}


# Create a function that calculates an affordable home price based on income and three
# different down payment scenarios.

calc_affordable_sales <- function(data, input_col, dwn_opts1, dwn_opts2, dwn_opts3){
  
  int_rate <- 0.0694 # U.S. WEEKLY AVG AS OF 05.23.2024
  dwn_1 <- 1-dwn_opts1
  dwn_2<- 1-dwn_opts2
  dwn_3 <- 1-dwn_opts3
  
  
  # Create dynamic column names based on dwn_opts values
  colname1 <- paste0("affordable_sales_", dwn_opts1)
  colname2 <- paste0("affordable_sales_", dwn_opts2)
  colname3 <- paste0("affordable_sales_", dwn_opts3)
  
  data %>%
    mutate(third = !!sym(input_col) * 0.28, # Lender preference for 28% of household income
           ho_monthly = (third / 12) - 250) %>%  # Assume $250 monthly tax and insurance
    mutate(!!sym(colname1) := abs(pv(int_rate / 12, 360, 0, ho_monthly, 0)) * dwn_1,
           !!sym(colname2) := abs(pv(int_rate / 12, 360, 0, ho_monthly, 0)) * dwn_2,
           !!sym(colname3) := abs(pv(int_rate / 12, 360, 0, ho_monthly, 0)) * dwn_3)
}


localities_df <- calc_affordable_rent(localities_df, "median_income")

localities_df <- calc_affordable_sales(localities_df, "median_income", 0.03, 0.10, 0.20)
