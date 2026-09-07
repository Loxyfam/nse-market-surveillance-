library(dplyr)
library(zoo)

generate_surveillance_alerts <- function(data, window = 20, z_thresh = 3.0) {
  data %>%
    arrange(Ticker, Date) %>%
    group_by(Ticker) %>%
    mutate(
      Log_Return = log(Close / lag(Close)),
      
      # Rolling Volume Metrics
      Vol_Mean = rollmean(Volume, k = window, fill = NA, align = "right"),
      Vol_Std  = rollapply(Volume, width = window, FUN = sd, fill = NA, align = "right"),
      Volume_Z_Score = (Volume - Vol_Mean) / Vol_Std,
      
      # Bollinger Bands
      Price_SMA = rollmean(Close, k = window, fill = NA, align = "right"),
      Price_Std = rollapply(Close, width = window, FUN = sd, fill = NA, align = "right"),
      Upper_Band = Price_SMA + (2.5 * Price_Std),
      Lower_Band = Price_SMA - (2.5 * Price_Std),
      
      # Compliance Flags
      Flag_Volume = ifelse(Volume_Z_Score > z_thresh, TRUE, FALSE),
      Flag_Price  = ifelse(Close > Upper_Band | Close < Lower_Band, TRUE, FALSE),
      Compliance_Alert = Flag_Volume & Flag_Price
    ) %>%
    ungroup()
}
