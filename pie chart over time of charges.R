install.packages("DBI")
install.packages("RPostgres")
install.packages("ggplot2")
library(ggplot2)
library(DBI)
library(RPostgres)

library(ggplot2)
install.packages("tidyr")
library(tidyr)


revenue_long <- revenue %>%
  gather(key = "transaction_type", value = "value", inflow, outflow)

ggplot(revenue_long, aes(x = transaction_description, y = value, fill = transaction_type)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Inflow and Outflow by Transaction Description", x = "Transaction Description", y = "Amount")

#-----------------------Revenue Charges---------------------
charges <- read.csv('/Users/tshmacm1172/Desktop/ABSA_PROJECT/charges.csv', sep = ',')
charges$amt <- ifelse(charges$amt < 0, abs(charges$amt), charges$amt)

filtered_data$record_date <- as.Date(filtered_data$record_date)
filtered_data$record_date <- as.Date(filtered_data$record_date, format="%d/%m/%Y")
filtered_data <- charges[charges$record_date >= "2021-08-01" & charges$record_date <= "2022-08-31", ]
filtered_data$record_date <- as.POSIXct(filtered_data$record_date)

filtered_data$year_month <- format(filtered_data$record_date, "%Y-%m")
#install.packages("dplyr")
library(dplyr)

monthly_sum <- filtered_data %>%
  group_by(year_month) %>%
  summarise(monthly_total = sum(amt, na.rm = TRUE))

# View the result
pie(monthly_sum$monthly_total,monthly_sum$year_month, main="Total charges 
    Received by ABSA per Month for Financial Period of 2021/2022" )

#plot
plot( monthly_sum$monthly_total)
library(ggplot2)
library(lubridate)
ggplot(monthly_sum, aes(x = year_month, y = monthly_total)) +
  geom_line(color = "blue", size = 1) +  # Line plot
  geom_point(color = "red", size = 3) +  # Add points on the line
  labs(title = "Monthly Transaction Total from July 2021 to August 2022",
       x = "Month",
       y = "Total Amount") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9),  # Rotate x-axis labels
        plot.title = element_text(hjust = 0.5),  # Center title
        axis.title = element_text(size = 12))  # Adjust axis title size


# Bar plot
barplot(monthly_sum$monthly_total, names.arg = monthly_sum$year_month, las = 2)

# Time series plot
plot(monthly_sum$year_month, monthly_sum$monthly_total, type = "b", xlab = "Year-Month", ylab = "Total")












