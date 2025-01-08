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


filtered_data <- charges[charges$record_date >= "2021-08-01" & charges$record_date <= "2022-08-31", ]
filtered_data$year_month <- format(filtered_data$record_date, "%Y-%m")
library(dplyr)

monthly_sum <- filtered_data %>%
  group_by(year_month) %>%
  summarise(monthly_total = sum(amt, na.rm = TRUE))

# View the result
pie(monthly_sum$monthly_total,monthly_sum$year_month )

#plot
plot(monthly_sum$monthly_total)


charges$amt <- ifelse(charges$amt < 0, abs(charges$amt), charges$amt)

charges$record_date <- as.Date(charges$record_date, format = "%Y-%m-%d")  # Adjust format if needed
august_data <- charges[format(charges$record_date, "%m") == "08", ]
august_amt <- august_data$amt
summary(august_amt)




