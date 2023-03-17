### Case Writeup 1 - VIALE DANIELLA

library(stargazer)
library(tidyverse)
library(lfe) 
library(readxl)
library(ggplot2)
library(knitr)
library(scales)
install.packages("RColorBrewer")

file_path <- "/Users/Daniellaviale/Downloads/Champo Carpets (1).xlsx"
champo <- tibble(read_excel(file_path, sheet = "Raw Data-Order and Sample"))


##Summary statistics

#Category
category_stats <- champo %>%
group_by(OrderCategory) %>%
  summarize(Count = n(),
            Mean_Revenue = mean(Amount),
            Median_Revenue = median(Amount),
            Min_Revenue = min(Amount),
            Max_Revenue = max(Amount),
            Std_Dev = sd(Amount),
            IQR = IQR(Amount))
category_stats <- category_stats %>% mutate_if(is.numeric, ~ round(., 4))
category_stats <- category_stats %>%
  mutate_if(is.numeric, ~ format(., big.mark=","))
category_stats <- category_stats %>%
  arrange(desc(Mean_Revenue))
kable(category_stats, "pandoc", align = c("l", "c", "c", "c", "c", "c", "c", "c"),
      col.names = c("Order Category", "Count", "Mean Revenue", "Median Revenue", "Min Revenue", "Max Revenue", "Std. Dev", "IQR"))

#Type
type_stats <- champo %>%
  group_by(OrderCategory, ITEM_NAME) %>%
  summarize(Count = n(),
            Mean = mean(Amount),
            Median = median(Amount),
            Min = min(Amount),
            Max = max(Amount),
            Std_Dev = sd(Amount),
            IQR = IQR(Amount))
type_stats <- type_stats %>% mutate_if(is.numeric, ~ round(., 4))
type_stats <- type_stats %>%
  mutate_if(is.numeric, ~ format(., big.mark=","))
type_stats <- type_stats %>%
  arrange(OrderCategory, desc(Count))
type_stats <- type_stats %>%
  filter(row_number() <= 5)
kable(type_stats, "pandoc", align = c("l", "l", "c", "c", "c", "c", "c", "c", "c"),
      col.names = c("Order Category", "Order Type", "Count", "Mean", "Median", "Min", "Max", "Std. Dev", "IQR"))

#Country
country_stats <- champo %>%
  group_by(OrderCategory, CountryName) %>%
  summarize(Count = n(),
            Mean = mean(Amount),
            Median = median(Amount),
            Min = min(Amount),
            Max = max(Amount),
            Std_Dev = sd(Amount),
            IQR = IQR(Amount))
country_stats <- country_stats %>% mutate_if(is.numeric, ~ round(., 4))
country_stats <- country_stats %>%
  mutate_if(is.numeric, ~ format(., big.mark=","))
country_stats <- country_stats %>%
  arrange(OrderCategory, desc(Count))
country_stats <- country_stats %>%
  filter(row_number() <= 5)
kable(country_stats, "pandoc", align = c("l", "l", "c", "c", "c", "c", "c", "c", "c"),
      col.names = c("Order Category", "Country", "Count", "Mean", "Median", "Min", "Max", "Std. Dev", "IQR"))

#Customer
customer_stats <- champo %>%
  group_by(OrderCategory, CustomerCode) %>%
  summarize(Count = n(),
            Mean = mean(Amount),
            Median = median(Amount),
            Min = min(Amount),
            Max = max(Amount),
            Std_Dev = sd(Amount),
            IQR = IQR(Amount))
customer_stats <- customer_stats %>% mutate_if(is.numeric, ~ round(., 4))
customer_stats <- customer_stats %>%
  mutate_if(is.numeric, ~ format(., big.mark=","))
customer_stats <- customer_stats %>%
  arrange(OrderCategory, desc(Mean))
customer_stats <- customer_stats %>%
  filter(row_number() <= 5)
kable(customer_stats, "pandoc", align = c("l", "l", "c", "c", "c", "c", "c", "c", "c"),
      col.names = c("Order Category", "Customer", "Count", "Mean", "Median", "Min", "Max", "Std. Dev", "IQR"))

#Analysis of revenue
champo %>%
  filter(OrderCategory == "Order") %>%
  group_by(CountryName) %>%
  summarize(Revenue = sum(Amount)) %>%
  arrange(desc(Revenue)) %>%
  mutate(Revenue = comma(Revenue))%>%
  kable(format = "pandoc", digits = 2)

champo%>%
  filter(OrderCategory == "Order") %>%
  group_by(CustomerCode) %>%
  summarize(Revenue = sum(Amount)) %>%
  arrange(desc(Revenue)) %>%
  head(14) %>%
  mutate(Revenue = comma(Revenue))%>%
  kable(format = "pandoc", digits = 2)

champo%>%
  filter(OrderCategory == "Order") %>%
  group_by(ITEM_NAME) %>%
  summarize(Revenue = sum(Amount)) %>%
  arrange(desc(Revenue)) %>%
  head(14) %>%
  mutate(Revenue = comma(Revenue))%>%
  kable(format = "pandoc", digits = 2)


##Conversion data
update.packages()
install.packages("dplyr")
library(dplyr)

champo_conv <-
  champo %>%
  filter(OrderCategory == "Sample") %>% ## get the samples sent out
  left_join(
    champo %>%
      filter(OrderCategory == "Order") ## get the  orders placed
    ,   ## join the two sets together, based on matching of characteristics
    ## and subsequent orders being placed
    join_by(CustomerCode == CustomerCode, 
            ITEM_NAME == ITEM_NAME, 
            DesignName == DesignName,
            ColorName == ColorName,
            QualityName == QualityName,
            Custorderdate<=Custorderdate),
    
    suffix = c("", ".drop") ## Many of the columns in the orders will
    ## be duplicates, so make it easy to drop them
  ) %>%
  mutate(
    conversion = as.numeric(!is.na(OrderType.drop)), ## Code for conversions, i.e.matching order
    OrderType_order = OrderType.drop, ## Create new variables for what we want to keep
    Custorderdate_order = Custorderdate.drop, 
    UnitName_order = UnitName.drop, 
    QtyRequired_order = QtyRequired.drop, 
    TotalArea_order =  TotalArea.drop, 
    Amount_order = Amount.drop, 
    AreaFt_order = AreaFt.drop
  ) %>%
  select(-ends_with(".drop")) ## Drop everything else

#Data Conversion Analysis

conversion <- champo_conv %>%
  group_by(ITEM_NAME) %>%
  summarise(Count_1 = sum(conversion == 1),
            Count_0 = sum(conversion == 0))

colnames(conversion) <- c("Item Name", "Count of Conversion = 1", "Count of Conversion = 0") 
conversion <- conversion %>%
  mutate(conversion_ratio = (`Count of Conversion = 1` / `Count of Conversion = 0`)) 
conversion$`Conversion Ratio` <- percent(conversion$conversion_ratio)  
conversion <- conversion %>%
  select('Item Name', 'Count of Conversion = 1', 'Count of Conversion = 0', `Conversion Ratio`)%>%
  arrange(desc(`Conversion Ratio`))
kable(conversion, format = "pandoc")

champo_conv%>%
  filter(conversion == 1) %>%
  group_by(ITEM_NAME) %>%
  summarize(Revenue = sum(Amount_order)) %>%
  arrange(desc(Revenue)) %>%
  mutate(Revenue = comma(Revenue))%>%
  kable(format = "pandoc", digits = 2)

revenue_sum <- champo_conv %>%
  filter(conversion == 1) %>%
  summarize(Revenue = sum(Amount_order)) %>%
  .$Revenue

print(revenue_sum)

# Filter the data to only include conversions
converted_orders <- champo_conv %>%
  filter(conversion == 1)

revenue_by_item <- converted_orders %>%
  group_by(ITEM_NAME) %>%
  summarize(Revenue = sum(Amount_order))

# Plot the total revenue
ggplot(revenue_by_item, aes(x = ITEM_NAME, y = Revenue)) +
  geom_bar(stat = "identity", fill = "#00CCCC", width = 0.5) +
  labs(x = "Item Name", y = "Total Revenue") +
  ggtitle("Revenue by Item") +
  scale_y_continuous(labels = dollar_format(suffix = "", prefix = "$")) +
  theme(axis.text.x = element_text(size = 8),
plot.title = element_text(hjust = 0.5))


#conversions and customers
conversion_cus <- champo_conv %>%
  group_by(CustomerCode) %>%
  summarise(Count_1 = sum(conversion == 1),
            Count_0 = sum(conversion == 0))
colnames(conversion_cus) <- c("Customer Code", "Count of Conversion = 1", "Count of Conversion = 0") 
conversion_cus <- conversion_cus %>%
  select('Customer Code', 'Count of Conversion = 1', 'Count of Conversion = 0')%>%
  arrange(desc(`Count of Conversion = 0`))
conversion_cus <- conversion_cus %>%
  filter(row_number() <= 10)
kable(conversion_cus, format = "pandoc")

champo_conv%>%
  filter(conversion == 1) %>%
  group_by(CustomerCode) %>%
  summarize(Revenue = sum(Amount_order)) %>%
  arrange(desc(Revenue)) %>%
  mutate(Revenue = comma(Revenue))%>%
  head(12)%>%
  kable(format = "pandoc", digits = 2)

conversion_plot <- champo_conv %>%
  filter(conversion == 1) %>%
  group_by(CustomerCode) %>%
  summarize(Revenue = sum(Amount_order)) %>%
  arrange(desc(Revenue)) %>%
  head(12)
ggplot(conversion_plot, aes(x = CustomerCode, y = Revenue, fill = "#00CCCC")) +
  geom_col(width = 0.7) +
  labs(x = "Customer Code", y = "Revenue", title = "Top 12 Customers by Revenue") +
  scale_y_continuous(labels = dollar_format(suffix = "", prefix = "$")) +
  theme(plot.title = element_text(hjust = 0.5))


distinct_order_sum <- champo_conv %>%
  group_by(CustomerCode) %>%
  summarise(Sum_Distinct_Order = n_distinct(CustomerOrderNo))%>%
  arrange(desc(Sum_Distinct_Order))
distinct_order_sum <- distinct_order_sum %>%
  filter(row_number() <= 10)
kable(distinct_order_sum, format = "pandoc")


# chart customer revenue
revenue_by_cus <- champo_conv %>%
  group_by(CustomerCode) %>%
  summarize(Revenue = sum(Amount_order))

# Plot the total revenue for each item
ggplot(revenue_by_cus, aes(x = CustomerCode, y = Revenue)) +
  geom_bar(stat = "identity", fill = "#00CCCC", width = 0.5) +
  labs(x = "Customer Code", y = "Total Revenue") +
  ggtitle("Revenue by Item") +
  scale_y_continuous(labels = dollar_format(suffix = "", prefix = "$")) +
  theme(axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5))

