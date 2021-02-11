library(readxl)
library(writexl)
library(stringr)
library(dplyr)

xl <<- read_excel("C:/Users/djiang/Downloads/2020 07 17 YTD.xlsx") #change to local location of sale data .xlsx

top_ten <- function(){ #pulls top 10 bestsellers then populates .xlsx files of lists of commonly purchsed satandards
                      #to a local directory
  most_common <- sort(table(xl$ProductId), decreasing=TRUE)[1:10] #up to ten for google optimize max number of pages
  for (items in 1:nrow(most_common)){
    
    standard_input <- trimws(items)
    customers <- filter(xl, ProductId == standard_input)
    total_orders <- data.frame(OrderNumber = character())
    other_ordered_standards <- data.frame(Owner_GUID = character(), OrderNumber = character(), ProductId = character())
    
    for(orders in 1:nrow(customers)){
      order_id <- customers[orders,]$OrderNumber
      temp <- filter(xl, OrderNumber == order_id)
      other_ordered_standards <- rbind(other_ordered_standards, temp)
    }
    
    other_standards_bought <- filter(other_ordered_standards, ProductId != standard_input)
    occurances <- table(unlist(other_standards_bought$ProductId))
    occurances_df <- as.data.frame(occurances)
    occurances_ordered <- arrange(occurances_df, desc(Freq)) #end goal data frame
    filename_clean <- str_remove_all(standard_input, "[(): /]")
    filepath <- paste("C:/Users/djiang/relevantstandardscarousel/Jul 2020/", filename_clean, '.xlsx', sep = "") #change to local folder
    tmp <- write_xlsx(occurances_ordered, filepath)
  }
}

find_relevant <- function(SKU){ #finds single standard's relevant standards from SKU
  customers <- filter(xl, ProductId == SKU)
  total_orders <- data.frame(OrderNumber = character())
  other_ordered_standards <- data.frame(Owner_GUID = character(), OrderNumber = character(), ProductId = character())
  
  for(orders in 1:nrow(customers)){
    order_id <- customers[orders,]$OrderNumber
    temp <- filter(xl, OrderNumber == order_id)
    other_ordered_standards <- rbind(other_ordered_standards, temp)
  }
  
  other_standards_bought <- filter(other_ordered_standards, ProductId != standard_input)
  occurances <- table(unlist(other_standards_bought$ProductId))
  occurances_df <- as.data.frame(occurances)
  occurances_ordered <- arrange(occurances_df, desc(Freq)) #end goal data frame
  top_3_ordered <- head(occurances_ordered, 3) #top 3 most relevant standards
  filename_clean <- str_remove_all(standard_input, "[(): /]")
  filepath <- paste("C:/Users/djiang/relevantstandardscarousel/Jul 2020/", filename_clean, '.xlsx', sep = "") #change to local folder
  tmp <- write_xlsx(occurances_ordered, filepath)
}

top_ten()
find_relevant("ISO 9001:2015") #input as string
