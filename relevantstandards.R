library(readxl)
library(writexl)
library(stringr)
library(dplyr)
library(data.table)
library(tidyverse)

top_ten <- function(xl, local_dir){ #pulls top 10 bestsellers
  #most_common <- sort(table(xl$ProductId))
  most_common <- arrange(xl, ProductId)
  most_common_freq <- setDT(most_common)[,freq := .N, by = c("ProductId")] #https://stackoverflow.com/questions/35036182/sort-a-dataframe-column-by-the-frequency-of-occurrence
  most_common_freq <- arrange(most_common_freq, freq)
  most_common_freq <- most_common_freq %>% map_df(rev)
  most_common_freq <- distinct(most_common_freq, ProductId)
  ten_most_common <- head(most_common_freq, 10)
  return(ten_most_common)
  
  #return_df <- as.data.frame(matrix(nrow=30, ncol=4))
  #colnames(return_df) <- c("")
  # for (items in 1:nrow(ten_most_common)){
  #   print(find_relevant(xl, local_dir, ten_most_common$ProductId[items]))
  # }
  #return(return_df)
}

find_relevant <- function(xl, local_dir, SKU){ #finds single standard's relevant standards from SKU
  customers <- filter(xl, ProductId == SKU)
  other_ordered_standards <- data.frame(Owner_GUID = character(), OrderNumber = character(), ProductId = character())
  print(customers)
  for(orders in 1:nrow(customers)){
    #print(orders)
    order_id <- customers[orders,]$OrderNumber
    #print(order_id)
    temp <- filter(xl, OrderNumber == order_id)
    #print(temp)
    other_ordered_standards <- rbind(other_ordered_standards, temp)
  }
  
  other_standards_bought <- filter(other_ordered_standards, ProductId != SKU)
  occurances <- table(unlist(other_standards_bought$ProductId))
  occurances_df <- as.data.frame(occurances)
  occurances_ordered <- arrange(occurances_df, desc(Freq)) #end goal data frame
  top_3_ordered <- head(occurances_ordered, 3) #top 3 most relevant standards
  filename_clean <- str_remove_all(SKU, "[(): /]")
  filepath <- paste(local_dir, filename_clean, '.xlsx', sep = "")
  #tmp <- write_xlsx(occurances_ordered, filepath)
  return(top_3_ordered)
}

xl <<- read_excel("C:/Users/light/RelevantStandards2020/2019 export parsed values.xlsx") #change to local location
                                                                                         #of sale data .xlsx
local_dir <- "C:/Users/light/relevantstandards2020/Feb 2021/"
SKU <- "AIAG MSA-4:2010"

top_ten(xl, local_dir) #xl is data frame, local_dir is string
find_relevant(xl, local_dir, SKU) #input as string
