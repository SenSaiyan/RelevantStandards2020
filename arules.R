library(readxl);
library(writexl);
library(stringr);
library(plyr);
library(dplyr);
library(arules);
library(RColorBrewer);
library(splitstackshape);

xl <<- read_excel("C:/Users/djiang/relevantstandardscarousel/2019 export parsed values.xlsx"); #change to local location
xl <- xl[order(xl$Owner_GUID),]; #order by user making purchases, even across different orders
transData <- ddply(xl,c("Owner_GUID"), function(df1)paste(df1$ProductId, collapse=","));
transDataMultiple <- transData[grepl(",", transData$V1),];
temp <- write.csv(transDataMultiple,"C:/Users/djiang/relevantstandardscarousel/basket.csv", row.names = FALSE);

# https://support.office.com/en-us/article/split-text-into-different-columns-with-the-convert-text-to-columns-wizard-30b14928-5550-41f5-97ca-7a3e9c363ed7
# split V1 into multiple columns by comma
# remove first row
# remove owner_guid column
txn <- read.transactions(file="C:/Users/djiang/relevantstandardscarousel/basket.csv", format = "basket", sep=",");
# data("Groceries")
rules <- apriori(txn, parameter = list(support = 0.01, confidence = 0.5))
 