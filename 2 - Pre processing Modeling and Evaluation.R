library(arules)
library(arulesViz)
library(readr)
library(reshape2)

tr <-
  read.transactions("C:/Users/user/Downloads/ElectronidexTransactions2017.csv", sep = ",")

# Our first step will be to understand the data:
# we have been given data for a month of Electronidex online sales (9835 transactions). 
# Electronidex has a range of 125 products splitted into 17 product types.

itemLabels(tr)
length (tr)

# Looking at the transactions we can already see that we have very big purchases 
# (half of the purchases include more than 4 products, which is not usual for individual 
# customers). We can already think we will have 2 client types: B2B and B2C.

itemsbought <- sort(size(tr), decreasing = T)
table(itemsbought)
summary(itemsbought)
hist(itemsbought, breaks = 30)


# As a 2nd check, we look at the rules this dataset would give us, and we already see that 6 out 
# of our top 8 rules imply buying 2 computers or 2 printers together, which is definetly not 
# common for individual customers.

itemFrequencyPlot(tr, type = "absolute", topN = 10, horiz = F)
sort(itemFrequency(tr, type = "absolute"), decreasing = T)

RulesName<- apriori (tr, parameter = list(supp = 0.05, conf = 0.1, minlen = 2))
inspect(sort(RulesName, by = "support"))
 
# therefore, we'll first split the dataset in B2B = 5 or more products, B2C = 4 products or less:

tr.df <- 
  read_csv("C:/Users/user/Downloads/ElectronidexTransactions2017.csv", col_names = FALSE)

tr.df$itemsbought <- (32-rowSums(is.na(tr.df)))

# and we'll include laptop and desktop information for the split:

product_key <-
  read_delim("~/producttypekey.csv", ";", escape_double = FALSE, trim_ws = TRUE)

product_key[(which(product_key == "Desktop")-125),]
product_key[(which(product_key == "Laptops")-125),]

desktop_or_laptop <- rbind(product_key[(which(product_key == "Desktop")-125),], 
                           product_key[(which(product_key == "Laptops")-125),])

computercount <- c()

for (i in 1:nrow(tr.df)) {
  
  hola <- sum(tr.df[i, ] %in% desktop_or_laptop$`product name`)
  computercount <- rbind(computercount, hola)
  
}

computercount

tr.df <- cbind(tr.df, computercount)

# printer:

printers <- product_key[(which(product_key == "Printers")-125),]

printerscount <- c()

for (i in 1:nrow(tr.df)) {
  
  hola <- sum(tr.df[i, ] %in% printers$`product name`)
  printerscount <- rbind(printerscount, hola)
  
}

printerscount

tr.df <- cbind(tr.df, printerscount)

# however once we split the datasets we see that some customers that buy more than 4 products 
# are buying gaming products, which is not a common behaviour for a company, so we'll include 
# that in our split as well. Also, out of these gaming customers we can see that a lot of them
# buy a computer specific for gaming and a normal one on the same purchase, so we'll separate
# them from the normal B2C into a Gaming category:


gaming <- c()

for (i in 1:nrow(tr.df)) {
  
  hola <- sum(grepl("Gam", tr.df[i, ], ))
  gaming <- rbind(gaming, hola)
  
}

gaming

tr.df <- cbind(tr.df, gaming)


# Now we identified what makes a customer B2B, B2C or gamer, we can split the data in 3 datasets:

tr.df$customer_type <- ifelse(
  tr.df$gaming > 0, "Gamer", ifelse(
    tr.df$computercount > 1, "B2B", ifelse(
      tr.df$printerscount > 1, "B2B", ifelse(
        tr.df$itemsbought > 4, "B2B", "B2C"
    ))))

Gamer_transactions <- tr[which(tr.df$customer_type == "Gamer"),]
B2B_transactions <- tr[which(tr.df$customer_type == "B2B"),]
B2C_transactions <- tr[which(tr.df$customer_type == "B2C"),]

length (B2B_transactions)
length (B2C_transactions)
length (Gamer_transactions)


# Now we have the dataset split in B2B, Gamer and B2C we will insepct both again:

## B2B:

itemsbought <- sort(size(B2B_transactions), decreasing = T)
table(itemsbought)
summary(itemsbought)
hist(itemsbought, breaks = 30)

itemFrequencyPlot(B2B_transactions, type = "absolute", topN = 10, horiz = F)
sort(itemFrequency(B2B_transactions, type = "absolute"), decreasing = T)

B2BRulesName<- apriori (B2B_transactions, parameter = list(supp = 0.046, conf = 0.4, minlen = 2))
inspect(sort(B2BRulesName, by = "lift"))

## Gamer:

itemsbought <- sort(size(Gamer_transactions), decreasing = T)
table(itemsbought)
summary(itemsbought)
hist(itemsbought, breaks = 30)

itemFrequencyPlot(Gamer_transactions, type = "absolute", topN = 10, horiz = F)
sort(itemFrequency(Gamer_transactions, type = "absolute"), decreasing = T)

GamerRulesName<- apriori (Gamer_transactions, parameter = list(supp = 0.05, conf = 0.5, minlen = 2))
inspect(sort(GamerRulesName, by = "lift"))

## B2C:

itemsbought <- sort(size(B2C_transactions), decreasing = T)
table(itemsbought)
summary(itemsbought)
hist(itemsbought, breaks = 30)

itemFrequencyPlot(B2C_transactions, type = "absolute", topN = 10, horiz = F)
sort(itemFrequency(B2C_transactions, type = "absolute"), decreasing = T)

B2CRulesName<- apriori (B2C_transactions, parameter = list(supp = 0.0075, conf = 0.1, minlen = 2))
inspect(sort(B2CRulesName, by = "lift"))

# Rules:

inspect(sort(B2BRulesName, by = "lift"))
inspect(sort(GamerRulesName, by = "lift"))
inspect(sort(B2CRulesName, by = "lift"))
