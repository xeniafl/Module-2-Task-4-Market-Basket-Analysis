## Comparing Blackwell and Electronidex Sales:

# According to 

library(arules)
library(ggplot2)
library(readr)

Blackwell <- read_csv("C:/Users/user/Downloads/existingproductattributes2017.csv")

Electronidex.tr <- 
  read.transactions("C:/Users/user/Downloads/ElectronidexTransactions2017.csv", sep = ",")

ProductTypeKey <-
  read_delim("~/producttypekey_Blackwell product types.csv", ";")

# First of all, we want to see how big in volume is Electronidex compared to Blackwell:

sum(Blackwell$Volume)
length(Electronidex.tr)
comparison <- data.frame("company" = c("Blackwell", "Electronidex"), 
                         "volume" = c(sum(Blackwell$Volume), length(Electronidex.tr)))

ggplot(comparison, aes(x = company, y = volume, fill = company)) + geom_col()

# Thanks to this graph we can see that Blackwell is way bigger than ELectronidex, 
# between 5 and 6x:

# Margin:

Blackwell$ProductProfit <- (Blackwell$Price * Blackwell$ProfitMargin)
ggplot(Blackwell, aes(x = ProductType, y = ProductProfit, fill = ProductType)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Volume per category:

ggplot(Blackwell, aes(x = ProductType, y = Volume, fill = ProductType)) + geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

volume <- sort(itemFrequency(Electronidex.tr, type = "absolute"), decreasing = T)

Electronidex <- as.data.frame(volume)
Electronidex$'product name' <- row.names(Electronidex)

Electronidex_wProducttype <- merge(Electronidex, ProductTypeKey)
Electronidex_wProducttype <- Electronidex_wProducttype[,(c(3, 1, 2))]
colnames(Electronidex_wProducttype) <- c("ProductType", "ProductName", "Volume")
ggplot(Electronidex_wProducttype, aes(x = ProductType, y = Volume, fill = ProductType)) + 
    geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

Blackwell$Company <- rep("Blackwell", 80)
Electronidex_wProducttype$Company <- rep("Electronidex", 125)


ggplot() +
  geom_col(data = Electronidex_wProducttype, aes(x = ProductType, y = Volume, fill = ProductType)) +
  geom_col(data = Blackwell, aes(x = ProductType, y = Volume, fill = ProductType)) +
  facet_grid(Company~.) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


