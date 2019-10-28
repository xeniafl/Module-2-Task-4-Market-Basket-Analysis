# hola hola!

# PLAN OF ATTACK STEP 2: INSTALL, UPLOAD AND GET TO KNOW YOUR DATA SET

library(arules)
library(arulesViz)
library(readr)
library(reshape2)

tr <- 
  read.transactions("C:/Users/user/Downloads/ElectronidexTransactions2017.csv", sep = ",")

x <- as(tr, "matrix")

a <- t(x)

colnames(a)

product_key <- read_delim("~/producttypekey.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)


hola1 <- merge(a, product_key, by.x = "", by.y = "product name", all.x = TRUE)




########






inspect(electronidex_transactions)
itemLabels(electronidex_transactions)
LIST(electronidex_transactions)
length (electronidex_transactions)
size(electronidex_transactions)
itemsbought <- sort(size(electronidex_transactions), decreasing = T)
table(itemsbought)
hist(itemsbought, breaks = 30)

electronidex_transactions[order(size(electronidex_transactions))]
sorted <- electronidex_transactions[order(size(electronidex_transactions))]
B2B <- sorted[1:1000]
B2C <- sorted[]


# PLAN OF ATTACK STEP 3: VISUALIZE YOUR DATA SET

itemFrequency(electronidex_transactions, type = "absolute")
itemFrequencyPlot(electronidex_transactions, type = "absolute", topN = 10, horiz = F)
image(electronidex_transactions)

set.seed(243)
image(sample(electronidex_transactions, 100))

# PLAN OF ATTACK STEP 4: APPLY THE APRIORI ALGORITHM

RulesName<- apriori (electronidex_transactions, parameter = list(supp = 0.01, conf = 0.5, minlen = 1))

# PLAN OF ATTACK STEP 5: EVALUATE YOUR MODEL

summary(RulesName)
inspect(RulesName)

# PLAN OF ATTACK STEP 6: IMPROVE YOUR MODEL

inspect(sort(RulesName, by = "support"))

# ItemRules <- subset(RulesName, items %in% "iMac")

is.redundant(RulesName)

# PLAN OF ATTACK STEP 7: VISUALIZE YOUR RESULTS

plot(RulesName, engine = "interactive")
plot(RulesName, method="graph", control=list(type="items"), engine = "interactive") 

