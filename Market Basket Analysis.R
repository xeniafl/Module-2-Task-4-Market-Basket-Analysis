# PLAN OF ATTACK STEP 2: INSTALL, UPLOAD AND GET TO KNOW YOUR DATA SET

library(arules)
library(arulesViz)
library(readr)
library(reshape2)

tr <-
    read.transactions("C:/Users/user/Downloads/ElectronidexTransactions2017.csv", sep = ",")

tr <- read_csv("C:/Users/user/Downloads/ElectronidexTransactions2017.csv", col_names = FALSE)

product_key <-
  read_delim("~/producttypekey.csv", ";", escape_double = FALSE, trim_ws = TRUE)

names(product_key) <- c("labels", "product type")

tr@itemInfo$producttype <- product_key$`product type`

by_producttype <- arules::aggregate(tr, by = "producttype")


# duplicates <- duplicated(tr)
# table(duplicates)
# which(duplicates == TRUE)
# 
# 
# B2B <- tr[which(duplicates == TRUE),]
# B2C <- tr[-which(duplicates == TRUE),]
# 
# write.csv(B2B, file = "B2B.csv", row.names = FALSE)
# write.csv(B2C, file = "B2C.csv", row.names = FALSE)
# 
# B2B <-
#   read.transactions("C:/Users/user/Documents/market basket analysis/B2B.csv", sep = ",")
# B2C <-
#   read.transactions("C:/Users/user/Documents/market basket analysis/B2C.csv", sep = ",")
# 



x <- as(tr, "matrix")

a <- t(x)

a <- as.data.frame(a)

row.names(a)

a$names <-  row.names(a)

a <- a[ , c(9836,1:9835)]

row.names(a) <- 1:nrow(a)

hola1 <- merge(a, product_key, by.x = "names", by.y = "product name", all.x = TRUE)

hola1 <- hola1[, c(1, 9837, 2:9836)]

dataframe <- t(hola1)

#
#
# # dataframe <- as(dataframe, "matrix")
#



########






inspect(by_producttype)
itemLabels(by_producttype)
LIST(by_producttype)
length (by_producttype)
size(by_producttype)
itemsbought <- sort(size(by_producttype), decreasing = T)
table(itemsbought)
hist(itemsbought, breaks = 30)

by_producttype[order(size(by_producttype))]
sorted <- by_producttype[order(size(by_producttype))]


# PLAN OF ATTACK STEP 3: VISUALIZE YOUR DATA SET

itemFrequency(by_producttype, type = "absolute")
itemFrequencyPlot(by_producttype, type = "absolute", topN = 10, horiz = F)
image(by_producttype)

set.seed(243)
image(sample(by_producttype, 100))

# PLAN OF ATTACK STEP 4: APPLY THE APRIORI ALGORITHM

RulesName<- apriori (by_producttype, parameter = list(supp = 0.15, conf = 0.5, minlen = 2))

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

