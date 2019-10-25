# hola hola

# PLAN OF ATTACK STEP 2: INSTALL, UPLOAD AND GET TO KNOW YOUR DATA SET

library(arules)
library(arulesViz)

electronidex_transactions <- 
  read.transactions("C:/Users/user/Downloads/ElectronidexTransactions2017.csv", sep = ",")

inspect(electronidex_transactions)
length (electronidex_transactions)
size(electronidex_transactions)
LIST(electronidex_transactions)
itemLabels(electronidex_transactions)

# PLAN OF ATTACK STEP 3: VISUALIZE YOUR DATA SET

itemFrequency(electronidex_transactions, type = "absolute")
itemFrequencyPlot(electronidex_transactions, type = "absolute", topN = 10, horiz = F)
image(electronidex_transactions[1:50])

sample(electronidex_transactions, 50)
itemFrequencyPlot(sample(electronidex_transactions, 50))
image(sample(electronidex_transactions, 50))

# PLAN OF ATTACK STEP 4: APPLY THE APRIORI ALGORITHM

RulesName<- apriori (electronidex_transactions, parameter = list(supp = 0.05, conf = 0.1))

# PLAN OF ATTACK STEP 5: EVALUATE YOUR MODEL

inspect(RulesName)

# PLAN OF ATTACK STEP 6: IMPROVE YOUR MODEL

inspect(sort(RulesName, by = "count"))
ItemRules <- subset(RulesName, items %in% "iMac")

is.redundant(RulesName)

# PLAN OF ATTACK STEP 7: VISUALIZE YOUR RESULTS

plot(RulesName)
plot(RulesName, method="graph", control=list(type="items")) 

