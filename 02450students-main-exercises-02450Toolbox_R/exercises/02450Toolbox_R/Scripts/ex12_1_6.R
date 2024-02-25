####################
# Exercise 12.1.6
####################

# Load data and functions defined in previous scripts:
source(file.path('Scripts', 'ex12_1_4.R') )
source(file.path('Scripts', 'ex12_1_5.R') )

source("setup.R")

# Turn into transaction list:
T <- mat2transactions(v$X, v$attributeNames)

# Run apriori and print result. See documentation for more options:
rules <- apriori(T, parameter = list(supp = 0.3, conf = 0.6, target = "rules"))
summary(rules)

# For only taking a few rules or sorting
as(rules, "data.frame")

