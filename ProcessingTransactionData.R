# 1. Load the arules package
library(arules)

# 2. Load the Groceries data set that comes with the arules package
data("Groceries",package = "arules")

# 3. Use the summary function to familiarize yourself with the data in the data set
summary(Groceries)

# 4. Use the inspect() function to view transactions 3 through 6
inspect(Groceries[3:6])

# 5. Use the itemfrequency() function to see the proportion of transactions that contain the first  item
itemFrequency(Groceries[,1:1])

# 6. View the proportion of transactions that contain of a number of other items (replace x with an integer)
itemFrequency(Groceries[,1:30])

# 7. Use the itemFrequencyPlot() to visually display the proportion of transactions containing items that have at least 15 percent support
itemFrequencyPlot(Groceries, support = .15)

# 8. Display the proportions of the top ten items
itemFrequencyPlot(Groceries,topN = 10)

# 9. Visualize the entire sparse matrix
image(sample(Groceries,169))

