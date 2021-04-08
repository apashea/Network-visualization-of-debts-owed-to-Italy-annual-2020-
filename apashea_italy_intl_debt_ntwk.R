# World Bank - 2020 Annual International Debt Statistics

library(tidyverse)
library(igraph)

# Importing our data
wb_debt <- read_csv("C:/Users/andre/Desktop/R/ntwk_intl_debt/Sheet_1_data.csv")

# Tidying our data
df <- wb_debt %>% 
  mutate(creditor = `Creditor Country`, debtor = `Debtor Country`, debt = `Total Debt Service`) %>%
  select(creditor, debtor, debt) %>%
  arrange(desc(debt))

# Checking for unique creditor country names
unique(df$creditor)

# Filtering for Italy as creditor and making an igraph object
italy_df <- df %>% filter(creditor == "Italy")
italy_g <- italy_df %>% 
  select(creditor, debtor) %>% 
  as.matrix() %>% 
  graph.edgelist(directed = TRUE)

# FIGURE 1: Basic plot
plot(italy_g)

# Adding debt as an edge attribute
italy_debt <- italy_df %>%
  select(debt) %>%
  unlist()

# Creating a layout igraph generates to best fit our data's network structure
layout <- layout_nicely(italy_g)

# Setting out plot's margins
par(mar = c(3,0.5,3,0.5))
# FIGURE 2: Italy's network of debtor countries
plot(italy_g,
     vertex.shape = "rectangle",
     vertex.size = 40,
     vertex.size2 = 25,
     vertex.label.color = "black",
     vertex.color = "#F25226",
     vertex.label.font = 2,
     vertex.label.family = "Helvetica",
     edge.color = '#C2FFB7',
     edge.width = (log(italy_debt) * 5),
     edge.label = italy_debt,
     edge.label.font = 4,
     edge.label.color = "black",
     layout = layout)
mtext("Debts Owed to Italy", line = -1, side = 3,cex = 1.2, font = 2, at = -0.9)
mtext("2020, International Debt Statistics, Measured in thousands of USD", line = -2, side = 3, cex = 1.0, font = 1, at = -0.41)
mtext("Source: Word Bank, Debt Service Suspension Initiative, \n    https://datatopics.worldbank.org/debt/ids/", line = -1, side = 1, font = 1, at = 0.6)

