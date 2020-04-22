install.packages("arules")
update.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("tidyverse")
library(tidyverse)
install.packages("readxml")
install.packages("knitr")
library(knitr)
install.packages("ggplot2")
update.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library(lubridate)
install.packages("plyr")
library(plyr)
library(dplyr)
ord_prod <- read.csv("/Users/poojatyagi/Desktop/University of Arizona admission process/Courses/BI/instacart-market-basket-analysis 2/order_products__train.csv")
ord <- read.csv("/Users/poojatyagi/Desktop/University of Arizona admission process/Courses/BI/instacart-market-basket-analysis 2/orders.csv")
prod <- read.csv("/Users/poojatyagi/Desktop/University of Arizona admission process/Courses/BI/instacart-market-basket-analysis 2/products.csv")

head(ord)
prod_orders <- merge(ord_prod,ord,by='order_id',all = F)
prod_orders <-merge(prod_orders,prod, by ='product_id',all=F)
dim(prod_orders)
str(prod_orders)
prod_orders <- prod_orders[complete.cases(prod_orders), ]
unique(prod_orders$eval_set)
prod_orders <- prod_orders[as.factor(prod_orders$eval_set) == "train", ]
dim(prod_orders)
str(prod_orders)
prod_orders %>% mutate(product_name = as.character(product_name))
str(prod_orders)

str(prod_orders)
prod_orders$product_id <- as.character(prod_orders$product_id)
prod_orders$product_name <- as.character(prod_orders$product_name)
prod_orders$order_id <- as.character(prod_orders$order_id)
prod_orders$user_id <- as.character(prod_orders$user_id)
str(prod_orders)

df2 <- prod_orders[prod_orders$reordered == 1,]
dim(df2)
prod_orders <- rbind(prod_orders, df2)
dim(prod_orders)

glimpse(prod_orders)

library(plyr)
transactionData <- ddply(prod_orders,c("order_id"),
                         function(df1)paste(df1$product_name,
                                            collapse = ","))

head(transactionData)
transactionData$order_id <- NULL
colnames(transactionData) <- c("items")
head(transactionData)
write.csv(transactionData,"/Users/poojatyagi/Desktop/University of Arizona admission process/Courses/BI/instacart-market-basket-analysis 2/market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
tr <- read.transactions('/Users/poojatyagi/Desktop/University of Arizona admission process/Courses/BI/instacart-market-basket-analysis 2/market_basket_transactions.csv', 
                        format = 'basket', sep=',')
trObj<-as(tr,"transactions")
tr
summary(tr)



if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}

itemFrequencyPlot(tr,topN=20,type="absolute",
                  col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot of high selling top 20 products")


itemFrequencyPlot(tr,topN=20,type="relative",
                  col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot of high selling top 20 products relative to others")

association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))


summary(association.rules)
inspect(association.rules[1:10])

association.rules <- sort(association.rules, by = c('confidence','lift'))


shorter.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=4))

subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1)
length(subset.rules)

subset.association.rules. <- association.rules[-subset.rules]


subRules<-association.rules[quality(association.rules)$confidence>0.4]
plot(subRules, jitter = 0)


plot(subRules,method="two-key plot")



plotly_arules(subRules)


top10subRules <- head(subRules, n = 20, by = c("confidence","lift"))
plot(top10subRules, method = "graph",  engine = "htmlwidget")

subrules2 <- head(subRules, n=20, by = "lift")
plot (subrules2, method = "paracoord")



