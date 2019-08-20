# this file applies association rule mining to the project data
# Objective: Top 10 rules by support, confidence, lift
# Attempt to set LHS to a meaningful value and assess top 10 rules
# results will be saved into 'data/analysis_arules.rda'

rm(list = ls())
#################################################################
# load libraries
#################################################################
library(plyr)
library(dplyr)
library(arules)
library(arulesViz)
library(magrittr)
library(caret)
library(ggplot2)
library(reshape2)
library(knitr)
library(gridExtra)

#################################################################
# define functions
#################################################################
# function to convert named vector to dataframe
vec_to_df <- function(vec, nm) {
  df <- as.list(vec) %>% data.frame(.) %>% t
  colnames(df) <- nm
  return(df)
}

# function to kable a table with standard format
# x = table object (i.e. dataframe or matrix)
# nm = table caption
kable_it <- function(x, nm, ...) {
  require(knitr)
  require(tools)
  kable(x,
        digits = 2,
        caption = toTitleCase(nm),
        row.names = FALSE)
}

# function to convert table output to kabeled dataframe
tab_df <- function(varNm, nm) {
  require(magrittr)
  df <- table(df[,varNm]) %>% as.data.frame
  colnames(df) <- c(varNm, 'proportion')
  df$proportion <- df$proportion / sum(df$proportion)
  kable_it(df, nm)
}

# function to perform pearson's chi-square test
# x = categorical variable
# y = categorical variable
chi <- function(x,y) {
  result <- chisq.test(table(x,y))$p.value
  if (result <= 0.05) {1} else 0
}

# interesting relationships
# function to plot cross-tab of two variables
plot_dat <- function(x,y, nm, xnm, ynm) {
  dfPlot <- table(dat[,x], dat[,y]) %>% as.data.frame
  ggplot(dfPlot, aes(Var1, Var2)) + 
    geom_point(aes(size = Freq), colour = "green") + 
    theme_bw() + xlab("") + ylab("") +
    scale_size_continuous(range=c(8,24)) + 
    geom_text(aes(label = Freq)) +
    xlab(xnm) + ylab(ynm) +
    ggtitle(nm)
}

#################################################################
# load transactions dataset - subsets to article text in one year 2018
#################################################################
# load transactions dataset - subsets to article text in one year 2018
load('data/2Transform.rda')
rm(dtm,df)

#################################################################
# explore
#################################################################
# plot item frequency
# relative item frequency plot
itemFrequencyPlot(trans, topN = 20,
                  main = 'Relative Item Frequency Plot')


# absolute item frequency plot
itemFrequencyPlot(trans, type = 'absolute', topN = 20,
                  main = 'Absolute Item Frequency Plot')


#################################################################
# Simulate rule generation
# In the following matrices, minimum support values are varied across rows 
# and minimum confidence values are varied across the columns. 
# 50 simulations of the apriori algorithm are performed, 
# and the results are collected in the matrix. 
# In the following plots, the ideal parameter settings are made 
# apparent by assessment of the magnitude (shading and labels) 
# and patterns that emerge from the heatmap.
#################################################################
# initialize values for simulation matrix
suppVal <- seq(0.10, 0.5, 0.05)
confVal <- seq(0.8, 1, 0.1)
# simulate number of rules generated
mNumRules <- matrix(nrow = length(suppVal), ncol = length(confVal))
rownames(mNumRules) <- suppVal
colnames(mNumRules) <- confVal
for (i in 1:length(suppVal)) {
  for (j in 1:length(confVal)) {
    r <- apriori(trans, parameter = list(supp = suppVal[i], conf = confVal[j]))
    if (length(r) > 0) {
      mNumRules[i,j] <- length(r)
    }
  }
}
mNumRules %>% kable(., caption = "Simulate: Number of Rules")
ggplot(melt(mNumRules), aes(Var1,Var2, fill=value)) + geom_raster() +
  geom_text(aes(label = round(value, 1)), size = 3) +
  scale_fill_gradient(low = 'white', high = 'red') +
  xlab("min_support") + ylab("min_confidence") +
  labs(fill = 'Rules')+
  ggtitle("Heatmap: Number of Rules")
# From the above matrix and heatmap, by setting min confidence to 0.9 and min support to 0.2 a total of 19272 rules can be generated.
# This seems like a good baseline parameter setting for the apriori function. The simulations below will assess mean rule size and quality

# simulate average size of rules (k)
# the mean size of rule itemsets is 5
# given minimum confidence of 0.9 and minimum support of 0.2
# size of 5 might contain diverse length of rule itemsets that are not too large or small.
mSize <- matrix(nrow = length(suppVal), ncol = length(confVal))
rownames(mSize) <- suppVal
colnames(mSize) <- confVal
for (i in 1:length(suppVal)) {
  for (j in 1:length(confVal)) {
    r <- apriori(trans, parameter = list(supp = suppVal[i], conf = confVal[j]))
    if (length(r) > 0) mSize[i,j] <- signif(mean(size(r)),0)
  }
}
mSize %>% kable(., caption = "Simulate: Mean Size of Rules")
ggplot(melt(mSize), aes(Var1,Var2, fill=value)) + geom_raster() +
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = 'white', high = 'red') +
  xlab("min_support") + ylab("min_confidence") +
  labs(fill = 'Size')+ 
  ggtitle("Heatmap: Mean Size of Rules")

# simulate average support quality
# given minsupp = 0.2 and minconf = 0.9, an average support of 0.23 can be expected
# an average support of 0.23 shows fairly high potential for including strong rules.
mSupp <- matrix(nrow = length(suppVal), ncol = length(confVal))
rownames(mSupp) <- suppVal
colnames(mSupp) <- confVal

for (i in 1:length(suppVal)) {
  for (j in 1:length(confVal)) {
    r <- apriori(trans, parameter = list(supp = suppVal[i], conf = confVal[j]))
    if (length(r) > 0) mSupp[i,j] <- signif(mean(quality(r)$support), 2)
  }
}
mSupp %>% kable(., caption = "Simulate: Mean Support of Rules")
ggplot(melt(mSupp), aes(Var1,Var2, fill=value)) + geom_raster() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "white", high = "red") +
  xlab("min_support") + ylab("min_confidence") +
  labs(fill = "Support")+
  ggtitle("Heatmap: Mean Support Quality")

# simulate average confidence quality
# the average confidence to be expected from a minimum support parameter of 0.2 and minimum confidence parameter of 0.9 is 0.93
mConf <- matrix(nrow = length(suppVal), ncol = length(confVal))
rownames(mConf) <- suppVal
colnames(mConf) <- confVal
for (i in 1:length(suppVal)) {
  for (j in 1:length(confVal)) {
    r <- apriori(trans, parameter = list(supp = suppVal[i], conf = confVal[j]))
    if (length(r) > 0) mConf[i,j] <- signif(mean(quality(r)$confidence), 2)
  }
}
mConf %>% kable(., caption = "Simulate: Mean Confidence of Rules")
ggplot(melt(mConf), aes(Var1,Var2, fill=value)) + geom_raster() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "white", high = "red") +
  xlab("min_support") + ylab("min_confidence") +
  labs(fill = "Support")+
  ggtitle("Heatmap: Mean Confidence Quality")

# simulate average lift quality
# An average lift value of 1.1 can be expected from a minimum support parameter of 0.2 and minimum confidence parameter of 0.9
# The average lift is about the same in all simulations.
mLift <- matrix(nrow = length(suppVal), ncol = length(confVal))
rownames(mLift) <- suppVal
colnames(mLift) <- confVal
for (i in 1:length(suppVal)) {
  for (j in 1:length(confVal)) {
    r <- apriori(trans, parameter = list(supp = suppVal[i], conf = confVal[j]))
    if (length(r) > 0) mLift[i,j] <- signif(mean(quality(r)$lift), 2)
  }
}
mLift %>% kable(., caption = "Simulate: Mean Lift of Rules")
ggplot(melt(mLift), aes(Var1,Var2, fill=value)) + geom_raster() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "white", high = "red") +
  xlab("min_support") + ylab("min_confidence") +
  ggtitle("Heatmap: Mean Lift Quality")

#################################################################
# Generate Rules
#################################################################
# generate rules based on pre-determined confidence and support thresholds
rules <- apriori(trans, parameter = list(supp = 0.2, conf = 0.9))
# summary of rules
summary(rules)
# 19272 rules generated
# mean size of rule itemsets is k = 4.5
# mean support of rules is 0.23
# mean confidence of rules is 0.93
# mean lift of rules is 1.1

# print sample of rules
ruledf = data.frame(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)), 
  rules@quality)
head(ruledf) %>% kable_it(., "Sample of Rules Generated")

# plot distributions of rule quality
# the distribution of support is right-skewed and includes very high support values upwards of 40%
# the distribution of lift shows that most values are between 1 and 1.1. Some values are upwards of 2.3.
ggplot(melt(ruledf[3:5]), aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 0.01, alpha = 0.3) +
  facet_wrap(~variable, scales = 'free') +
  ggtitle("Histogram of Quality")
ggplot(melt(ruledf[3:5]), aes(x = value, fill = variable)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~variable, scales = 'free') +
  ggtitle("Density of Quality")

# scatterplot of rules
# shows 75 rules with lift greater than 2
plot(rules, main = "Scatterplot of Rules")

#################################################################
# Top 10 Rules by Support
#################################################################
# Top 10 Rules by Support
ruledf[order(ruledf$support, decreasing = TRUE),] %>% 
  head(., 10) %>%
  kable_it(., "Top 10 Rules by Support")
# These rule itemsets appeared most frequently in the dataset. 
# The support for these 10 rules range from 49% to 58%

#################################################################
# Top 10 Rules by Confidence
#################################################################
# Top 10 Rules by Confidence
ruledf[order(ruledf$confidence, decreasing = TRUE),] %>% 
  head(., 10) %>%
  kable_it(., "Top 10 Rules by Confidence")
# Top 10 rule itemsets had a confidence of 98%, meaning that they have been found to be true almost 100% of the time.

#################################################################
# Top 10 Rules by Lift and Support
#################################################################
# Top 10 Rules by Lift and Support
ruledf[order(-ruledf$lift, -ruledf$support),] %>% 
  head(., 10) %>%
  kable_it(., "Top 10 Rules by Lift and Support")
# These rule itemsets are the most important rules measured by lift
# these also display strong support (20% - 30%) and confidence (94% - 95%).

#################################################################
# Targeted rules
# LHS will be set to 'president'
#################################################################
ruleLHS <- 'trump'
# Generate rules based on min. support 0.041 and min. confidence 0.9
rulesLHS <- apriori(trans, parameter = list(supp = 0.01, conf = 0.5, minlen = 2),
                    appearance = list(default = 'rhs', lhs = ruleLHS))
summary(rulesLHS)

# print sample of rules generated
ruledfLHS = data.frame(
  lhs = labels(lhs(rulesLHS)),
  rhs = labels(rhs(rulesLHS)), 
  rulesLHS@quality)
head(ruledfLHS) %>% kable_it(., "Sample of Rules Generated")

# Top 5 Rules by Support
ruledfLHS[order(ruledfLHS$support, decreasing = TRUE),] %>% 
  head(., 5) %>%
  kable_it(., "Top 5 Rules by Support")


# Top 5 Rules by Confidence
ruledfLHS[order(ruledfLHS$confidence, decreasing = TRUE),] %>% 
  head(., 5) %>%
  kable_it(., "Top 5 Rules by Confidence")


# Top 5 Rules by Lift
ruledfLHS[order(ruledfLHS$lift, decreasing = TRUE),] %>% 
  head(., 5) %>%
  kable_it(., "Top 5 Rules by Lift")

# scatterplot of rules
plot(rulesLHS, main = "Plots of 39 Rules")
# two-key plot of rules
plot(rulesLHS, method = 'two-key plot')
# graph plot subset of rules based on high confidence and support
x <- quality(rulesLHS)$support > 0.2 & quality(rulesLHS)$confidence > 0.9
# subset rules
rulesLHSSub <- subset(rulesLHS, subset = x)
plot(rulesLHSSub, method = 'graph')
# print sample of top most interesting rules
rulesLHSSubDf = data.frame(
  lhs = labels(lhs(rulesLHSSub)),
  rhs = labels(rhs(rulesLHSSub)), 
  rulesLHSSub@quality)
head(rulesLHSSubDf) %>% kable_it(., "Top Association Rules for Trump")
