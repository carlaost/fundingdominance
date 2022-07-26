#SETUP ####
library(readr)
library(ggplot2)
library(car)
library(psych)
library(MASS)
library(sfsmisc)
library(broom)
library(interactions)
library(jtools)
library(reghelper)
library(robustbase)

#load processed dataset
filepath = 'Downloads/funding_dominance_processed.csv'
df = read_csv(filepath)
#create facial expression factor variable
df$smile_dom = df$smile
df$smile_dom[df$smile=='dominant'] = 1
df$smile_dom[df$smile!='dominant'] = 0
df$smile_dom = as.factor(df$smile_dom)
df$smile = as.factor(df$smile)

#apply inclusion criterion 2: minimum of 12 tweets
df2 = df[df$tweet_count > 11,]

#DESCRIPTIVE DATA EXPLORATION####
View(df2)

#scatter plot of deal size as function of tweet dominance
ggplot(df2, aes(ebk_d, deal_size)) +
  geom_point() +
  geom_smooth()

#boxplots of deal size as function of profile picture dominance
boxplot(deal_size~smile, data=df2)
boxplot(deal_size~smile_dom, data=df2)

#distribution of deal size
hist(df$deal_size)
quantile(df2$deal_size, probs = seq(0, 1, by= 0.1), na.rm=TRUE)

#scatterplot of bottom 99% and 95% deal size as function of tweet dominance
ds99 = quantile(df2$deal_size, .99, type=1, na.rm=TRUE)
ds95 = quantile(df2$deal_size, .95, type=1, na.rm=TRUE)

ggplot(df2, aes(ebk_d, deal_size)) +
  geom_point() +
  geom_smooth() +
  ylim(0,ds99)

ggplot(df2, aes(ebk_d, deal_size)) +
  geom_point() +
  geom_smooth() +
  ylim(0,ds95)

#basic descriptive stats
summary(df2)
sd(df2$ebk_d, na.rm = TRUE) # SD of tweet dominance
sd(df2$network, na.rm = TRUE) # SD of network size
sd(df2$deal_size, na.rm = TRUE) # SD of deal size

#correlation matrix
df2$smile_dom_n = as.numeric(df2$smile_dom)
df2_cor = data.frame(df2$deal_size, df2$ebk_d, df2$smile_dom_n, df2$network)
cor(df2_cor, use='complete.obs')
corr.test(df2_num)

#INITIAL REGRESSION (OLS)####
reg_full = lm(deal_size ~ ebk_d + smile_dom + ebk_d*network + smile_dom*network, data=df2)
summary(reg_full)
confint(reg_full)

#assumption tests
dwt(reg_full) # Durbin Watson Test: independence
vif(reg_full) # Variance Inflation Factor: multicollinearity
plot(reg_full) # plots: residuals

#ROBUST REGRESSION (MM-ESTIMATORS)####
lmr = lmrob(deal_size ~ ebk_d + smile_dom + ebk_d*network + smile_dom:network, data=df2)
summary(lmr) # display results
confint(lmr) # estimate confidence intervals

#calculate f2 based on summary(lmr): R2 = 0.09451; Adj. R2 = 0.08817
R2 = 0.09451
f2 = R2 / (1 - R2)
f2
R2adj = 0.08817
f2a= R2adj / (1-R2adj)
f2a

#interaction analysis
johnson_neyman(model=lmr, pred=ebk_d, modx=network,
               mod.range=(c(0,20000)), control.fdr = TRUE)

#descriptives of cases included in regression (cases including NA were excluded, NAs were mostly due to missing profile pictures)
df_lmr = lmr$model # dataframe creation 
summary(df_lmr)
sd(df_lmr$ebk_d, na.rm = TRUE) # SD of tweet dominance
sd(df_lmr$network, na.rm = TRUE) # SD of network size
sd(df_lmr$deal_size, na.rm = TRUE) # SD of deal size
nobs(lmr) # n

#correlation matrix
df_lmr$smile_dom_n = as.numeric(df_lmr$smile_dom)
c = data.frame(df_lmr$deal_size, df_lmr$ebk_d, df_lmr$smile_dom_n, df_lmr$network)
cor(c)
corr.test(c)