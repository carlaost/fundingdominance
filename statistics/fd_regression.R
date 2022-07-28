#SETUP####
#load packages
library(readr)
library(ggplot2)
library(car)
library(psych)
library(MASS)
library(sfsmisc)
library(broom)
#load data
filepath = '/Users/Carla/Downloads/funding_dominance_processed.csv'
df = read_csv(filepath)
#create smile type as factor variable
df$smile_dom = df$smile
df$smile_dom[df$smile=='dominant'] = 1
df$smile_dom[df$smile!='dominant'] = 0
df$smile_dom = as.factor(df$smile_dom)
df$smile = as.factor(df$smile)
#apply inclusion criterion 2 (tweet count >=12) & 3 (face detected)
df2 = df[df$tweet_count > 11,]
df2 = na.omit(df2)

#DESCRIPTIVE####
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
corr.test(df2_cor)
#OLS REGRESSION####
reg = lm(deal_size ~ ebk_d + smile_dom + ebk_d*network + smile_dom*network, data=df2)
summary(reg)
confint(reg)
#calculate f2 based on summary(lmr): R2 = 0.01823; Adj. R2 = 0.01136
R2 = 0.01823
f2 = R2 / (1 - R2)
f2
R2adj = 0.01136
f2a= R2adj / (1-R2adj)
f2a

#assumption tests
dwt(reg) # Durbin Watson Test: independence
vif(reg) # Variance Inflation Factor: multicollinearity
plot(reg) # plots: residuals

#ROBUST REGRESSION (HUBER WEIGHTS)####
rr = rlm(deal_size ~ ebk_d + smile_dom + ebk_d*network + smile_dom:network, data=df2, na.action=na.omit)
summary(rr)
#calculate p values
f.robftest(rr, var = '(Intercept)') # p=0.02067 *
f.robftest(rr, var = 'ebk_d') #p=0.009244 **
f.robftest(rr, var = 'smile_dom1') #p=0.3985
f.robftest(rr, var = 'network') #p=0.04376 *
f.robftest(rr, var = 'ebk_d:network') #p=0.0426 *
f.robftest(rr, var = 'smile_dom1:network') #p=0.3039
#explore regression results
tidy(rr, conf.int=TRUE)
df2_rr = rr$model
View(df2_rr)
summary(df2_rr)
quantile(df2_rr$deal_size, probs = seq(0, 1, by= 0.1), na.rm=TRUE)
sd(df2_rr$ebk_d, na.rm = TRUE)
quantile(df2_rr$network, probs = seq(0, 1, by= 0.1), na.rm=TRUE)
sd(df2_rr$network, na.rm = TRUE)
vcov(rr)

#correlation matrix
df2_rr$smile_dom_n = as.numeric(df2_rr$smile_dom)
c = data.frame(df2_rr$deal_size, df2_rr$ebk_d, df2_rr$smile_dom_n, df2_rr$network)
cor(c)
corr.test(c)