# Information about code:
# This code corresponds to a chapter in my MSc thesis for
# Chapter 3, the section on Determinants of taxonomic resources flow: data analysis (GLM)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Set up
source('2019-06-19-jsa-type-ch3-flow/analysis1/prep.R')
source('2019-06-19-jsa-type-ch3-flow/analysis1/data_analysis1b.R')

# Libraries
library(tidyr)
library(ResourceSelection) # mcfadden
library(pscl)   # hoslem
library(caret)  # cross validation
# library(ROCR) # roc

# Read and process describer data
auth <- get_des(write=F)
auth <- auth[ns_spp_N >=1,c("idx_auth", "residence.country.describer.n")]
auth <- data.table(auth %>% separate_rows(residence.country.describer.n, sep="; "))
auth_N <- auth[residence.country.describer.n != "[unknown]", list(N_taxonomist=.N), 
               by=c("residence.country.describer.n")]
auth <- auth[, idx:= 1:.N, by=c("idx_auth")]
auth <- auth[!duplicated(idx_auth)]
auth <- unique(auth[residence.country.describer.n != "[unknown]"]$residence.country.describer.n)

# Read bee data
df <- fread(paste0(dir_data, "eda1_flow/2019-11-01-flow-GLM.csv"), encoding="UTF-8")
df <- df[class_check != 'Unclassed']
df <- df[continent_ori != 'Unclassed']
df <- df[ori %in% auth]
df <- merge(df, auth_N, by.x="ori", by.y="residence.country.describer.n", all.x=T, all.y=F)
df <- unique(df)

sapply(df[, 5:10], unique)
sapply(df[, 5:10], table)
table(df$ori)

# Derived values
df$prop_flow <- ifelse(df$N_flow==0, 0, df$N_flow / df$N_total)
df$N_self <- df$N_total-df$N_flow
df$flow <- ifelse(df$N_flow >0, "Yes",  "No")

# Create factors
df$continent_check <- factor(df$continent_check, levels=c("Different continent", "Same continent"))
df$class_check <- factor(df$class_check, levels=c("Equal", "Low-to-high", "High-to-low"))
df$col_check <- factor(df$col_check, levels=c("Not colonised", "Colonised"))
df$adj_check <- factor(df$adj_check, levels=c("Not adjacent", "Adjacent"))
df$continent_ori <- factor(df$continent_ori, 
                           levels=c("Oceania", "Africa", "Australia", 
                                    "Asia", "Europe", "North America", "South America"))
df$Class_ori <- factor(df$Class_ori, 
                       levels= c("Upper middle income", "Low income", 
                                 "Lower middle income", "High income"))
df$Class_des <- factor(df$Class_des, 
                       levels= c("Upper middle income", "Low income",
                                 "Lower middle income", "High income"))
df$flow <- factor(df$flow, levels=c("No", "Yes"))
sapply(df, function(x) any(is.na(x))) # !CHECK

# Model 1: predict binary
# baseline: Same continent, equal class, not colonised, not adjacent, Australia, Low income

# Model: GLM
# a0 <- glm(flow ~ continent_check + col_check + adj_check + Class_ori + Class_des, 
#           data=df, family="binomial")
a1 <- glm(flow ~ continent_check + adj_check + col_check + class_check + N_taxonomist, 
          data=df, family="binomial")
# anova(a0, a1, test="Chisq") # if significant = keep complex model

# Model summary
summary(a1) # !IMPORTANT
exp(cbind(OR = coef(a1), confint(a1, level=0.95))) # !IMPORTANT

# Model diagnostics
# source: https://www.r-bloggers.com/evaluating-logistic-regression-models/

# Macfadden's pseudo Rsq
pR2(a0)
pR2(a1) # !IMPORTANT

# Deviance explained
with(a1, null.deviance - deviance)
with(a1, df.null - df.residual)
with(a1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(a1)

# Homer-Lemeshow Test
# source: http://ijbssnet.com/journals/Vol_4_No_3_March_2013/6.pdf
hoslem.test(as.numeric(df$flow), fitted(a1), g=10)

# Variable importance
varImp(a1)

# Model 2: predict proportion
a2 <- glm(cbind(N_flow, N_total-N_flow) ~ continent_check + class_check + col_check + adj_check + 
    continent_ori, data=df, family="binomial")
summary(a2)
par(mfrow=c(2,2)); plot(a2)


# Overdispersion
resid.ssq <- sum(residuals(a2, type="pearson")^2)   ## sum of squares of Pearson resids
resid.df <- nrow(df)-length(coef(a2))               ## estimated resid df (N-p)
resid.ssq/resid.df