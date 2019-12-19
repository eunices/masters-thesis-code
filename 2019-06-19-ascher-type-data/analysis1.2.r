source('2019-06-19-ascher-type-data/var.R')

library(ResourceSelection) # mcfadden
library(pscl) # hoslem
library(caret) # cross validation
library(tidyr)
# library(ROCR) # roc

# Read data
df <- fread(paste0(dir_data, "eda1_flow/2019-11-01-flow-GLM.csv"), encoding="UTF-8")

auth <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final.csv"), encoding="UTF-8")
auth <- auth[ns_spp_N >=1,c("idx_auth", "residence.country.describer.n")]
auth <- data.table(auth %>% separate_rows(residence.country.describer.n, sep="; "))
auth_N <- auth[residence.country.describer.n != "[unknown]", list(N_taxonomist=.N), by=c("residence.country.describer.n")]
auth <- auth[, idx:= 1:.N, by=c("idx_auth")]
auth <- auth[!duplicated(idx_auth)]
auth <- unique(auth[residence.country.describer.n != "[unknown]"]$residence.country.describer.n)

# Data filtering
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
                       levels= c("Upper middle income", "Low income", "Lower middle income", "High income"))
df$Class_des <- factor(df$Class_des, 
                       levels= c("Upper middle income", "Low income", "Lower middle income", "High income"))
df$flow <- factor(df$flow, levels=c("No", "Yes"))

sapply(df, function(x) any(is.na(x)))

# baseline: Same continent, equal class, not colonised, not adjacent, Australia, Low income

# Model 1: predict binary
a0 <- glm(flow ~ continent_check + col_check + adj_check + Class_ori + Class_des, 
          data=df, family="binomial")
a1 <- glm(flow ~ continent_check + adj_check + col_check +  + class_check + N_taxonomist, 
          data=df, family="binomial")
a2 <- glm(flow ~ continent_check * adj_check + col_check +  + class_check + N_taxonomist, 
          data=df, family="binomial")
summary(a1)
# anova(a0, a1, test="Chisq") # significant = keep complex model
exp(cbind(OR = coef(a1), confint(a1, level=0.95)))

# Plot data
# newdata1 <- with(df, data.frame(continent_check=factor(1:2),
#                                 class_check=factor(1:3),
#                                 col_check=factor(1:2),
#                                 adj_check=factor(1:2),
#                                 Class_ori=factor(1:4),
#                                 continent_ori=factor(1:7)))
newdata1 <- expand.grid(continent_check=c("Different continent", "Same continent"),
                        class_check=c("Equal", "High-to-low", "Low-to-high"),
                        col_check=c("Colonised", "Not colonised"),
                        adj_check=c("Adjacent", "Not adjacent"))
newdata1$continent_check <- factor(newdata1$continent_check, levels=c("Same continent", "Different continent"))
newdata1$class_check <- factor(newdata1$class_check, levels=c("Equal", "Low-to-high", "High-to-low"))
newdata1$col_check <- factor(newdata1$col_check, levels=c("Not colonised", "Colonised"))
newdata1$adj_check <- factor(newdata1$adj_check, levels=c("Not adjacent", "Adjacent"))
newdata1$flow <- predict(a1, newdata = newdata1, type = "response")
newdata1[order(-newdata1$flow),]

# TODO: 
# https://stats.idre.ucla.edu/r/dae/logit-regression/ 
# continue with this

# Diagnostics
with(a1, null.deviance - deviance)
with(a1, df.null - df.residual)
with(a1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(a1)


# https://www.r-bloggers.com/evaluating-logistic-regression-models/

# Macfadden's pseudo Rsq
pR2(a0)
pR2(a1)

# Homer-Lemeshow Test
# http://ijbssnet.com/journals/Vol_4_No_3_March_2013/6.pdf
hoslem.test(as.numeric(df$flow), fitted(a1), g=10)

# Model fit
varImp(a1)

# CV
smp_size <- floor(0.75 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_fit <- train(flow ~ continent_check + col_check + adj_check + class_check, 
                 data=train, family="binomial", method="glm",
                 trControl = ctrl, tuneLength = 5)
pred <- predict(mod_fit, newdata=test)
confusionMatrix(data=pred, test$flow)

# https://courses.washington.edu/b515/l14.pdf




# Model 2: predict proportion
a2 <- glm(cbind(N_flow, N_total-N_flow) ~ continent_check + class_check + col_check + adj_check + continent_ori, data=df, family="binomial")
summary(a2)
par(mfrow=c(2,2)); plot(a2)


# Overdispersion
resid.ssq <- sum(residuals(a2, type="pearson")^2)   ## sum of squares of Pearson resids
resid.df <- nrow(df)-length(coef(a2))               ## estimated resid df (N-p)
resid.ssq/resid.df