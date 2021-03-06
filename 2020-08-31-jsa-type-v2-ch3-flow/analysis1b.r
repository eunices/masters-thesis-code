# Information about code:
# This code corresponds to a chapter in my MSc thesis for
# Chapter 3, the section on Determinants of taxonomic resources flow: data analysis (GLM)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Set up
source('2020-08-31-jsa-type-v2-ch3-flow/analysis1/prep.r')
source('2020-08-31-jsa-type-v2-ch3-flow/analysis1/data_analysis1b.r')

# Libraries
library(tidyr)
library(car)
library(ResourceSelection) # mcfadden
library(pscl)              # hoslem
library(caret)             # cross validation
library(boot)
# library(ROCR)            # roc


# Process data  ------------------------------------------------------------------------------------
# Read and process describer data
auth <- get_des()
names(auth)

# Subset those with at least one valid species
auth <- auth[
    (ns_spp_N + syn_spp_N) >=1, 
    c("full.name.of.describer", "residence.country.describer.first")
]

auth_N <- auth[
    residence.country.describer.first != "[unknown]", list(N_taxonomist=.N), 
    by=c("residence.country.describer.first")
]

auth <- unique(auth[
    residence.country.describer.first != "[unknown]"
]$residence.country.describer.first)


# Read bee data
df <- fread(
    paste0(v2_dir_data_ch3_flow, "2019-11-01-flow-GLM.csv"), encoding="UTF-8"
)

# Check 
sqrt(dim(df)[1]) # 249 countries
df[class_check == 'Unclassed']
df[Class_des == 'Unclassed', .N, by="des"]
df[Class_ori == 'Unclassed', .N, by="ori"]
unclass <- unique(df[Class_ori == 'Unclassed']$ori)
length(unclass) # 33 with no class
auth[auth %in% unclass] # none of the authors from there

sqrt(dim(df)[1]) - length(unclass) # expected number of countries 

df <- df[class_check != 'Unclassed'] # removing countries with no class
df <- df[ori %in% auth] # only filtering for countrie with authors

# Check
dim(df)[1]
length(auth)
dim(df)[1]/length(auth)

df <- merge(
    df, auth_N, by.x="ori", by.y="residence.country.describer.first", 
    all.x=T, all.y=F
)

df <- unique(df)

# Remove where ori and des are same
dim(df)

df <- df[ori != des] # minus length(auth) number of rows

dim(df)[1] + length(auth)



dim(df)[1]

length(table(df$ori)) # number of origin countries
length(table(df$des)) # number of describer countries

table(df$ori)
table(df$des)

sapply(df[, 5:10], unique)
sapply(df[, 5:10], table)

unique(df$ori)

# Derived values
df$prop_flow <- ifelse(df$N_flow==0, 0, df$N_flow / df$N_total)
df$N_self <- df$N_total-df$N_flow
df$flow <- ifelse(df$N_flow >0, "Yes",  "No")

# Create factors
df$continent_check <- factor(
    df$continent_check, levels=c("Different continent", "Same continent")
)

df$class_check <- factor(
    df$class_check, levels=c("Equal", "Low-to-high", "High-to-low")
)

df$col_check <- factor(
    df$col_check, levels=c("Not colonised", "Colonised")
)

df$adj_check <- factor(
    df$adj_check, levels=c("Not adjacent", "Adjacent")
)

df$continent_ori <- factor(
    df$continent_ori, levels=c(
        "Africa", "Oceania", "Asia", "Australia", 
        "South America", "North America",  "Europe"
    )
)

df$continent_des <- factor(
    df$continent_des, levels=c(
        "Africa", "Oceania", "Asia", "Australia", 
        "South America", "North America",  "Europe"
    )
)

df$Class_ori <- factor(
    df$Class_ori, levels= c(
        "Upper middle income", "Low income", 
        "Lower middle income", "High income"
    )
)

df$Class_des <- factor(
    df$Class_des, levels=c(
        "Upper middle income", "Low income",
        "Lower middle income", "High income"
    )
)

df$flow <- factor(df$flow, levels=c("No", "Yes"))

any(sapply(df, function(x) any(is.na(x))))
unlist(lapply(df, class))

# Create model -----------------------------------------------------------------
# Model 1: predict binary
# baseline: Same continent, equal class, not colonised, not adjacent, Australia, Low income

# Using number of taxonomists
a1 <- glm(
    flow ~ continent_check + adj_check + col_check + class_check + N_taxonomist, 
    data=df, family="binomial"
)

table(df$continent_check)
table(df$adj_check)

# anova(a0, a1, test="Chisq") # if significant = keep complex model

# Using origin continent
table(df$flow)
a1 <- glm(
    flow ~ continent_check + adj_check + col_check + 
        class_check + continent_ori + continent_des, 
    data=df, family=binomial
)

dim(df)

# in v1 i was using logit (assumes flow has equal classes)
# see https://fukamilab.github.io/BIO202/04-B-binary-data.html
# to change to cloglog as flow is highly inbalanced
# this resulted in decreased but similar effect sizes


# Model summary ----------------------------------------------------------------
glm_summary <- summary(a1)
glm_odds_ratio <- exp(cbind(OR = coef(a1), confint(a1, level=0.95)))
glm_odds_ratio <- glm_odds_ratio[!is.na(glm_odds_ratio[,1]),]
glm_results <- cbind(glm_summary$coefficient, glm_odds_ratio)
glm_results <- cbind(glm_results, "")

glm_results[, (dim(glm_results)[2])] <-
    ifelse(as.numeric(glm_results[,4])<.05, "*", "")

write.csv(
    glm_results, 
    paste0(dir_tables, "2020-06-24-flow-GLM-summary.csv"), 
    fileEncoding="UTF-8", row.names=T
)

glm_results_data <- data.frame(glm_results)
glm_results_data$coefficient <- rownames(glm_results_data)

names(glm_results_data) <- c(
    "coefficient", "se", "z", "p_value", "odds_ratio", 
    "lower_95CI", "upper_95CI", "significant", "coefficient_name"
)

glm_results_data <- cbind(
    data.frame(variable=glm_results_data[,dim(glm_results_data)[2]]),
    glm_results_data[,-dim(glm_results_data)[2]]
)

glm_results_data

wfile <- paste0(v2_dir_data_webapp, "ch3-fig-03-data.csv")
fwrite(glm_results_data, wfile, na="")


# Model diagnostics ------------------------------------------------------------

# source: https://www.r-bloggers.com/evaluating-logistic-regression-models/

# Macfadden's pseudo Rsq
pR2(a1) # !IMPORTANT # McFadden 0.1734151

# Overdispersion
resid.ssq <- sum(residuals(a1, type="pearson")^2)   ## sum of squares of Pearson resids
resid.df <- nrow(df)-length(coef(a1))               ## estimated resid df (N-p)
resid.ssq/resid.df # 1.148002

# Diagnostic plots
diag <- glm.diag(a1)
glm.diag.plots(a1, diag)
# using top left: residuals against X or yhat (how residuals are distributed)
# using top right: QQ plot (how expected transformed fits)

# https://bookdown.org/egarpor/PM-UC3M/glm-diagnostics.html
# https://stat.ethz.ch/R-manual/R-devel/library/boot/html/glm.diag.plots.html
# https://ms.mcmaster.ca/~bolker/R/misc/modelDiag.html

# Multicollinearity
car::vif(a1) # absence of multicollinearity



# Others

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



# Model 2: predict proportion ----------------------------------------------------------------------

a2 <- glm(cbind(N_flow, N_total-N_flow) ~ continent_check + class_check + col_check + adj_check + 
    continent_ori, data=df, family="binomial")
summary(a2)
par(mfrow=c(2,2)); plot(a2)

