# Init
data_dir <- "data/2020-07-07-deep-learning-sys-rev/"
test_data_dir <- paste0(data_dir, "test/_data/")

# Libraries
library(meta)
library(dmetar)
library(metafor)
library(xlsx)
library(data.table)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggrepel)

# Source
source("2020-07-07-deep-learning-sys-rev/dmetar.r")


# Load data
# 
metacont <- read.xlsx2(paste0(test_data_dir, "metacont.xlsx"), sheetIndex=1)
metacont[] <- lapply(metacont, as.character)
metacont[,2:dim(metacont)[2]] <- lapply(metacont[,2:dim(metacont)[2]], as.numeric)
metacont <- data.table(metacont)
metacont <- metacont[Author != ""]

# 
load(file = paste0(test_data_dir, "Meta_Analysis_Data.rdata"))
str(Meta_Analysis_Data)
madata <- Meta_Analysis_Data

# 
load(paste0(test_data_dir, "binarydata.RData"))
str(binarydata)

# 
load(paste0(test_data_dir, "mvreg_data.rda"))


# Analysis
# source: https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/fixed.html

# fixed effects
# for pre-calculated effect size
m <- metagen(TE,                      # effect size (Cohen's d or Hedges' g)
             seTE,                    # standard error of calculated effect
             data=madata,             # data.frame
             studlab=Author,          # study name
             comb.fixed = TRUE,       # fixed effect?
             comb.random = FALSE,     # random effect? 
             prediction=TRUE,         # print prediction?
             sm="SMD")                # summary measure (MD=mean diff; SMD=cohen's d/ hedges' g)

m

# individual effect sizes
# number of studies included (k)
# overall effect (g) and confidence interval
# between-study heterogenity tau^2/ I^2/ Q test 

# 0% to 40%: might not be important;
# 30% to 60%: may represent moderate heterogeneity*;
# 50% to 90%: may represent substantial heterogeneity*;
# 75% to 100%: considerable heterogeneity*.
# source: https://tinyurl.com/q-test-1


# for raw data
m.raw <- metacont(Ne,    # column name for number of participants in experimental/ intervention group
                  Me,    # column name for mean of experimental/ intervention group
                  Se,    # column name for standard deviation of experimental/ intervention group
                  Nc,    # column name for number of participants in control group
                  Mc,    # column name for mean of control group
                  Sc,    # column name for standard deviation of control group
                  data=metacont,
                  studlab=Author,
                  comb.fixed = TRUE,
                  comb.random = FALSE,
                  prediction=TRUE,
                  sm="SMD")

m.raw


# random effects

# pre-calculated effect sizes

m.hksj <- metagen(TE,
                  seTE,
                  data = madata,
                  studlab = paste(Author),
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  method.tau = "SJ",   # method to calculate tau. Most popular HKSJ
                  hakn = TRUE,
                  prediction = TRUE,
                  sm = "SMD")
m.hksj
find.outliers(m.hksj)

fo <- find.outliers(m.hksj)
# meta::forest(fo, col.predict = "blue")

inf.analysis <- InfluenceAnalysis(x = m.hksj,
                                  random = TRUE)
summary(inf.analysis)
dmetar:::plot.InfluenceAnalysis(inf.analysis, "influence")
dmetar:::plot.InfluenceAnalysis(inf.analysis, "baujat")
dmetar:::plot.InfluenceAnalysis(inf.analysis, "es")
dmetar:::plot.InfluenceAnalysis(inf.analysis, "is")


# raw data

m.hksj.raw <- metacont(Ne,
                       Me,
                       Se,
                       Nc,
                       Mc,
                       Sc,
                       data = metacont,
                       studlab = paste(Author),
                       comb.fixed = FALSE,
                       comb.random = TRUE,
                       method.tau = "SJ",
                       hakn = TRUE,
                       prediction = TRUE,
                       sm = "SMD")
m.hksj.raw
forest(m.hksj.raw)


# binary data

str(binarydata)
m.bin <- metabin(Ee,
                 Ne,
                 Ec,
                 Nc,
                 data = binarydata,
                 studlab = paste(Author),
                 comb.fixed = FALSE,
                 comb.random = TRUE,
                 method.tau = "SJ",
                 hakn = TRUE,
                 prediction = TRUE,
                 incr = 0.1,
                 sm = "RR")
m.bin

labbe.metabin(x = m.bin,
              bg = "blue",
              studlab = TRUE,
              col.random = "red")


# Multiple regression

mvreg.data$continent <- factor(mvreg.data$continent)
cor(mvreg.data[,3:5])

model1 <- rma(yi = yi,             # effect size 
              sei = sei,           # standard error of effect size
              data = mvreg.data,   # data
              method = "ML",       # tau-squared method. ML or REML advised for multiple reg
              mods = ~ quality,    # regression variables
              test = "knha")       # regression coefficient tests
model1


model2 <- rma(yi = yi, 
              sei = sei, 
              data = mvreg.data, 
              method = "ML", 
              mods = ~ quality + reputation, 
              test="knha")
model2

anova(model1, model2)

permutest(model2)


interaction.model <- rma(yi=yi,
                         sei=sei, 
                         data=mvreg.data, 
                         method = "REML", 
                         mods = ~ pubyear*continent, 
                         test="knha")
interaction.model

multimodel.inference(TE = "yi", 
                     seTE = "sei",
                     data = mvreg.data,
                     predictors = c("pubyear", "quality", "reputation", "continent"),
                     interaction = FALSE)


# Checking for publication biases

funnel(m.hksj,xlab = "Hedges' g",studlab = TRUE)
funnel(m.hksj, xlab="Hedges' g", 
       contour = c(.95,.975,.99),
       col.contour=c("darkblue","blue","lightblue")) +
    legend(1.4, 0, c("p < 0.05", "p<0.025", "< 0.01"), bty = "n",
           fill=c("darkblue","blue","lightblue"))
eggers.test(x = m.hksj)
trimfill(m.hksj)
pcurve(m.hksj)
pcurve(m.hksj, effect.estimation = TRUE, N = N.m.hksj, dmin = 0, dmax = 1)
