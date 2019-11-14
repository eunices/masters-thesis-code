# https://www.statforbiology.com/2019/stat_nlmm_interaction/

library(devtools)
# install_github("OnofriAndreaPG/aomisc")

library(lattice)
library(nlme)
library(aomisc)

dataset <- read.csv("https://raw.githubusercontent.com/OnofriAndreaPG/agroBioData/master/growthNGEN.csv",
  header=T)
dataset$Block <- factor(dataset$Block)
head(dataset)

table(dataset$Block, dataset$Plot)

# Without blocking, plots, "naive"
modNaive1 <- drm(Yield ~ DAS, fct = L.3(), data = dataset,
            curveid = GEN:N,
            pmodels = c( ~ 1,  ~ N*GEN,  ~ N*GEN))

# With blocking and plots
modnlme1 <- nlme(Yield ~ nlsL.3(DAS, b, d, e), data = dataset,
                    random = d + e ~ 1|Block/Plot,
                    fixed = list(b ~ 1, d ~ N*GEN, e ~ N*GEN),
                    weights = varPower(),
                    start = coef(modNaive1), control = list(msMaxIter = 200))