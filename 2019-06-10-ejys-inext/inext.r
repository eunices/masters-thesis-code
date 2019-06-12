# Tutorial on iNEXT
# https://cran.r-project.org/web/packages/iNEXT/vignettes/Introduction.html

library(iNEXT)



####################################

data(spider)
str(spider) # abundance data (species abundance x species)

m <- c(1, 5, 20, 50, 100, 200, 400)
x <- iNEXT(spider, q=0, datatype="abundance", size=m)
x$DataInfo
est1 = x$iNextEst$Girdled
est2 = x$iNextEst$Logged
x$AsyEst

x <- iNEXT(spider, q=c(0,1,2), datatype="abundance", size=m)
x$DataInfo
est1 <- x$iNextEst$Girdled
est2 <- x$iNextEst$Logged
x$AsyEst

x <- iNEXT(spider, q=0, datatype="abundance")
x$DataInfo
est1 = x$iNextEst$Girdled
est2 = x$iNextEst$Logged
x$AsyEst

ggiNEXT(x, type=1, se=TRUE, facet.var="none", color.var="site", grey=FALSE)
ggiNEXT(x, type=2, se=TRUE, facet.var="none", color.var="site", grey=FALSE)
ggiNEXT(x, type=3, se=TRUE, facet.var="none", color.var="site", grey=FALSE)

ggiNEXT(x, type=1, se=TRUE, facet.var="site", grey=FALSE)
ggiNEXT(x, type=2, se=TRUE, facet.var="site", grey=FALSE)
ggiNEXT(x, type=3, se=TRUE, facet.var="site", grey=FALSE)


####################################

data(ant) 
str(ant) # incidence data (species incidence frequency x species) + sampling unit

t <- seq(1, 700, by=10)
out.inc <- iNEXT(ant, q=0, datatype="incidence_freq", size=t)

# Sample‐size‐based R/E curves
ggiNEXT(out.inc, type=3, color.var="site") + 
  theme_bw(base_size = 18) + 
  theme(legend.position="none")

# https://rstudio-pubs-static.s3.amazonaws.com/210845_860b29c5e0a643f987a80179b61bcf16.html
# http://www.flutterbys.com.au/stats/tut/tut13.2.html


####################################

data(ciliates)
str(ciliates)
head(ciliates$EtoshaPan) # species incidence raw x plot

out.raw <- iNEXT(ciliates, datatype="incidence_raw", endpoint=150)
ggiNEXT(out.raw)
out.raw

####################################

library(vegan)
data(BCI)

S <- vegan::specnumber(BCI) # total no of species in each site
raremax <- min(rowSums(BCI)) # min no of individuals in each site
Srare <- vegan::rarefy(BCI, raremax)

par(mfrow = c(1,2))
plot(S, Srare, xlab = "Observed No. of Species", 
     ylab = "Rarefied No. of Species",
     main = " plot(rarefy(BCI, raremax))")
abline(0, 1)
rarecurve(BCI, step = 20, 
          sample = raremax, 
          col = "blue", 
          cex = 0.6,
          main = "rarecurve()")



