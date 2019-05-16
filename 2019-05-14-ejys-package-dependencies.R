##################################################

library(igraph)
library(miniCRAN)

setwd('c:/Dev/msc/01-literature-review/')
df = read.csv('data-out/clean/pkgs-comb-2019-05-13.csv', stringsAsFactors = F)

tags <- c(df[df$source == "CRAN",]$name)
x <- makeDepGraph(tags, available=T, recommended=F, enhances = F, includeBasePkgs = F, suggests = F)
# attributes(x)
# class(x)
# x[]
# igraph::E(x)
# igraph::V(x)
# igraph::vertex.attributes(x)

plotColours <- c("ivory2", "orange1")
pkgsToHighlight <- attr(x, "pkgs")
topLevel <- as.numeric(igraph::V(x)$name %in% pkgsToHighlight)
vColor <- plotColours[1 + topLevel]
vFont <- 1 + topLevel
vShape <- c("none", "circle")[1 + topLevel]
edgeColor <- c(Imports = "grey70", Depends = "grey70", Suggests = "grey70", Enhances = "grey70", LinkingTo = "grey70")

eColor <- edgeColor[igraph::get.edge.attribute(x, "type")]
typesInGraph <- unique(igraph::get.edge.attribute(x, "type"))
edgeColor <- edgeColor[typesInGraph]

par(mai = rep(0.25, 4))

pdf("plots/2019-05-14-package-networks.pdf", width = 24, height = 24)
igraph::plot.igraph(x, vertex.size = 1,
                    edge.arrow.size = 0.5,
                    edge.color = eColor,
                    vertex.label.cex = 0.5,
                    vertex.label.color = "black",
                    vertex.color = vColor,
                    vertex.shape = vShape,
                    vertex.label.font = vFont,
                    xlim = c(-1.5, 1)
)

dev.off()

write.csv(as_long_data_frame(x), 'data-out/pkgs-deps-2019-05-14.csv')

##################################################

library(visNetwork)
visIgraph(dg, physics = F)

##################################################

g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F ) 
plot(g1)
class(g1)
graph_attr(g1)
attributes(g1)


