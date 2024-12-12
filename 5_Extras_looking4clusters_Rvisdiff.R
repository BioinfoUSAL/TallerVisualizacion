## Perform dimensional recduction, clustering and visualization methods
# Installation
library(devtools)
install_github('BioinfoUSAL/looking4clusters')
library(looking4clusters)
install.packages("Rtsne")
install.packages("NMF")
install.packages("uwot")

# https://raw.githubusercontent.com/sdelquin/laliga-data/refs/heads/main/datasets/S2324-laliga-players.csv
laliga <- read.csv('S2324-laliga-players.csv')
laliga <- laliga[laliga$competition=="Classics",]

subset <- laliga[,sapply(laliga,is.numeric)]
rownames(subset) <- laliga$slug
subset <- subset[,sapply(subset,function(x){
  return(sum(is.na(x))<(nrow(subset)*0.25))
})]
players <- apply(subset,1,function(x){
  return(!sum(is.na(x)))
})

obj <- l4c(subset[players,-c(1:5,13,22,23)], groups=laliga[players,'position'], threads = 2)
plot(obj, dir="laliga_l4c")


## Visualization of contrast tests with the package Rvisdiff
# Identify differential variables between Goalkeepers and other positions
# https://www.bioconductor.org/packages/release/bioc/html/Rvisdiff.html
library(devtools)
install_github('BioinfoUSAL/Rvisdiff')
install.packages("matrixTests")

library(Rvisdiff)
library(matrixTests)

inputdata <- subset[players,-c(1:5,13,22,23)]
inputclass <- laliga[players,'position']
goalkeeper <- inputdata[laliga[players,'position'] == "Goalkeeper",]
others <- inputdata[laliga[players,'position'] != "Goalkeeper",]


wilcox <- col_wilcoxon_twosample(goalkeeper, others)
stat <- wilcox$statistic
p <- wilcox$pvalue
log2FoldChange <- log2(colMeans(others)+1) - log2(colMeans(goalkeeper)+1)
wilcox <- cbind(stat = stat, pvalue = round(p, 6),
    padj = p.adjust(p, method = "BH"),
    baseMean = colMeans(inputdata ),
    log2FoldChange = log2FoldChange)
rownames(wilcox) <- colnames(inputdata)

DEreport(wilcox, t(inputdata), inputclass== "Goalkeeper", dir="DEreport" )

