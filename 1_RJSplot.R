##################################### 
# Interactive Graphs with RJSplot   #
#####################################

# Installation of RJSplot package
#install.packages("RJSplot")
library(RJSplot)

## Scatterplot of Iris data
scatterplot_rjs(iris[,"Sepal.Width"],
                  iris[,"Sepal.Length",], 
                  col = iris[,"Species"], 
                  pch = as.numeric(iris[,"Species"]), 
                  id = iris[,"Species"], 
                  xlab = "Sepal Width (cm)", 
                  ylab = "Sepal Length (cm)")

## Boxplot
boxplot_rjs(iris[,1:4])

## Density plot
densityplot_rjs(iris[,-5])

## HTLM table of USArrests
tables_rjs(USArrests)

## barplot USArrests
barplot_rjs(USArrests, xlab="states", ylab="arrests")

## Pie chart of deaths causes in Virginia
piechart_rjs(VADeaths)

## 3D scatter plot of Iris data
scatter3d_rjs(iris[,"Sepal.Width"], 
                 iris[,"Sepal.Length"], 
                 iris[,"Petal.Width"], 
                 color = iris[,"Species"], 
                 xlab = "Sepal Width (cm)", 
                 ylab = "Sepal Length (cm)", 
                 zlab = "Petal Width (cm)")

## Surface plot of Volcano data
surface3d_rjs(volcano,color=c("red","green"))

## Interactive Network
# Data preparation: a data.frame with 2 columns is necessary:
# source node, target node and an optional weight for the link
x <- 1-cor(t(mtcars))
source <- rep(rownames(x),nrow(x))
target <- rep(rownames(x),rep(ncol(x),nrow(x)))
links <- data.frame(source=source, target=target, 
                      value=as.vector(x))

# Network creation
# Only links with a weight > 0.1 are represented
# nodes are grouped based on the cyl variable
# node size is proportional to hp variable
# node color is proportional to consumption (mpg)
network_rjs(links[links[,3]>0.1,], mtcars, 
              group = "cyl", size = "hp", color = "mpg")

## Symmetric Heatmap
symheatmap_rjs(links, mtcars, group = "cyl")

## Interactive Heatmap
heatmap_rjs(mtcars, scale="column", color="RdBkGr")

## Wordcloud
# It receives a data frame with words and its frequency
input <- data.frame(rownames(USArrests),USArrests[,4])
wordcloud_rjs(input)



