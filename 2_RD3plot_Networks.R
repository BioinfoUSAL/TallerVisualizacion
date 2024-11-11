############################################
## Visual Analytics Networks with RD3plot ##
############################################
if (!require("rD3plot", quietly = TRUE))
 devtools::install_github('Modesto-Escobar/rD3plot')

## Load rD3plot library and example data with the dependencies network of CRAN packages
# It loads 2 data.frames: links and nodes
library(rD3plot)
load("crannetworkdata.Rdata")

## Simple network creation. Columns with edge color, node size, node color and node information are specified for network customization.
net <- network_rd3(links=links, nodes=nodes, lcolor="Type", size="downloadsyear", info="info", color="downloads", dir="CRANnetwork")
plot(net)


## Evolving CRAN network creation
# It is necessary the creation of a list of networks that will be joined in a evolutive network with the evolNetwork_rd3 function
nets <- list()
for(t in (min(links $Year):max(links$Year))){
    sublinks <- links[links$Year<=t,]
    nets[[paste0("year_",t)]] <- network_rd3(links=sublinks, nodes=nodes, lcolor="Type", size="downloads", info="info")
}
net <- evolNetwork_rd3(nets, dir="CRANnetworkEvol") #do.call(evolNetwork_rd3,nets)
plot(net)


## Network with photos as nodes 
# Spotify network from https://github.com/Veruka2021
library(rD3plot)
library(XML)
library(rgexf)

# Download and parse source data
# This file represents a network of related artists on Spotify.
# The file is in .gexf format and contains metadata such as number of followers, popularity, and links to each artist's images.
inputfile <- tempfile()
download.file("https://raw.githubusercontent.com/Veruka2021/Spotify-Network-Artist_2024_gexf/refs/heads/main/Spotify%20Network%20Artist_Python_2024_Vero.gexf", inputfile)
g <- read.gexf(inputfile)

nodes <- g$nodes
edges <- g$edges

edges$weight <- NULL

xmlnet <- xmlParse(g$graph)
xmltop <- xmlRoot(xmlnet)

ids <- sapply(seq_len(nrow(nodes)),function(i){
  return(xmlAttrs(xmltop[['graph']][['nodes']][[i]])[['id']])
})
nodes <- data.frame(id=ids)

# Download Images
dir.create("images")
images <- sapply(seq_len(nrow(nodes)),function(i){
  url <- xmlAttrs(xmltop[['graph']][['nodes']][[i]][['attvalues']][[1]])[['value']]
  filename <- file.path("images",paste0("artist_",i,".jpg"))
  download.file(url, filename, mode = 'wb')
  return(filename)
})
nodes$image <- images

# Parse data from gexf format
genres <- sapply(seq_len(nrow(nodes)),function(i){
  value <- xmlAttrs(xmltop[['graph']][['nodes']][[i]][['attvalues']][[2]])[['value']]
  return(paste0(unlist(strsplit(value,", ",TRUE)),collapse="|"))
})
nodes$genre <- genres

followers <- sapply(seq_len(nrow(nodes)),function(i){
  return(xmlAttrs(xmltop[['graph']][['nodes']][[i]][['attvalues']][[3]])[['value']])
})
nodes$followers <- followers

popularity <- sapply(seq_len(nrow(nodes)),function(i){
  return(xmlAttrs(xmltop[['graph']][['nodes']][[i]][['attvalues']][[4]])[['value']])
})
nodes$popularity <- popularity

nodes$info <- apply(nodes[,-2],1,function(x){
  x[2] <- gsub("|",", ",x[2],fixed=TRUE)
  aux <- paste0('<li><b>',names(x),':</b> ',x,'</li>')
  aux <- paste0(aux,collapse="")
  aux <- paste0("<ul>",aux,"</ul>")
  return(aux)
})

nodes$picture <- nodes$id

# Network creation with images. Image parameter specifies the column with image paths.
net <- network_rd3(nodes, edges, name="id", source="source", target="target", image="image", imageNames="picture", info="info", roundedItems=TRUE, dir="spotifyNetwork")
plot(net)

