if (!require("XML", quietly = TRUE))
 install.packages("XML")

library(XML)

# Get package data
if(!file.exists("packages.rds")){
  download.file("http://cran.r-project.org/web/packages/packages.rds", "packages.rds")
}
packages <- as.data.frame(readRDS("packages.rds"), row.names=NA)
packages <- packages[!duplicated(packages$Package), c("Package","Depends","Imports","Suggests","Title","Description","Published")]

for(i in c("Depends","Imports","Suggests","Title","Description")){
  packages[[i]] <- gsub("\n","",packages[[i]])
}

# Get first release year
if(!file.exists("Year.rds")){
  Year <- numeric(nrow(packages))
  for(i in seq_len(nrow(packages))){
    pkgdate <- NA
    tryCatch({
      download.file(paste0("https://cran.r-project.org/src/contrib/Archive/",packages[i,"Package"]), "archive.html")
      archive <- htmlParse("archive.html")
      pkgdate <- xmlValue(getNodeSet(archive, '//table/tr/td[3]')[[2]])
    }, warning=function(e){ }, error=function(e){ })
    if(is.na(pkgdate)){
      pkgdate <- packages[i,"Published"]
    }
    Year[i] <- as.numeric(format(as.Date(pkgdate),"%Y"))
  }
  file.remove("archive.html")
  saveRDS(Year,"Year.rds")
}else{
  Year <- readRDS("Year.rds")
}
packages$Published <- NULL
packages$Year <- Year

# Get number of downloads
if(!file.exists("rpackages.html")){
  download.file("https://www.datasciencemeta.com/rpackages", "rpackages.html")
}
rpackages <- htmlParse("rpackages.html")
pcknames <- getNodeSet(rpackages, '//table/tbody/tr/td[2]')
pcknames <- sapply(pcknames,xmlValue)
ndownloads <- getNodeSet(rpackages, '//table/tbody/tr/td[3]')
ndownloads <- sapply(ndownloads,xmlValue)
ndownloads <- gsub(",","",ndownloads)
ndownloads <- as.numeric(ndownloads)
downloads <- data.frame(Package=pcknames,downloads=ndownloads)
packages <- merge(packages,downloads,by="Package")

saveRDS(packages,"package_data.rds")


## Create data frames of nodes and links
packages <- readRDS("package_data.rds")

getDepDF <- function(data,i,type){
  aux <- data[i,type]
  if(!is.na(aux)){
    aux <- gsub(" ","",aux)
    aux <- gsub("\\(.+\\)","",aux)
    aux <- unlist(strsplit(aux,",",fixed=TRUE))
    return(data.frame(Source=data[i,"Package"], Target=aux, Type=type))
  }
}

links <- data.frame()
packages$downloadsyear <- round(packages[,"downloads"] / (2025 - packages[,"Year"]))

for(i in seq_len(nrow(packages))){
  if(packages[i,"downloadsyear"]>50000){
    links <- rbind(links,getDepDF(packages,i,"Depends"))
    links <- rbind(links,getDepDF(packages,i,"Imports"))
#    links <- rbind(links,getDepDF(packages,i,"Suggests"))
  }
}
links <- merge(links,packages[,c("Package","downloadsyear"),drop=FALSE],by.x="Target",by.y="Package")
links <- links[links$downloadsyear>50000,]
links <- links[,c("Source","Target","Type")]

nodes <- merge(packages,data.frame(Package=unique(c(links$Source,links$Target))),by="Package")[,c("Package","downloads","Title","Description","Year","downloadsyear")]

nodes$info <- paste0('<h2 style="background: white; color: rgb(40%, 40%, 40%); font-family: monospace; font-size: large;">',nodes$Title,'</h2><p>',nodes$Description,'</p>')


## Create a table with links along the time
linkstemp <- merge(links,packages[,c("Package","Year"),drop=FALSE],by.x="Target",by.y="Package")
linkstemp <- merge(linkstemp ,packages[,c("Package","Year"),drop=FALSE],by.x="Source",by.y="Package")
linkstemp $Year <- apply(linkstemp,1,function(x){
  if(x[5]>x[4]){
    return(x[5])
  }
  return(x[4])
})
links <- linkstemp[,c(1,2,3,6)]

save(nodes, links, file="crannetworkdata.Rdata")



