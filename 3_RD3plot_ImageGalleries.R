############################################
## Image Galleries with netCoin           ##
############################################
library(netCoin)

## Read database with players of La Liga
# Source:
inputfile <- tempfile()
download.file("https://raw.githubusercontent.com/sdelquin/laliga-data/refs/heads/main/datasets/S2324-laliga-players.csv", inputfile)
laliga <- read.csv(inputfile)
laliga <- laliga[laliga$competition=="Classics",]

# Download images if necessary
imagenes <- laliga[,'photo']
naimages <- imagenes==""
imagenes <- paste0('PlayerImages/',laliga[,'id'])
extension <- tools::file_ext(laliga[,'photo'])
imagenes <- paste0(imagenes,".",extension)
imagenes[naimages] <- NA
if(!file.exists("PlayerImages")){
  dir.create("PlayerImages")
}
for(i in seq_len(nrow(laliga))){
  if(laliga[i,'photo']!="" && !file.exists(imagenes[i])){
    Sys.sleep(5)
    download.file(laliga[i,'photo'],imagenes[i])
  }
}

# Select input information
jugadores <- data.frame(slug=laliga$slug,label=laliga$name,image=imagenes,laliga[,c(11:13,17:18,23,56,44:46,50,149)])

## Perform image galleries
# It uses a data.frame as input with this columns: name -> image descriptive name, 
# image -> the image path and label -> the showing name in each image
g <- exhibit(jugadores,name="slug",label="label",image="image", main= "Álbum Ranini",dir="GaleriaLaLiga")
plot(g)


# This code adds an HTML descriptive text for each player
library(tableHTML)
jugadores$information <- apply(jugadores[,-3],1,function(data){
  return(tableHTML(as.data.frame(data[-2]),headers=data[2]))
})

## Perform image galleries
# ntext is a column with information about the image in plain/html format 
g <- exhibit(jugadores,name="slug",label="label",image="image", main= "Álbum Ranini",dir="GaleriaLaLiga",ntext="information")
plot(g)


## Add another gallery adding a teams gallery which is connected with players
# Parse information and download images
teams <- laliga[,c('team','team.foundation','team.shield','team.shortname')]
teams <- unique(teams)
imagenes <- teams[,'team.shield']
naimages <- imagenes==""
imagenes <- paste0('TeamShield/',teams[,'team.shortname'])
extension <- tools::file_ext(teams[,'team.shield'])
imagenes <- paste0(imagenes,".",extension)
imagenes[naimages] <- NA
if(!file.exists("TeamShield")){
  dir.create("TeamShield")
}
for(i in seq_len(nrow(teams))){
  if(teams[i,'team.shield']!="" && !file.exists(imagenes[i])){
    Sys.sleep(5)
    download.file(teams[i,'team.shield'],imagenes[i])
  }
}
teams[,'image'] <- imagenes
teams[,'team.shield'] <- NULL
teams$label <- teams$team

tree <- laliga[,c('team','slug')]
names(tree) <- c("teams","players")
tree <- tree[tree[,'teams']!="",]

## Perform image galleries
# It is necessary a list of images with players and temas, and the relation between them stored in tree (edge list)
g <- netExhibit(tree, nodes=list(teams=teams,players=jugadores), image="image", label="label"
, main= "Multi-Gallery", initialType="teams", ntext="information",dir="GaleriaLaLiga")
plot(g)
