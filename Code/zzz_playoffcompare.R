library("XML")
library("readxl")
setwd("C:/Users/andyh/Documents/OMAHASERIES/Code")

fbs <- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/College Football Games.xlsx",sheet = "Games")
fbs <- as.data.frame(fbs) 

fcs <- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/Football Playoffs.xlsx",sheet = "Import")
fcs <- as.data.frame(fcs) 

fbs$a <- ifelse(fbs$points1>=fbs$points2,fbs$team1,fbs$team2)
fbs$b <- ifelse(fbs$points1>=fbs$points2,fbs$team2,fbs$team1)
fbs$c <- ifelse(fbs$points1>=fbs$points2,fbs$points1,fbs$points2)
fbs$d <- ifelse(fbs$points1>=fbs$points2,fbs$points2,fbs$points1)
fbs$x <- 1
fbs$flip <- ifelse(fbs$points1>=fbs$points2,0,1)

fbs <- fbs[order(fbs$season, fbs$a, fbs$b, fbs$c, fbs$d, fbs$date), ]

fcs$a <- ifelse(fcs$points1>=fcs$points2,fcs$team1,fcs$team2)
fcs$b <- ifelse(fcs$points1>=fcs$points2,fcs$team2,fcs$team1)
fcs$c <- ifelse(fcs$points1>=fcs$points2,fcs$points1,fcs$points2)
fcs$d <- ifelse(fcs$points1>=fcs$points2,fcs$points2,fcs$points1)
fcs$y <- 1
fcs$flip <- ifelse(fcs$points1>=fcs$points2,0,1)

fcs <- fcs[order(fcs$season, fcs$a, fcs$b, fcs$c, fcs$d, fcs$date), ]

recon <- merge(fbs, fcs, by=c("season","a","b","c","d"),all=T)

#subset(recon, x==1 & is.na(y))[c("season","a","b","c","d")]

#subset(recon, x==1 & y==1 & date.x!=date.y)[c("season","a","b","c","d","date.x","date.y","text.x","text.y")]

subset(recon, x==1 & grepl("D3",text.x) | y==1 & grepl("D3",text.y))[c("season","a","b","c","d","date.x","date.y")]

output1 <- subset(recon, x==1 & y==1)[c("season","date.x","team1.x","team2.x","points1.x","points2.x","locn.x","locn.y","text.x","text.y","ot.y")]

recon$row <- 8888888
output2 <- subset(recon, y==1)[c("row","season","date.y","team1.y","team2.y","points1.y","points2.y","ot.y","loc.y","locn.y","text.y",
						"date.x","team1.x","team2.x","points1.x","points2.x","ot.x","loc.x","locn.x","text.x" )]

nrow(fcs)
nrow(recon)
nrow(output1)
nrow(output2)

write.csv(output2,file="C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/playoffs.csv",na='')
#write.csv(output1,file="C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/fcs_2.csv",na='')


