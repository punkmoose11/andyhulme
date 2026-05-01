library("XML")
library("readxl")
setwd("C:/Users/andyh/Documents/OMAHASERIES/Code")

fbs <- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/College Football Games.xlsx",sheet = "Games")
fbs <- as.data.frame(fbs) 

fcs <- read_excel("C:/Users/andyh/Downloads/MoState.xlsx",sheet = "Sheet4")
fcs <- as.data.frame(fcs) 

fbs$a <- ifelse(fbs$points1>=fbs$points2 | (fbs$points1==fbs$points2 & fbs$team1>fbs$team2),fbs$team1,fbs$team2)
fbs$b <- ifelse(fbs$points1>=fbs$points2 | (fbs$points1==fbs$points2 & fbs$team1>fbs$team2),fbs$team2,fbs$team1)
fbs$c <- ifelse(fbs$points1>=fbs$points2 | (fbs$points1==fbs$points2 & fbs$team1>fbs$team2),fbs$points1,fbs$points2)
fbs$d <- ifelse(fbs$points1>=fbs$points2 | (fbs$points1==fbs$points2 & fbs$team1>fbs$team2),fbs$points2,fbs$points1)
fbs$x <- 1
fbs$flip <- ifelse(fbs$points1>=fbs$points2,0,1)

fbs <- fbs[order(fbs$season, fbs$a, fbs$b, fbs$c, fbs$d, fbs$date), ]

fcs$season <- fcs$Season
fcs$date   <- fcs$Date
fcs$a <- ifelse(fcs$Score1>=fcs$Score2 | (fcs$Score1==fcs$Score2 & fcs$Team1>fcs$Team2),fcs$Team1,fcs$Team2)
fcs$b <- ifelse(fcs$Score1>=fcs$Score2 | (fcs$Score1==fcs$Score2 & fcs$Team1>fcs$Team2),fcs$Team2,fcs$Team1)
fcs$c <- ifelse(fcs$Score1>=fcs$Score2 | (fcs$Score1==fcs$Score2 & fcs$Team1>fcs$Team2),fcs$Score1,fcs$Score2)
fcs$d <- ifelse(fcs$Score1>=fcs$Score2 | (fcs$Score1==fcs$Score2 & fcs$Team1>fcs$Team2),fcs$Score2,fcs$Score1)
fcs$y <- 1
fcs$flip <- ifelse(fcs$Score1>=fcs$Score2,0,1)

fcs <- subset(fcs,season<1995)

recon1<- merge(fbs, fcs, by=c("season","a","b"),all=T)
recon2<- merge(fbs, fcs, by=c("season","a","b","c","d"),all=T)

subset(recon1, x==1 & y==1 & (c.x!=c.y | d.x!=d.y))[c("season","a","b","c.x","d.x","c.y","d.y")]
subset(recon2, x==1 & y==1 & date.x!=date.y)[c("season","a","b","c","d","date.x","date.y")]
subset(recon2, x==1 & y==1 & loc!=Loc & flip.x==flip.y)[c("season","a","b","c","d","loc","Loc","flip.x","flip.y")]
subset(recon2, x==1 & y==1 & (locn!=Locx | is.na(text)==F))[c("season","a","b","c","d","locn","Locx","text")]

#subset(recon2, y==1 & is.na(x))[c("season","a","b","c","d")]

output1 <- subset(recon2, x==1 & y==1)[c("season","date.y","Team1","Team2","Score1","Score2","ot","Loc","Locx","text" )]

recon2$row <- 7770001
output2 <- subset(recon2, y==1 & is.na(x))[c("row","season","date.y","Team1","Team2","Score1","Score2","ot","Loc","Locx","text" )]

nrow(fcs)
nrow(recon)
nrow(output1)
nrow(output2)

#write.csv(output2,file="C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/fcs.csv",na='')
write.csv(output2,file="C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/mostdel.csv",na='')

######

games <- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/College Football Games.xlsx",sheet = "Games")
games <- as.data.frame(games) 
games$major <- ifelse((games$statusx!="X" & games$statusx!="Minor")|(games$statusy!="X" & games$statusy!="Minor"),1,0)
games <- games[c("game","season","date","team1","team2","points1","points2","loc","locn","text","conf","major")]
games <- subset(games, is.na(points1)==F)

teams <- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/College Football Games.xlsx",sheet = "Seasons")
teams <- as.data.frame(teams) 
teams <- teams[c("season","team1","status","division")]

cfgames <- read.csv("../Data/cfgames.csv")
elo.1 <- subset(cfgames, rtg1!=-9999)[c("season","team1")]
elo.2 <- subset(cfgames, rtg2!=-9999)[c("season","team2")]; names(elo.2) <- c("season","team1");
elo <- rbind(elo.1,elo.2)
elo <- unique(elo[,c("season","team1")])
teams <- merge(elo, teams, by=c("season","team1"), all=T)
teams$status <- ifelse(is.na(teams$status),"Partial",teams$status)

games.1 <- games
games.1$n <- 1
games.1$conf <- ifelse(games.1$conf=="C1","C",games.1$conf)
games.1$conf <- ifelse(games.1$conf=="C2","X",games.1$conf)
games.2 <- games
games.2$n <- 2
games.2$conf <- ifelse(games.2$conf=="C1","X",games.2$conf)
games.2$conf <- ifelse(games.2$conf=="C2","C",games.2$conf)

games.2$team1 <- games$team2
games.2$team2 <- games$team1
games.2$points1 <- games$points2
games.2$points2 <- games$points1

all <- rbind(games.1,games.2)

all <- merge(all, teams, by=c("season","team1"), all.x=T)
#all <- subset(all, major==1)

all$result  <- ifelse(all$points1==all$points2,"T",ifelse(all$points1>all$points2,"W","L"))

all$result <- ifelse(all$n==1 & all$conf=="CF1","L",all$result)
all$result <- ifelse(all$n==2 & all$conf=="CF1","W",all$result)
all$conf   <- ifelse(all$n==1 & all$conf=="CF1","C",all$conf)
all$conf   <- ifelse(all$n==2 & all$conf=="CF1","C",all$conf)

all$result <- ifelse(all$n==1 & all$conf=="CF2","W",all$result)
all$result <- ifelse(all$n==2 & all$conf=="CF2","L",all$result)
all$conf   <- ifelse(all$n==1 & all$conf=="CF2","C",all$conf)
all$conf   <- ifelse(all$n==2 & all$conf=="CF2","C",all$conf)

all$resultc <- paste(all$result,sep="")

summ <- as.data.frame.matrix(table(paste(all$season,all$team1), all$resultc))
summ$key <- rownames(summ)
summ$season <- substring(summ$key,1,4)
summ$team1  <- substring(summ$key,6,100)
summ <- merge(summ, teams, by=c("season","team1"), all.x=T)
summ <- subset(summ, status != "Partial" & is.na(status)==F)

all1 <- subset(all, game!=7770001 | is.na(game))
summ1 <- as.data.frame.matrix(table(paste(all1$season,all1$team1), all1$resultc))
summ1$key <- rownames(summ1)
summ1$season <- substring(summ1$key,1,4)
summ1$team1  <- substring(summ1$key,6,100)
summ1 <- merge(summ1, teams, by=c("season","team1"), all.x=T)
summ1 <- subset(summ1, status != "Partial" & is.na(status)==F)

compares <- merge(summ,summ1,by=c("season","team1"),all=T)
compares <- compares[c("season","team1","W.x","L.x","T.x","W.y","L.y","T.y")]

subset(compares, W.x!=W.y|L.x!=L.y|T.x!=T.y)

alltime <- as.data.frame.matrix(table(paste(all$team1), all$resultc))



