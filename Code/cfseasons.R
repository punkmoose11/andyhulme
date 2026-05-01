library("XML")
library("readxl")
setwd("C:/Users/andyh/Documents/OMAHASERIES/Code")

games <- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/College Football Games.xlsx",sheet = "Games")
games <- as.data.frame(games) 
games$major <- ifelse((games$statusx!="X" & games$statusx!="Minor")|(games$statusy!="X" & games$statusy!="Minor"),1,0)
games <- games[c("season","date","team1","team2","points1","points2","loc","locn","text","conf","major")]
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

all$resultc <- paste(all$result,all$conf,sep="")

summ <- as.data.frame.matrix(table(paste(all$season,all$team1), all$resultc))

summ$key <- rownames(summ)
summ$season <- substring(summ$key,1,4)
summ$team1  <- substring(summ$key,6,100)

summ <- merge(summ, teams, by=c("season","team1"), all.x=T)

summ$W <- summ$WC + summ$WX + summ$WCR
summ$L <- summ$LC + summ$LX + summ$LCR
summ$T <- summ$TC + summ$TX

summ$WC<- summ$WC + summ$LCR
summ$LC<- summ$LC + summ$WCR
summ$TC<- summ$TC

summ$Pct <- round((summ$W  + summ$T /2) / (summ$W  + summ$L  + summ$T), 2) * 100
summ$PctC<- round((summ$WC + summ$TC/2) / (summ$WC + summ$LC + summ$TC), 2) * 100

summ <- summ[c("season","status","division","team1","WC","LC","TC","PctC","W","L","T","Pct")]
summ <- summ[order(summ$season, summ$status, summ$division, -summ$PctC, -summ$WC, -summ$Pct ), ]

summ$year <- summ$season
summ$team <- summ$team1
summ$conf <- summ$status

export <- summ[c("year","team","WC","LC","TC","W","L","T","conf","division")]
export <- subset(export, is.na(conf)==F)
export <- export[order(export$team, export$year ), ]
write.csv(export,file="../Data/cfseasons.csv",na='')
export$GP <- export$WC+export$LC+export$TC
subset(export[order(export$year, export$conf, export$division, -export$WC+export$LC ), ],conf=="GWC")

x <- as.data.frame(table(subset(all, is.na(status))$team1))
x <- x[order(-x$Freq), ]
#x

#subset(summ, team=="Kennesaw St.")
#write.csv(subset(summ, season>=1995 & W+L+T>7 & is.na(status)),file="C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/fcsteams.csv",na='')
