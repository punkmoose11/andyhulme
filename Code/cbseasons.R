library("XML")
library("readxl")
setwd("C:/Users/andyh/Documents/OMAHASERIES/Code")

games <- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/College Baseball Games.xlsx",sheet = "Games")
games <- as.data.frame(games) 

games <- games[c("Year","Date","Team1","Team2","Runs1","Runs2","Conf")]

teams <- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/College Baseball Games.xlsx",sheet = "Teams")
teams <- as.data.frame(teams) 
teams <- teams[c("Year","School","Conference","Division")]

games.1 <- games 
games.1$n <- 1

games.2 <- games
games.2$n <- 2

games.2$Team1 <- games$Team2
games.2$Team2 <- games$Team1
games.2$Runs1 <- games$Runs2
games.2$Runs2 <- games$Runs1

all <- rbind(games.1,games.2)

all <- merge(all, teams, by.x=c("Year","Team1"), by.y=c("Year","School"), all.x=T)

all$Conf <- ifelse(is.na(all$Conf),"X",all$Conf)

all$Result  <- ifelse(all$Runs1==all$Runs2,"T",ifelse(all$Runs1>all$Runs2,"W","L"))
all$Result  <- ifelse(all$Runs1==998 & all$Runs2==998,"L",all$Result)

all$Resultc <- paste(all$Result,all$Conf,sep="")

summ <- as.data.frame.matrix(table(paste(all$Year,all$Team1), all$Resultc))

summ$key <- rownames(summ)
summ$Year   <- substring(summ$key,1,4)
summ$School <- substring(summ$key,6,100)

summ <- merge(summ, teams, by=c("Year","School"), all.y=T)

summ$W <- summ$WC + summ$WX + summ$WT + summ$WN
summ$L <- summ$LC + summ$LX + summ$LT + summ$LN
summ$T <- summ$TC + summ$TX

summ$Pct <- round((summ$W  + summ$T /2) / (summ$W  + summ$L  + summ$T), 3) * 1000
summ$PctC<- round((summ$WC + summ$TC/2) / (summ$WC + summ$LC + summ$TC), 3) * 1000
summ$PC <- summ$WC + summ$LC + summ$TC

summ$ConfT <- ifelse(summ$WT+summ$LT>0,paste(summ$WT,summ$LT,sep="&"),".")
summ$NcaaT <- ifelse(summ$WN+summ$LN>0,paste(summ$WN,summ$LN,sep="&"),".")

summ <- summ[c("Year","Conference","Division","School","PC","WC","LC","TC","W","L","T","WT","LT","WN","LN")]
summ <- summ[order(summ$Year, summ$Conference, summ$Division, summ$School), ]

write.csv(summ,file="C:/Users/andyh/Documents/OMAHASERIES/Data/cbseasons.csv",quote = FALSE, row.names = FALSE, na="")
###

