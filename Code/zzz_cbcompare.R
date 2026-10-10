library("XML")
library("readxl")
setwd("C:/Users/andyh/Documents/OMAHASERIES/Code")

games <- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/College Baseball Games.xlsx",sheet = "Games")
games <- as.data.frame(games) 

games <- subset(games, Year!=1998 | (is.na(Conf1)==F & is.na(Conf2)==F) )

game<- games[c("Year","Date")]
game$Team1 <- ifelse(games$Runs1>games$Runs2,games$Team1,games$Team2)
game$Team2 <- ifelse(games$Runs1>games$Runs2,games$Team2,games$Team1)
game$Runs1 <- ifelse(games$Runs1>games$Runs2,games$Runs1,games$Runs2)
game$Runs2 <- ifelse(games$Runs1>games$Runs2,games$Runs2,games$Runs1)

boyd<- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/boyd.xlsx",sheet = "scores")
boyd<- as.data.frame(boyd) 

boyd<- boyd[c("Year","Date","Team1","Team2","Runs1","Runs2")]

game <- subset(game, Year==1997 & Runs1>Runs2)
boyd <- subset(boyd, Year==1997)
game$x<-1
boyd$y<-1

comb <- merge(game, boyd, by=c("Year","Team1","Team2","Runs1","Runs2"),all=T)

subset(comb, is.na(y))
subset(comb, is.na(x))


