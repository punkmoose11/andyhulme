
library("readxl")

games <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/ncaah.xlsx",sheet = "Sheet2")
games <- as.data.frame(games) 

seeds <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/ncaah.xlsx",sheet = "Sheet3")
seeds <- as.data.frame(seeds) 

games <- merge(games, seeds, by.x=c("year","team1"),by.y=c("year","team"),all.x=T)
games <- merge(games, seeds, by.x=c("year","team2"),by.y=c("year","team"),all.x=T)

natseeds <- subset(seeds, is.na(natseed)==F)[c("year","region","natseed")]
games <- merge(games, natseeds, by.x=c("year","region.x"),by.y=c("year","region"),all.x=T)

games$gameno <- 0
games$flip   <- 0
games$round2 <- 0

for (i in 1:nrow(games)) {

if (games$year[i]<=1980) {
  if (games$round[i]==4 & games$region.x[i]=="east" & games$seed.x[i]==1) {games$gameno[i] <- 201}
  if (games$round[i]==4 & games$region.x[i]=="west" & games$seed.x[i]>=2) {games$gameno[i] <- 201; games$flip[i] <- 1}
  if (games$round[i]==4 & games$region.x[i]=="west" & games$seed.x[i]==1) {games$gameno[i] <- 202}
  if (games$round[i]==4 & games$region.x[i]=="east" & games$seed.x[i]>=2) {games$gameno[i] <- 202; games$flip[i] <- 1}

  if (games$round[i]==1) {games$gameno[i] <- 101}
  if (games$round[i]==1 & games$region.x[i]=="west" & games$seed.x[i]==1) {games$flip[i] <- 1}
  if (games$round[i]==1 & games$region.x[i]=="east" & games$seed.x[i]>=2) {games$flip[i] <- 1}

  if (games$round[i]==3) {games$gameno[i] <- 150}
  if (games$round[i]==3 & games$region.x[i]=="west" & games$seed.x[i]==1) {games$flip[i] <- 1}
  if (games$round[i]==3 & games$region.x[i]=="east" & games$seed.x[i]>=2) {games$flip[i] <- 1}

  if (games$round[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==3) {games$gameno[i] <- 302}
  if (games$round[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==2) {games$gameno[i] <- 302; games$flip[i] <- 1}
  if (games$round[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==3) {games$gameno[i] <- 304}
  if (games$round[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==2) {games$gameno[i] <- 304; games$flip[i] <- 1}
}

if (games$year[i]>=1981 & games$year[i]<=1991) {

  if (games$round[i] %in% c(71,72,80,81,82)) {games$round2[i] <- 8}
  if (games$round[i] %in% c(78,79,87,88,89)) {games$round2[i] <- 16}

  if (games$round2[i]==16 & games$region.x[i]=="west" & games$seed.x[i]==4) {games$gameno[i] <- 402}
  if (games$round2[i]==16 & games$region.x[i]=="east" & games$seed.x[i]==5) {games$gameno[i] <- 402; games$flip[i] <- 1}
  if (games$round2[i]==16 & games$region.x[i]=="east" & games$seed.x[i]==3) {games$gameno[i] <- 403}
  if (games$round2[i]==16 & games$region.x[i]=="west" & games$seed.x[i]==6) {games$gameno[i] <- 403; games$flip[i] <- 1}
  if (games$round2[i]==16 & games$region.x[i]=="east" & games$seed.x[i]==4) {games$gameno[i] <- 406}
  if (games$round2[i]==16 & games$region.x[i]=="west" & games$seed.x[i]==5) {games$gameno[i] <- 406; games$flip[i] <- 1}
  if (games$round2[i]==16 & games$region.x[i]=="west" & games$seed.x[i]==3) {games$gameno[i] <- 407}
  if (games$round2[i]==16 & games$region.x[i]=="east" & games$seed.x[i]==6) {games$gameno[i] <- 407; games$flip[i] <- 1}

  if (games$round2[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==1) {games$gameno[i] <- 301}
  if (games$round2[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==4) {games$gameno[i] <- 301; games$flip[i] <- 1}
  if (games$round2[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==5) {games$gameno[i] <- 301; games$flip[i] <- 1}
  if (games$round2[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==3) {games$gameno[i] <- 302}
  if (games$round2[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==6) {games$gameno[i] <- 302}
  if (games$round2[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==2) {games$gameno[i] <- 302; games$flip[i] <- 1}
  if (games$round2[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==1) {games$gameno[i] <- 303}
  if (games$round2[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==4) {games$gameno[i] <- 303; games$flip[i] <- 1}
  if (games$round2[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==5) {games$gameno[i] <- 303; games$flip[i] <- 1}
  if (games$round2[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==3) {games$gameno[i] <- 304}
  if (games$round2[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==6) {games$gameno[i] <- 304}
  if (games$round2[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==2) {games$gameno[i] <- 304; games$flip[i] <- 1}

  if (games$round[i]==4 & games$region.x[i]=="east" & games$seed.x[i]==1) {games$gameno[i] <- 201}
  if (games$round[i]==4 & games$region.x[i]=="west" & games$seed.x[i]==4) {games$gameno[i] <- 201}
  if (games$round[i]==4 & games$region.x[i]=="east" & games$seed.x[i]==5) {games$gameno[i] <- 201}
  if (games$round[i]==4 & games$region.x[i]=="east" & games$seed.x[i]==3) {games$gameno[i] <- 201; games$flip[i] <- 1}
  if (games$round[i]==4 & games$region.x[i]=="west" & games$seed.x[i]==6) {games$gameno[i] <- 201; games$flip[i] <- 1}
  if (games$round[i]==4 & games$region.x[i]=="west" & games$seed.x[i]==2) {games$gameno[i] <- 201; games$flip[i] <- 1}
  if (games$round[i]==4 & games$region.x[i]=="west" & games$seed.x[i]==1) {games$gameno[i] <- 202}
  if (games$round[i]==4 & games$region.x[i]=="east" & games$seed.x[i]==4) {games$gameno[i] <- 202}
  if (games$round[i]==4 & games$region.x[i]=="west" & games$seed.x[i]==5) {games$gameno[i] <- 202}
  if (games$round[i]==4 & games$region.x[i]=="west" & games$seed.x[i]==3) {games$gameno[i] <- 202; games$flip[i] <- 1}
  if (games$round[i]==4 & games$region.x[i]=="east" & games$seed.x[i]==6) {games$gameno[i] <- 202; games$flip[i] <- 1}
  if (games$round[i]==4 & games$region.x[i]=="east" & games$seed.x[i]==2) {games$gameno[i] <- 202; games$flip[i] <- 1}

  if (games$round[i]==1) {games$gameno[i] <- 101}
  if (games$round[i]==1 & games$region.x[i]=="west" & games$seed.x[i] %in% c(1,3,5)) {games$flip[i] <- 1}
  if (games$round[i]==1 & games$region.x[i]=="east" & games$seed.x[i] %in% c(2,4,6)) {games$flip[i] <- 1}

  if (games$round[i]==3) {games$gameno[i] <- 150}
  if (games$round[i]==1 & games$region.x[i]=="west" & games$seed.x[i] %in% c(1,3,5)) {games$flip[i] <- 1}
  if (games$round[i]==1 & games$region.x[i]=="east" & games$seed.x[i] %in% c(2,4,6)) {games$flip[i] <- 1}
}

if (games$year[i]>=1992 & games$year[i]<=2002) {

  if (games$round[i]==16 & games$region.x[i]=="east" & games$seed.x[i]==4) {games$gameno[i] <- 402}
  if (games$round[i]==16 & games$region.x[i]=="east" & games$seed.x[i]==5) {games$gameno[i] <- 402; games$flip[i] <- 1}
  if (games$round[i]==16 & games$region.x[i]=="west" & games$seed.x[i]==3) {games$gameno[i] <- 403}
  if (games$round[i]==16 & games$region.x[i]=="west" & games$seed.x[i]==6) {games$gameno[i] <- 403; games$flip[i] <- 1}
  if (games$round[i]==16 & games$region.x[i]=="west" & games$seed.x[i]==4) {games$gameno[i] <- 406}
  if (games$round[i]==16 & games$region.x[i]=="west" & games$seed.x[i]==5) {games$gameno[i] <- 406; games$flip[i] <- 1}
  if (games$round[i]==16 & games$region.x[i]=="east" & games$seed.x[i]==3) {games$gameno[i] <- 407}
  if (games$round[i]==16 & games$region.x[i]=="east" & games$seed.x[i]==6) {games$gameno[i] <- 407; games$flip[i] <- 1}

  if (games$round[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==1) {games$gameno[i] <- 301}
  if (games$round[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==4) {games$gameno[i] <- 301; games$flip[i] <- 1}
  if (games$round[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==5) {games$gameno[i] <- 301; games$flip[i] <- 1}
  if (games$round[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==3) {games$gameno[i] <- 302}
  if (games$round[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==6) {games$gameno[i] <- 302}
  if (games$round[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==2) {games$gameno[i] <- 302; games$flip[i] <- 1}
  if (games$round[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==1) {games$gameno[i] <- 303}
  if (games$round[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==4) {games$gameno[i] <- 303; games$flip[i] <- 1}
  if (games$round[i]==8 & games$region.x[i]=="west" & games$seed.x[i]==5) {games$gameno[i] <- 303; games$flip[i] <- 1}
  if (games$round[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==3) {games$gameno[i] <- 304}
  if (games$round[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==6) {games$gameno[i] <- 304}
  if (games$round[i]==8 & games$region.x[i]=="east" & games$seed.x[i]==2) {games$gameno[i] <- 304; games$flip[i] <- 1}

  if (games$round[i]==4 & games$region.x[i]=="east" & games$seed.x[i] %in% c(1,4,5)) {games$gameno[i] <- 201}
  if (games$round[i]==4 & games$region.x[i]=="west" & games$seed.x[i] %in% c(2,3,6)) {games$gameno[i] <- 201; games$flip[i] <- 1}
  if (games$round[i]==4 & games$region.x[i]=="west" & games$seed.x[i] %in% c(1,4,5)) {games$gameno[i] <- 202}
  if (games$round[i]==4 & games$region.x[i]=="east" & games$seed.x[i] %in% c(2,3,6)) {games$gameno[i] <- 202; games$flip[i] <- 1}

  if (games$round[i]==1) {games$gameno[i] <- 101}
  if (games$round[i]==1 & games$region.x[i]=="west" & games$seed.x[i] %in% c(1,4,5)) {games$flip[i] <- 1}
  if (games$round[i]==1 & games$region.x[i]=="east" & games$seed.x[i] %in% c(2,3,6)) {games$flip[i] <- 1}

  if (games$round[i]==3) {games$gameno[i] <- 150}
  if (games$round[i]==1 & games$region.x[i]=="west" & games$seed.x[i] %in% c(1,4,5)) {games$flip[i] <- 1}
  if (games$round[i]==1 & games$region.x[i]=="east" & games$seed.x[i] %in% c(2,3,6)) {games$flip[i] <- 1}
}

if (games$year[i]>=2003 & games$year[i]<=9999) {

  if (games$round[i]==16 & games$natseed[i]==1) {games$gameno[i] <- 401}
  if (games$round[i]==16 & games$natseed[i]==4) {games$gameno[i] <- 403}
  if (games$round[i]==16 & games$natseed[i]==3) {games$gameno[i] <- 405}
  if (games$round[i]==16 & games$natseed[i]==2) {games$gameno[i] <- 407}
  if (games$round[i]==16 & games$seed.x[i]==1) {games$gameno[i] <- games$gameno[i]}
  if (games$round[i]==16 & games$seed.x[i]==4) {games$gameno[i] <- games$gameno[i]; games$flip[i] <- 1}
  if (games$round[i]==16 & games$seed.x[i]==3) {games$gameno[i] <- games$gameno[i]+1}
  if (games$round[i]==16 & games$seed.x[i]==2) {games$gameno[i] <- games$gameno[i]+1; games$flip[i] <- 1}

  if (games$round[i]==8 & games$natseed[i]==1) {games$gameno[i] <- 301}
  if (games$round[i]==8 & games$natseed[i]==4) {games$gameno[i] <- 302}
  if (games$round[i]==8 & games$natseed[i]==3) {games$gameno[i] <- 303}
  if (games$round[i]==8 & games$natseed[i]==2) {games$gameno[i] <- 304}
  if (games$round[i]==8 & games$seed.x[i] %in% c(2,3)) {games$flip[i] <- 1}

  if (games$round[i]==4 & games$natseed[i]==1) {games$gameno[i] <- 201}
  if (games$round[i]==4 & games$natseed[i]==4) {games$gameno[i] <- 201; games$flip[i] <- 1}
  if (games$round[i]==4 & games$natseed[i]==3) {games$gameno[i] <- 202}
  if (games$round[i]==4 & games$natseed[i]==2) {games$gameno[i] <- 202; games$flip[i] <- 1}

  if (games$round[i]==1) {games$gameno[i] <- 101}
  if (games$round[i]==1 & games$natseed[i] %in% c(2,3)) {games$flip[i] <- 1}
}

}

games$oteam1 <- ifelse(games$flip==1,games$team2,games$team1)
games$oteam2 <- ifelse(games$flip==1,games$team1,games$team2)
games$oscore1 <- ifelse(games$flip==1,games$score2,games$score1)
games$oscore2 <- ifelse(games$flip==1,games$score1,games$score2)

games <- games[order(games$year, -games$round, games$gameno),]
games.x <- games[c("year","name1","round","gameno","oteam1","oscore1","oteam2","oscore2","notes")]


