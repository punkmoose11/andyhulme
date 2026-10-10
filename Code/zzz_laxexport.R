
library("readxl")

games <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/LAX.xlsx",sheet = "Sheet1")
games <- as.data.frame(games) 

seeds <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/LAX.xlsx",sheet = "Sheet2")
seeds <- as.data.frame(seeds) 

games <- merge(games, seeds, by.x=c("year","winner"), by.y=c("year","team"),all.x=T)
games <- merge(games, seeds, by.x=c("year","loser"),  by.y=c("year","team"),all.x=T)

rd16 <- subset(games, round=="First Round")
rd16$block <- ifelse(is.na(rd16$seed.x),rd16$seed.y,rd16$seed.x)

rd16w <- rd16[c("year","winner","block")]
names(rd16w)[names(rd16w) == "winner"] <- "team"
rd16l <- rd16[c("year","loser" ,"block")]
names(rd16l)[names(rd16l) == "loser" ] <- "team"

blocks <- rbind(rd16w,rd16l)

games <- merge(games, blocks , by.x=c("year","winner"), by.y=c("year","team"),all.x=T)
games <- merge(games, blocks , by.x=c("year","loser"),  by.y=c("year","team"),all.x=T)

games$linex <- ifelse(is.na(games$seed.x),games$block.x,games$seed.x)
games$liney <- ifelse(is.na(games$seed.y),games$block.y,games$seed.y)

games1<-games
names(games1)[names(games1) == "winner"] <- "team1"
names(games1)[names(games1) == "loser"] <- "team2"

games1$gameno <- 0
games1$flip   <- 0
games1$roundn <- 0

for (i in 1:nrow(games1)) {

  if (games1$round[i]=="Opening Round" & games1$linex[i]==1) {games1$gameno[i] <- 502}
  if (games1$round[i]=="Opening Round" & games1$linex[i]==2) {games1$gameno[i] <- 516}
  if (games1$round[i]=="Opening Round") {games1$roundn[i] <- 32}

  if (games1$round[i]=="First Round" & games1$linex[i]==1) {games1$gameno[i] <- 401}
  if (games1$round[i]=="First Round" & games1$linex[i]==8) {games1$gameno[i] <- 402}
  if (games1$round[i]=="First Round" & games1$linex[i]==5) {games1$gameno[i] <- 403}
  if (games1$round[i]=="First Round" & games1$linex[i]==4) {games1$gameno[i] <- 404}
  if (games1$round[i]=="First Round" & games1$linex[i]==3) {games1$gameno[i] <- 405}
  if (games1$round[i]=="First Round" & games1$linex[i]==6) {games1$gameno[i] <- 406}
  if (games1$round[i]=="First Round" & games1$linex[i]==7) {games1$gameno[i] <- 407}
  if (games1$round[i]=="First Round" & games1$linex[i]==2) {games1$gameno[i] <- 408}
  if (games1$round[i]=="First Round" & is.na(games1$seed.x[i])==T) {games1$flip[i] <- 1}
  if (games1$round[i]=="First Round") {games1$roundn[i] <- 16}

  if (games1$round[i]=="Quarterfinals" & games1$linex[i] %in% c(1,8)) {games1$gameno[i] <- 301}
  if (games1$round[i]=="Quarterfinals" & games1$linex[i] %in% c(4,5)) {games1$gameno[i] <- 302}
  if (games1$round[i]=="Quarterfinals" & games1$linex[i] %in% c(3,6)) {games1$gameno[i] <- 303}
  if (games1$round[i]=="Quarterfinals" & games1$linex[i] %in% c(2,7)) {games1$gameno[i] <- 304}
  if (games1$round[i]=="Quarterfinals" & games1$linex[i] %in% c(8,4,6,2)) {games1$flip[i] <- 1}
  if (games1$round[i]=="Quarterfinals") {games1$roundn[i] <- 8}

  if (games1$round[i]=="Semifinals" & games1$linex[i] %in% c(1,8,5,4)) {games1$gameno[i] <- 201}
  if (games1$round[i]=="Semifinals" & games1$linex[i] %in% c(3,6,7,2)) {games1$gameno[i] <- 202}
  if (games1$round[i]=="Semifinals" & games1$linex[i] %in% c(5,4,7,2)) {games1$flip[i] <- 1}
  if (games1$round[i]=="Semifinals") {games1$roundn[i] <- 4}

  if (games1$round[i]=="Championship") {games1$gameno[i] <- 101}
  if (games1$round[i]=="Championship" & games1$linex[i] %in% c(3,6,7,2)) {games1$flip[i] <- 1}
  if (games1$round[i]=="Championship") {games1$roundn[i] <- 1}

}

games1$oteam1 <- ifelse(games1$flip==1,games1$team2,games1$team1)
games1$oteam2 <- ifelse(games1$flip==1,games1$team1,games1$team2)
games1$oscore1 <- ifelse(games1$flip==1,games1$score2,games1$score1)
games1$oscore2 <- ifelse(games1$flip==1,games1$score1,games1$score2)

games1 <- games1[order(games1$year, -games1$roundn, games1$gameno),]
games1.x <- games1[c("year","round","roundn","gameno","oteam1","oscore1","oteam2","oscore2","ot")]


games1 <- games1[order(games1$year, -games1$roundn, games1$gameno),]






