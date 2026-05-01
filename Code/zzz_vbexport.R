
library("readxl")

seedsort <- function(seed) {
	if (is.na(seed))  {seedn<-0}
	else if (seed==1) {seedn<-1}
	else if (seed==16){seedn<-2}
	else if (seed==9) {seedn<-3}
	else if (seed==8) {seedn<-4}
	else if (seed==5) {seedn<-5}
	else if (seed==12){seedn<-6}
	else if (seed==13) {seedn<-7}
	else if (seed==4){seedn<-8}
	else if (seed==3) {seedn<-9}
	else if (seed==14){seedn<-10}
	else if (seed==11) {seedn<-11}
	else if (seed==6){seedn<-12}
	else if (seed==7) {seedn<-13}
	else if (seed==10){seedn<-14}
	else if (seed==15) {seedn<-15}
	else if (seed==2){seedn<-16}
}

games <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/ncaavb.xlsx",sheet = "Sheet2")
games <- as.data.frame(games) 

seeds <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/ncaavb.xlsx",sheet = "Sheet3")
seeds <- as.data.frame(seeds) 

games1 <- subset(games, year>=1993)

rd64 <- subset(games1, round==64)
rd32 <- subset(games1, round==32)

rd32$quart <- paste(rd32$year,rd32$team1,rd32$team2,sep="/")

rd32.1 <- rd32[c("year","team1","quart")]; names(rd32.1)[names(rd32.1) == "team1"] <- "team";
rd32.2 <- rd32[c("year","team2","quart")]; names(rd32.2)[names(rd32.2) == "team2"] <- "team";
rd32x <- rbind(rd32.1,rd32.2)
rd32x <- merge(rd32x, seeds[c("year","team","natseed")],by=c("year","team"),all.x=T)
names(rd32x)[names(rd32x) == "natseed"] <- "nat32"
rd32x <- merge(rd32x[c("year","team","quart")], subset(rd32x[c("year","quart","nat32")], is.na(nat32)==F), by=c("year","quart"), all.x=T)

rd64 <- merge(rd64, rd32x, by.x=c("year","team1"),by.y=c("year","team"),all.x=T)

rd64.1 <- rd64[c("year","team1","quart","nat32")]; names(rd64.1)[names(rd64.1) == "team1"] <- "team";
rd64.2 <- rd64[c("year","team2","quart","nat32")]; names(rd64.2)[names(rd64.2) == "team2"] <- "team";
rd64x <- rbind(rd64.1,rd64.2)

rd64x <- merge(rd64x, seeds[c("year","team","natseed")],by=c("year","team"),all.x=T)

rd64u <- subset(rd64x[c("quart","natseed")],is.na(natseed)==F)
names(rd64u)[names(rd64u) == "natseed"] <- "quartet"

rd64y <- merge(rd64x, rd64u, by="quart", all.x=T)

rd64y$quartet <- ifelse(is.na(rd64y$quartet)==T,rd64y$nat32,rd64y$quartet)

games1 <- merge(games1, rd64y[c("year","team","quartet")],by.x=c("year","team1"),by.y=c("year","team"),all.x=T)
names(games1)[names(games1) == "quartet"] <- "quartet1"
games1 <- merge(games1, rd64y[c("year","team","quartet")],by.x=c("year","team2"),by.y=c("year","team"),all.x=T)
names(games1)[names(games1) == "quartet"] <- "quartet2"

games1 <- merge(games1, seeds[c("year","team","natseed")],by.x=c("year","team1"),by.y=c("year","team"),all.x=T)
names(games1)[names(games1) == "natseed"] <- "natseed1"
games1 <- merge(games1, seeds[c("year","team","natseed")],by.x=c("year","team2"),by.y=c("year","team"),all.x=T)
names(games1)[names(games1) == "natseed"] <- "natseed2"

games1$quartet1 <- ifelse(is.na(games1$quartet1),games1$natseed1,games1$quartet1)
games1$quartet2 <- ifelse(is.na(games1$quartet2),games1$natseed2,games1$quartet2)

games1$quartet1 <- sapply(games1$quartet1, seedsort)
games1$quartet2 <- sapply(games1$quartet2, seedsort)

games1$natseed1 <- ifelse(is.na(games1$natseed1),0,games1$natseed1)
games1$natseed2 <- ifelse(is.na(games1$natseed2),0,games1$natseed2)

games1$winner <- ifelse(games1$round==64 & games1$natseed1+games1$natseed2>0,games1$team1,"")
rd64z <- subset(games1,winner != "")[c("year","quartet1","winner")]; names(rd64z)[names(rd64z) == "winner"] <- "natwin";
games1 <- merge(games1, rd64z, by=c("year","quartet1"),all.x=T); 
games1$natwin <- ifelse(is.na(games1$natwin),"",games1$natwin)


games1$gameno <- 0
games1$flip   <- 0

for (i in 1:nrow(games1)) {

  if (games1$round[i]==64) {games1$gameno[i] <- 599 + games1$quartet1[i]*2}
  if (games1$round[i]==64 & games1$natseed2[i] %in% c(1,9,5,13,3,11,7,15)) {games1$flip[i] <- 1}
  if (games1$round[i]==64 & games1$natseed1[i]==0 & games1$natseed2[i]==0 & games1$quartet1[i] %in% c(1,3,5,7,9,11,13,15)) 
		{games1$gameno[i]<-games1$gameno[i]+1}
  if (games1$round[i]==64 & games1$natseed1[i] %in% c(16,8,12,4,14,6,10,2)) {games1$gameno[i]<-games1$gameno[i]+1; games1$flip[i] <- 1}
  if (games1$round[i]==64 & games1$natseed2[i] %in% c(16,8,12,4,14,6,10,2)) {games1$gameno[i]<-games1$gameno[i]+1}

  if (games1$round[i]==32) {games1$gameno[i] <- 500 + games1$quartet1[i]}
  if (games1$round[i]==32 & games1$quartet1[i] %in% c(1,3,5,7,9,11,13,15) & (games1$team2[i]==games1$natwin[i] | games1$natseed1[i]==0)) 
		{games1$flip[i] <- 1}
  if (games1$round[i]==32 & games1$quartet1[i] %in% c(2,4,6,8,10,12,14,16) & (games1$team1[i]==games1$natwin[i] | games1$natseed2[i]==0)) 
		{games1$flip[i] <- 1}

  if (games1$round[i]==16) {games1$gameno[i] <- 400 + ceiling(games1$quartet1[i]/2)}
  if (games1$round[i]==8) {games1$gameno[i] <- 300 + ceiling(games1$quartet1[i]/4)}
  if (games1$round[i]==4) {games1$gameno[i] <- 200 + ceiling(games1$quartet1[i]/8)}
  if (games1$round[i]==1) {games1$gameno[i] <- 101}

  if (games1$round[i] %in% c(16,8,4,1) & games1$quartet1[i]>games1$quartet2[i]) {games1$flip[i] <- 1}

}

games1$oteam1 <- ifelse(games1$flip==1,games1$team2,games1$team1)
games1$oteam2 <- ifelse(games1$flip==1,games1$team1,games1$team2)
games1$oscore1 <- ifelse(games1$flip==1,games1$score2,games1$score1)
games1$oscore2 <- ifelse(games1$flip==1,games1$score1,games1$score2)

games1 <- games1[order(games1$year, -games1$round, games1$gameno),]
games1.x <- games1[c("year","name1","round","gameno","oteam1","oscore1","oteam2","oscore2","notes")]


games1 <- games1[order(games1$year, -games1$round, games1$gameno),]






