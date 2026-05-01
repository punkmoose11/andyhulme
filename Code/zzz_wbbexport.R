
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

games1 <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/NCAAW.xlsx",sheet = "Sheet1")
games1 <- as.data.frame(games1) 

games2 <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/NCAAW.xlsx",sheet = "Sheet2")
games2 <- as.data.frame(games2) 

seeds <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/NCAAW.xlsx",sheet = "Sheet4")
seeds <- as.data.frame(seeds) 


# GAMES1;

games1$NAME1 <- ifelse(games1$REGION != "Final Four",paste(games1$REGION,"Regional"),"Final Four")

games1$NAME2 <- ifelse(games1$GAMENO>=600,"First Round",
			ifelse(games1$GAMENO>=500,"Second Round",
			ifelse(games1$GAMENO>=400,"Regional Semifinals",
			ifelse(games1$GAMENO>=300,"Regional Final",
			ifelse(games1$GAMENO>=200,"Semifinals",
			ifelse(games1$GAMENO>=100,"Championship",""))))))

games1$ROUND <- ifelse(games1$GAMENO>=600,64,
			ifelse(games1$GAMENO>=500,32,
			ifelse(games1$GAMENO>=400,16,
			ifelse(games1$GAMENO>=300,8,
			ifelse(games1$GAMENO>=200,4,
			ifelse(games1$GAMENO>=100,1,99))))))

teams1 <- subset(games1[c("YEAR","GAMENO","REGION","TEAM1")],GAMENO>=600)
teams2 <- subset(games1[c("YEAR","GAMENO","REGION","TEAM2")],GAMENO>=600); names(teams2)[names(teams2) == "TEAM2"] <- "TEAM1";
teams <- rbind(teams1,teams2)

# GAMES2;

games2$NUM <- ifelse(games2$ROUND=="First Round",64,
			ifelse(games2$ROUND=="Second Round",32,
			ifelse(games2$ROUND=="Regional Semifinals",16,
			ifelse(games2$ROUND=="Regional Finals",8,
			ifelse(games2$ROUND=="Semifinals",4,
			ifelse(games2$ROUND=="Final",1,99))))))


teams1 <- subset(games2[c("YEAR","TEAM1","SEQ","NUM")],NUM==64 | NUM==32)
teams2 <- subset(games2[c("YEAR","TEAM2","SEQ","NUM")],NUM==64 | NUM==32); names(teams2)[names(teams2) == "TEAM2"] <- "TEAM1";
teams <- rbind(teams1,teams2)

#####################################################33

names(games2)[names(games2) == "ROUND"] <- "NAME2"
names(games2)[names(games2) == "NUM"] <- "ROUND"

games <- rbind(games1[c("YEAR","TEAM1","SCORE1","TEAM2","SCORE2","OT","ROUND")],
		   games2[c("YEAR","TEAM1","SCORE1","TEAM2","SCORE2","OT","ROUND")])


seeds <- unique(seeds[c("YEAR","TEAM1","SEED","REGIONAL")])

games <- merge(games,seeds,by=c("YEAR","TEAM1"),all.x=T)
games <- merge(games,seeds,by.x=c("YEAR","TEAM2"),by.y=c("YEAR","TEAM1"),all.x=T)

semis <- subset(games, ROUND==4)
semis$regno1 <- ifelse(semis$REGIONAL.x=="East" | semis$REGIONAL.x=="Alamo",1,
			ifelse(semis$REGIONAL.y=="East" | semis$REGIONAL.y=="Alamo",2,
			ifelse(semis$REGIONAL.x < semis$REGIONAL.y,3,4)))
semis$regno2 <- ifelse(semis$REGIONAL.y=="East" | semis$REGIONAL.y=="Alamo",1,
			ifelse(semis$REGIONAL.x=="East" | semis$REGIONAL.x=="Alamo",2,
			ifelse(semis$REGIONAL.y < semis$REGIONAL.x,3,4)))

reg1 <- semis[c("YEAR","REGIONAL.x","regno1")] 
names(reg1)[names(reg1) == "REGIONAL.x"] <- "REGIONAL";
names(reg1)[names(reg1) == "regno1"] <- "REGNO";
reg2 <- semis[c("YEAR","REGIONAL.y","regno2")]
names(reg2)[names(reg2) == "REGIONAL.y"] <- "REGIONAL";
names(reg2)[names(reg2) == "regno2"] <- "REGNO";

reg <- rbind(reg1,reg2)

games <- merge(games,reg,by.x=c("YEAR","REGIONAL.x"),by.y=c("YEAR","REGIONAL"),all.x=T)
games1<- merge(games,reg,by.x=c("YEAR","REGIONAL.y"),by.y=c("YEAR","REGIONAL"),all.x=T)


games1$gameno <- 0
games1$flip   <- 0

for (i in 1:nrow(games1)) {

  if (games1$ROUND[i]==64) {games1$gameno[i] <- 600 + games1$REGNO.x[i]*8 - 8}
  if (games1$ROUND[i]==64 & games1$SEED.x[i] %in% c(16,9,12,13,11,14,10,15)) {games1$flip[i] <- 1}
  if (games1$ROUND[i]==64 & games1$SEED.x[i] %in% c(1,16)) {games1$gameno[i]<-games1$gameno[i]+1}
  if (games1$ROUND[i]==64 & games1$SEED.x[i] %in% c(8,9)) {games1$gameno[i]<-games1$gameno[i]+2}
  if (games1$ROUND[i]==64 & games1$SEED.x[i] %in% c(5,12)) {games1$gameno[i]<-games1$gameno[i]+3}
  if (games1$ROUND[i]==64 & games1$SEED.x[i] %in% c(4,13)) {games1$gameno[i]<-games1$gameno[i]+4}
  if (games1$ROUND[i]==64 & games1$SEED.x[i] %in% c(6,11)) {games1$gameno[i]<-games1$gameno[i]+5}
  if (games1$ROUND[i]==64 & games1$SEED.x[i] %in% c(3,14)) {games1$gameno[i]<-games1$gameno[i]+6}
  if (games1$ROUND[i]==64 & games1$SEED.x[i] %in% c(7,10)) {games1$gameno[i]<-games1$gameno[i]+7}
  if (games1$ROUND[i]==64 & games1$SEED.x[i] %in% c(2,15)) {games1$gameno[i]<-games1$gameno[i]+8}

  if (games1$ROUND[i]==32) {games1$gameno[i] <- 500 + games1$REGNO.x[i]*4 - 4}
  if (games1$ROUND[i]==32 & games1$SEED.x[i] %in% c(8,9,4,13,3,14,2,15)) {games1$flip[i] <- 1}
  if (games1$ROUND[i]==32 & games1$SEED.x[i] %in% c(1,16,8, 9)) {games1$gameno[i]<-games1$gameno[i]+1}
  if (games1$ROUND[i]==32 & games1$SEED.x[i] %in% c(5,12,4,13)) {games1$gameno[i]<-games1$gameno[i]+2}
  if (games1$ROUND[i]==32 & games1$SEED.x[i] %in% c(6,11,3,14)) {games1$gameno[i]<-games1$gameno[i]+3}
  if (games1$ROUND[i]==32 & games1$SEED.x[i] %in% c(7,10,2,15)) {games1$gameno[i]<-games1$gameno[i]+4}

  if (games1$ROUND[i]==16) {games1$gameno[i] <- 400 + games1$REGNO.x[i]*2 - 2}
  if (games1$ROUND[i]==16 & games1$SEED.x[i] %in% c(5,12,4,13,7,10,2,15)) {games1$flip[i] <- 1}
  if (games1$ROUND[i]==16 & games1$SEED.x[i] %in% c(1,16,8, 9,5,12,4,13)) {games1$gameno[i]<-games1$gameno[i]+1}
  if (games1$ROUND[i]==16 & games1$SEED.x[i] %in% c(6,11,3,14,7,10,2,15)) {games1$gameno[i]<-games1$gameno[i]+2}

  if (games1$ROUND[i]==8) {games1$gameno[i] <- 300 + games1$REGNO.x[i]}
  if (games1$ROUND[i]==8 & games1$SEED.x[i] %in% c(6,11,3,14,7,10,2,15)) {games1$flip[i] <- 1}

  if (games1$ROUND[i]==4) {games1$gameno[i] <- 200 + min(games1$REGNO.x[i],games1$REGNO.y[i])/2 + 0.5}
  if (games1$ROUND[i]==4 & games1$REGNO.x[i] %in% c(2,4)) {games1$flip[i] <- 1}

  if (games1$ROUND[i]==1) {games1$gameno[i] <- 101}
  if (games1$ROUND[i]==1 & games1$REGNO.x[i] %in% c(3,4)) {games1$flip[i] <- 1}


  if (games1$ROUND[i]==99) {games1$gameno[i] <- 700}
}

#subset(games1[order(games1$YEAR, games1$gameno),], ROUND==64)
table(games1$gameno)

games1$sport <- "WBB"
games1$name1 <- ifelse(games1$ROUND %in% c(1,4),"Final Four",paste(games1$REGIONAL.x,"Regional"))

games1$name2 <- ifelse(games1$gameno>=600,"First Round",
			ifelse(games1$gameno>=500,"Second Round",
			ifelse(games1$gameno>=400,"Regional Semifinals",
			ifelse(games1$gameno>=300,"Regional Final",
			ifelse(games1$gameno>=200,"Semifinals",
			ifelse(games1$gameno>=100,"Championship","Opening Round"))))))

games1$oteam1 <- ifelse(games1$flip==1,games1$TEAM2,games1$TEAM1)
games1$oteam2 <- ifelse(games1$flip==1,games1$TEAM1,games1$TEAM2)
games1$oscore1 <- ifelse(games1$flip==1,games1$SCORE2,games1$SCORE1)
games1$oscore2 <- ifelse(games1$flip==1,games1$SCORE1,games1$SCORE2)

games1 <- games1[order(games1$YEAR, -games1$ROUND, games1$gameno),]
games1.x <- games1[c("sport","YEAR","name1","name2","ROUND","gameno","oteam1","oscore1","oteam2","oscore2","OT")]

head(games1.x)

write.csv(games1.x, file = "../Data/ncaaw.csv", quote = FALSE, row.names = FALSE, na="")




