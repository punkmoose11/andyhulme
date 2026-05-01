library("XML")
library("readxl")
setwd("C:/Users/andyh/Documents/OMAHASERIES/Code")

data  <- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/College Football Games.xlsx",sheet = "Games")
games <- as.data.frame(data) 
games <- games[c("season","date","team1","team2","points1","points2","ot","loc","locn","text","conf")]

teams <- c(games$team1,games$team2)
teams <- as.data.frame(table(teams))
teams <- teams[order(-teams$Freq), ]
teams <- subset(teams, Freq>=20)
teams <- teams[order(teams$teams), ]
rownames(teams) <- NULL
teams$index <- as.numeric(rownames(teams))

games <- merge(games,teams[c("teams","index")],by.x="team1",by.y="teams",all.x=TRUE)
games <- merge(games,teams[c("teams","index")],by.x="team2",by.y="teams",all.x=TRUE)
games <- games[order(games$season, games$date, games$team1, games$team2), ]
games <- games[c("season","date","team1","team2","points1","points2","ot","loc","locn","text","conf","index.x","index.y")]

games.r <- subset(games, is.na(index.x)==FALSE & is.na(index.y)==FALSE)
keys <- c(paste(games.r$season,games.r$team1,sep=""),paste(games.r$season,games.r$team2,sep=""))
keys <- as.data.frame(table(keys))
keys$season <- substring(keys$keys,1,4)
keys$teams   <- substring(keys$keys,5,100)
keys <- merge(keys, teams[c("teams","index")], by="teams", all.y=TRUE)
keys <- keys[order(keys$team, keys$season), ]
keys <- subset(keys, season<=1887 | Freq>=3)
keys <- subset(keys, season<=1873 | Freq>=2)

Q <- matrix(0, nrow=1+nrow(games), ncol=nrow(teams))
for (i in 1:nrow(games)) {
q1 <- subset(keys, season==games$season[i])
	for (j in 1:nrow(q1)) {
		Q[i,q1$index[j]] <- 1
	}}

elo   <- subset(games,is.na(points1)==F)
elo$do.elo <- ifelse(is.na(elo$index.x)==FALSE & is.na(elo$index.y)==FALSE,1,0)

elo$win <- ifelse(elo$points1>elo$points2,1,ifelse(elo$points1<elo$points2,0,0.5))
elo$rtg1 <- 0
elo$rtg2 <- 0
elo$inrank1 <- 0
elo$inrank2 <- 0
elo$rank1 <- 0
elo$rank2 <- 0
elo$pts   <- -9999
elo$x   <- -9999

elo$last  <- 0
for (i in 1:nrow(elo)) {
	if (i<nrow(elo) & elo$season[i]!=elo$season[i+1]) {elo$last[i] <- 1}
	if (i==nrow(elo)) {elo$last[i] <- 1}
	}

E <- matrix(-9999, nrow=1+nrow(games), ncol=nrow(teams))
EE<- matrix(-9999, nrow=1+nrow(games), ncol=nrow(teams))

K <- 200
digs <- 0

### ELO Loop ###
for (i in 1:nrow(elo)) {

if (i>1 & elo$last[max(1,i-1)]==1) {
rates <- as.data.frame(EE[i-1,])
names(rates) <- c("value")
rates <- subset(rates, value != -9999)
ADJUST <- mean(rates$value)
for (kk in 1:ncol(E)) {E[i,kk] <- ifelse(E[i,kk]==-9999,-9999,round(E[i,kk]-ADJUST,digits=digs))}
print(paste(elo$season[i],"ADJUSTMENT is ",ADJUST))
}

if (elo$do.elo[i]==1) {
j1 <- elo$index.x[i] 
j2 <- elo$index.y[i]

if (E[i,j1]==-9999) {E[i,j1]<-0}
if (E[i,j2]==-9999) {E[i,j2]<-0}

We <- 1 / (10 ** (-1 * ( E[i,j1] - E[i,j2] ) / 400) + 1)
P  <- round ( K * (elo$win[i] - We), digits=digs)

E[i,j1] <- round(E[i,j1] + P,digits=digs)
E[i,j2] <- round(E[i,j2] - P,digits=digs)
}

for (k in 1:ncol(E)) {EE[i,k] <- ifelse(Q[i,k]==1,E[i,k],-9999)}
elo$rtg1[i] <- ifelse(is.na(elo$index.x[i])==FALSE,E[i,elo$index.x[i]],-9999)
elo$rtg2[i] <- ifelse(is.na(elo$index.y[i])==FALSE,E[i,elo$index.y[i]],-9999)
elo$inrank1[i] <- ifelse(i>1 & is.na(elo$index.x[i])==FALSE,
				ifelse(Q[i,elo$index.x[i]]==1,rank(-EE[i-1,],ties.method="min")[elo$index.x[i]],-9999),-9999)
elo$inrank2[i] <- ifelse(i>1 & is.na(elo$index.y[i])==FALSE,
				ifelse(Q[i,elo$index.y[i]]==1,rank(-EE[i-1,],ties.method="min")[elo$index.y[i]],-9999),-9999)
elo$rank1[i] <- ifelse(is.na(elo$index.x[i])==FALSE,
				ifelse(Q[i,elo$index.x[i]]==1,rank(-EE[i,],ties.method="min")[elo$index.x[i]],-9999),-9999)
elo$rank2[i] <- ifelse(is.na(elo$index.y[i])==FALSE,
				ifelse(Q[i,elo$index.y[i]]==1,rank(-EE[i,],ties.method="min")[elo$index.y[i]],-9999),-9999)
elo$pts[i] <- ifelse(elo$do.elo[i]==1,P,-9999)
elo$x[i] <- i

#print(paste(i,elo$pts[i],elo$date[i],"(",elo$inrank1[i],">>",elo$rank1[i],")",elo$team1[i],elo$points1[i],"-",elo$points2[i],elo$team2[i],"(",elo$inrank2[i],">>",elo$rank2[i],")"))

E[i+1,] <- E[i,]

if (elo$last[i]==1) {
rating <- as.data.frame(EE[i,])
rating$index <- rownames(rating)
rating <- merge(rating,teams[c("teams","index")],by="index")
rating <- rating[order(-rating[[2]]), ]
print(elo$season[i])
print(rating[1:5,])
outs <- rating
names(outs) <- c("index","rating","team")
outs$year <- elo$season[i]
outs$elo  <- rank(-outs$rating, ties.method='min')

if(elo$season[i]==1869) {outsall <- outs}
else if (elo$season[i]>1869) {outsall <- rbind(outsall,outs)}

}

}
### end elo loop ###

rating <- as.data.frame(EE[nrow(elo),])
rating$index <- rownames(rating)
rating <- merge(rating,teams[c("teams","index")],by="index")
rating <- rating[order(-rating[[2]]), ]
rating[1:25,]

elo.1 <- elo
elo.2 <- elo

elo.2$team1 <- elo$team2
elo.2$team2 <- elo$team1
elo.2$points1 <- elo$points2
elo.2$points2 <- elo$points1
elo.2$rtg1 <- elo$rtg2
elo.2$rtg2 <- elo$rtg1
elo.2$rank1 <- elo$rank2
elo.2$rank2 <- elo$rank1
elo.2$inrank1 <- elo$inrank2
elo.2$inrank2 <- elo$inrank1
elo.2$pts <- -elo$pts
elo.2$win <- ifelse(elo$win==1,0,ifelse(elo$win==0,1,0.5))
elo.2$loc <- ifelse(elo$loc==1,2,ifelse(elo$loc==2,1,elo$loc))

all <- rbind(elo.1,elo.2)
all <- all[order(all$season, all$date, all$team1, all$team2), ]
#all <- all[order(all$team1, all$season, all$date, all$team2), ]

#subset(all, team1=="Rutgers")[c("season","date","team1","team2","points1","points2","win","rtg1","rank1","pts")]
#subset(all, team1=="Nebraska" & season<=1900)

export <- all[c("season","date","team1","points1","team2","points2","ot","loc","locn","text","conf",
			"inrank1","inrank2","rank1","rank2","rtg1","rtg2","pts")]

write.csv(export,file="../Data/cfgames.csv",na='')
write.csv(outsall,file="../Data/cfelortg.csv",na='')


#rate <- export
#rate <- subset(rate, rtg1!=-9999 & rtg2!=-9999)
#rate$score <- ifelse(rate$points1>rate$points2,rate$rtg2+rate$pts+400,
#			ifelse(rate$points1<rate$points2,rate$rtg2+rate$pts-400,rate$rtg2+rate$pts))
#rate$key <- paste(rate$season,rate$team1,sep="/")
#rate$cnt <- 1

#ratings1 <- aggregate(rate$score, by=list(key=rate$key), FUN=sum); names(ratings1) <- c("key","sum");
#ratings2 <- aggregate(rate$cnt  , by=list(key=rate$key), FUN=sum); names(ratings2) <- c("key","cnt");
#ratings <- merge(ratings1,ratings2,by="key")

#ratings$season <- substring(ratings$key,1,4)
#ratings$team <- substring(ratings$key,6,100)
#ratings$rtg <- ratings$sum/ratings$cnt

#ratings <- ratings[order(ratings$season, -ratings$rtg),]
