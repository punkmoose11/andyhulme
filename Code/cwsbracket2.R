
############# import data
dataf <- read.csv("../Data/cwshtml.csv")
infof <- read.csv("../Data/cwsteams.csv")
infof <- infof [c("team","yr","nickname","conference","record","seed","natseed","host","suphost")]

###########################################

teamlink <- function(x){teamx = tolower(gsub("&","",gsub(".","",gsub("'","",gsub(" ", "",x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE))}

seedsort <- function(seed) {
	if (is.na(seed))  {seedn<-0}
	else if (seed==1) {seedn<-1}
	else if (seed==8) {seedn<-2}
	else if (seed==5) {seedn<-3}
	else if (seed==4) {seedn<-4}
	else if (seed==2) {seedn<-5}
	else if (seed==7) {seedn<-6}
	else if (seed==6) {seedn<-7}
	else if (seed==3) {seedn<-8}
}

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

##########################################
##########################################
for (yyyy in c(1950,1953:1987)) {
data <- subset(dataf, yr==yyyy)
info <- subset(infof, yr==yyyy)

cws  <- subset(data, regional == "CWS"); cws_ <- subset(data, regional == "CWS");
cws$team <- cws$winner; cws_$team<- cws_$loser; 
sr <- unique(subset(data,regional!="CWS")[c("winner","regional")])

cwsteams <- unique(rbind(cws["team"],cws_["team"]))

cwsteams <- merge(cwsteams,subset(cws,round=="Round 1")[c("winner","gameno")],by.x="team",by.y="winner",all.x=T)
cwsteams <- merge(cwsteams,subset(cws,round=="Round 1")[c("loser" ,"gameno")],by.x="team",by.y="loser" ,all.x=T)
cwsteams$gameno <- ifelse(is.na(cwsteams$gameno.x)==F,paste(cwsteams$gameno.x),paste(cwsteams$gameno.y))
cwsteams <- merge(cwsteams,sr[c("winner","regional")],all.x=T,by.x="team",by.y="winner")

cwsteams <- cwsteams[order(cwsteams$gameno, cwsteams$regional),]
cwsteams$seed <- row(cwsteams[c("team")])

games <- cws[c("winner","wsc","loser","lsc","gameno","round")]
games$rd   <- as.numeric(ifelse(grepl("Final",games$round),"9",substring(games$round,7,7)))
games$numb <- as.numeric(substring(games$gameno,6,7)) + ifelse(grepl("F",games$gameno),300,0)
	games$numb <- ifelse(is.na(games$numb),301,games$numb)
games.1<-games
games.1$winner <- games.1$loser
games.1$wsc <- games.1$lsc
games <- rbind(games,games.1)[c("rd","numb","winner","wsc")]
colnames(games) <- c("rd","numb","team","score")

TT <- matrix("",nrow=16,ncol=7)
TT[,1] <- c("101","102","103","104","201","202","203","204","301","302","303","401","402","501","901","902")

#Round 1
round1 <- subset(games,rd==1)

TT[1,2] <- paste(subset(cwsteams, seed==1)$team)
TT[1,3] <- paste(subset(cwsteams, seed==2)$team)
TT[2,2] <- paste(subset(cwsteams, seed==3)$team)
TT[2,3] <- paste(subset(cwsteams, seed==4)$team)
TT[3,2] <- paste(subset(cwsteams, seed==5)$team)
TT[3,3] <- paste(subset(cwsteams, seed==6)$team)
TT[4,2] <- paste(subset(cwsteams, seed==7)$team)
TT[4,3] <- paste(subset(cwsteams, seed==8)$team)

for (jj in c(1,2,3,4)) {
TT[jj,4] <- paste(subset(round1, team==TT[jj,2])$score)
TT[jj,5] <- paste(subset(round1, team==TT[jj,3])$score)
TT[jj,6] <- ifelse(as.numeric(TT[jj,4])>as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
TT[jj,7] <- ifelse(as.numeric(TT[jj,4])<as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
}

#Round 2
round2 <- subset(games,rd==2)

TT[ 5,2] <- TT[1,6]
TT[ 5,3] <- TT[2,6]
TT[ 6,2] <- TT[3,6]
TT[ 6,3] <- TT[4,6]
TT[ 7,2] <- TT[1,7]
TT[ 7,3] <- TT[2,7]
TT[ 8,2] <- TT[3,7]
TT[ 8,3] <- TT[4,7]

for (jj in c(5,6,7,8)) {
TT[jj,4] <- paste(subset(round2, team==TT[jj,2])$score)
TT[jj,5] <- paste(subset(round2, team==TT[jj,3])$score)
TT[jj,6] <- ifelse(as.numeric(TT[jj,4])>as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
TT[jj,7] <- ifelse(as.numeric(TT[jj,4])<as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])

}

#Round 3
round3 <- subset(games,rd==3)

TT[ 9,2] <- TT[ 5,6]
TT[ 9,3] <- TT[ 6,6]
TT[10,2] <- TT[ 6,7]
TT[10,3] <- TT[ 7,6]
TT[11,2] <- TT[ 5,7]
TT[11,3] <- TT[ 8,6]

if (yyyy==1950) {
TT[10,2] <- TT[ 7,6]
TT[10,3] <- TT[ 8,6]
TT[11,2] <- TT[ 5,7]
TT[11,3] <- TT[ 6,7]
}

for (jj in c(9,10,11)) {
TT[jj,4] <- paste(subset(round3, team==TT[jj,2])$score)
TT[jj,5] <- paste(subset(round3, team==TT[jj,3])$score)
TT[jj,6] <- ifelse(as.numeric(TT[jj,4])>as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
TT[jj,7] <- ifelse(as.numeric(TT[jj,4])<as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
}

#Round 4
round4 <- subset(games,rd==4)

TT[12,2] <- TT[ 9,6]
TT[13,2] <- TT[ 9,7]
if (yyyy==1953|yyyy==1954|yyyy==1955) {TT[13,2] <- TT[10,6]}

for (jj in c(12,13)) {
xx <- paste(subset(round4, team==TT[jj,2])$numb) 
TT[jj,3] <- paste(subset(round4, numb==xx & team!=TT[jj,2])$team)
TT[jj,4] <- paste(subset(round4, team==TT[jj,2])$score)
TT[jj,5] <- paste(subset(round4, team==TT[jj,3])$score)
TT[jj,6] <- ifelse(as.numeric(TT[jj,4])>as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
TT[jj,7] <- ifelse(as.numeric(TT[jj,4])<as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
}

#Round 5 - if necessary
round5 <- subset(games,rd==5)

if (nrow(round5)>0) {
jj<-14
TT[jj,2] <- paste(round5$team[[1]])
TT[jj,3] <- paste(round5$team[[2]])
TT[jj,4] <- paste(subset(round5, team==TT[jj,2])$score)
TT[jj,5] <- paste(subset(round5, team==TT[jj,3])$score)
TT[jj,6] <- ifelse(as.numeric(TT[jj,4])>as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
TT[jj,7] <- ifelse(as.numeric(TT[jj,4])<as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
}

#Final
final <- subset(games,rd==9)
if (nrow(final)==2) {final$x<-c(1,1)}
if (nrow(final)==4) {final$x<-c(1,2,1,2)}

for (jj in 1:(nrow(final)/2)) {
TT[14+jj,2] <- paste(final$team[[1]])
TT[14+jj,3] <- paste(final$team[[2+nrow(final)/2-1]])
TT[14+jj,4] <- paste(subset(final, team==TT[14+jj,2] & x==jj)$score)
TT[14+jj,5] <- paste(subset(final, team==TT[14+jj,3] & x==jj)$score)
}

#Format
XX <- as.data.frame(TT)
XX$html <- ifelse(XX$V2!="",paste(XX$V2," <b>",XX$V4,"</b><br>",XX$V3," <b>",XX$V5,"</b>",sep=""),"")

fileName <- "../CWS/bracket2.html"
bracket <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(XX)) {bracket <- gsub(paste("Game",XX$V1[i]),
	trim(paste(ifelse((XX$V1[i]==902) & XX$V2[i]!="","<br><br>",""),XX$html[i],
		ifelse((XX$V1[i]==501) & XX$V2[i]!="","<br><br>",""),sep="")),bracket)}
for (i in c(101:107,201:207,301:303,401:402,501,901:902)) {bracket <- gsub(paste("Game",i),"",bracket)}
#for (i in 1:4) {bracket <- gsub(paste("Regional",i),regional.winners$name1[i],bracket)}

sink("../CWS/bracket2016.html")
write.table(bracket, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

#####################################################################

fileName <- paste("../CWS/",yyyy,".html",sep="")
yearpage <- readChar(fileName, file.info(fileName)$size)
yearpage <- gsub("<!--Insert Bracket--!>",bracket,yearpage)

sink(fileName)
write.table(yearpage, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

}
#####################################################################
#####################################################################


