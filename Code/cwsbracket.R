
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
for (yyyy in c(1988:2019,2021:2025)) {

data <- subset(dataf, yr==yyyy)
info <- subset(infof, yr==yyyy)

cws  <- subset(data, regional == "CWS"); cws_ <- subset(data, regional == "CWS");
cws$team <- cws$winner; cws_$team<- cws_$loser; 
sr <- unique(subset(data,regional!="CWS")[c("winner","regional")])

cwsteams <- unique(rbind(cws["team"],cws_["team"]))

cwsteams <- merge(cwsteams,info[c("team","natseed")],all.x=T)
cwsteams <- merge(cwsteams,sr[c("winner","regional")],all.x=T,by.x="team",by.y="winner")

cwsteams$seed <- ifelse(grepl("Super",cwsteams$regional),as.numeric(gsub("Super Regional","",cwsteams$regional)),cwsteams$natseed)
cwsteams$seedn<- sapply(cwsteams$seed, seedsort)
cwsteams <- cwsteams[order(cwsteams$seedn),]

games <- cws[c("winner","wsc","loser","lsc","gameno")]
games$numb <- as.numeric(substring(games$gameno,6,7)) + ifelse(grepl("F",games$gameno),300,0)
	games$numb <- ifelse(is.na(games$numb),301,games$numb)
games.1<-games
games.1$winner <- games.1$loser
games.1$wsc <- games.1$lsc
games <- rbind(games,games.1)[c("numb","winner","wsc")]
colnames(games) <- c("numb","team","score")

TT <- matrix("",nrow=17,ncol=7)
TT[,1] <- c("101","102","103","104","105","106","107","201","202","203","204","205","206","207","301","302","303")

#Round 1
round1 <- subset(games,ceiling(numb/4)==1)

TT[1,2] <- paste(subset(cwsteams, seed==1)$team)
TT[1,3] <- paste(subset(cwsteams, seed==8)$team)
TT[2,2] <- paste(subset(cwsteams, seed==5)$team)
TT[2,3] <- paste(subset(cwsteams, seed==4)$team)
TT[8,2] <- paste(subset(cwsteams, seed==2)$team)
TT[8,3] <- paste(subset(cwsteams, seed==7)$team)
TT[9,2] <- paste(subset(cwsteams, seed==6)$team)
TT[9,3] <- paste(subset(cwsteams, seed==3)$team)

for (jj in c(1,2,8,9)) {
TT[jj,4] <- paste(subset(round1, team==TT[jj,2])$score)
TT[jj,5] <- paste(subset(round1, team==TT[jj,3])$score)
TT[jj,6] <- ifelse(as.numeric(TT[jj,4])>as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
TT[jj,7] <- ifelse(as.numeric(TT[jj,4])<as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
}

#Round 2
round2 <- subset(games,ceiling(numb/4)==2)

TT[ 3,2] <- TT[1,7]
TT[ 3,3] <- TT[2,7]
TT[ 4,2] <- TT[1,6]
TT[ 4,3] <- TT[2,6]
TT[10,2] <- TT[8,7]
TT[10,3] <- TT[9,7]
TT[11,2] <- TT[8,6]
TT[11,3] <- TT[9,6]

for (jj in c(3,4,10,11)) {
TT[jj,4] <- paste(subset(round2, team==TT[jj,2])$score)
TT[jj,5] <- paste(subset(round2, team==TT[jj,3])$score)
TT[jj,6] <- ifelse(as.numeric(TT[jj,4])>as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
TT[jj,7] <- ifelse(as.numeric(TT[jj,4])<as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])

}

#Round 3
round3 <- subset(games,ceiling(numb/2)==5)

TT[ 5,2] <- TT[ 4,7]
TT[ 5,3] <- TT[ 3,6]
TT[12,2] <- TT[11,7]
TT[12,3] <- TT[10,6]

for (jj in c(5,12)) {
TT[jj,4] <- paste(subset(round3, team==TT[jj,2])$score)
TT[jj,5] <- paste(subset(round3, team==TT[jj,3])$score)
TT[jj,6] <- ifelse(as.numeric(TT[jj,4])>as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
TT[jj,7] <- ifelse(as.numeric(TT[jj,4])<as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
}

#Round 4
round4 <- subset(games,ceiling(numb/2)==6)

TT[ 6,2] <- TT[ 4,6]
TT[ 6,3] <- TT[ 5,6]
TT[13,2] <- TT[11,6]
TT[13,3] <- TT[12,6]

for (jj in c(6,13)) {
TT[jj,4] <- paste(subset(round4, team==TT[jj,2])$score)
TT[jj,5] <- paste(subset(round4, team==TT[jj,3])$score)
TT[jj,6] <- ifelse(as.numeric(TT[jj,4])>as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
TT[jj,7] <- ifelse(as.numeric(TT[jj,4])<as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
}

#Round 5 - if necessary
round5 <- subset(games,ceiling(numb/2)==7)

if (TT[6,3]==TT[6,6]) {
TT[ 7,2] <- TT[ 6,2]
TT[ 7,3] <- TT[ 6,3] 

jj <- 7
TT[jj,4] <- paste(subset(round5, team==TT[jj,2])$score)
TT[jj,5] <- paste(subset(round5, team==TT[jj,3])$score)
TT[jj,6] <- ifelse(as.numeric(TT[jj,4])>as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
TT[jj,7] <- ifelse(as.numeric(TT[jj,4])<as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
}

if (TT[13,3]==TT[13,6]) {
TT[14,2] <- TT[13,2]
TT[14,3] <- TT[13,3] 

jj <- 14
TT[jj,4] <- paste(subset(round5, team==TT[jj,2])$score)
TT[jj,5] <- paste(subset(round5, team==TT[jj,3])$score)
TT[jj,6] <- ifelse(as.numeric(TT[jj,4])>as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
TT[jj,7] <- ifelse(as.numeric(TT[jj,4])<as.numeric(TT[jj,5]),TT[jj,2],TT[jj,3])
}

#Final
final <- subset(games,numb>100)

for (jj in 1:(nrow(final)/2)) {
TT[14+jj,2] <- ifelse(TT[ 7,6]!="",TT[ 7,6],TT[ 6,6])
TT[14+jj,3] <- ifelse(TT[14,6]!="",TT[14,6],TT[13,6])
TT[14+jj,4] <- paste(subset(final, team==TT[14+jj,2] & numb==300+jj)$score)
TT[14+jj,5] <- paste(subset(final, team==TT[14+jj,3] & numb==300+jj)$score)
}

#Format
XX <- as.data.frame(TT)
XX$html <- ifelse(XX$V2!="",paste(XX$V2," <b>",XX$V4,"</b><br>",XX$V3," <b>",XX$V5,"</b>",sep=""),"")

fileName <- "../CWS/bracket.html"
bracket <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(XX)) {bracket <- gsub(paste("Game",XX$V1[i]),
	trim(paste(ifelse((XX$V1[i]==107|XX$V1[i]==207|XX$V1[i]==302|XX$V1[i]==303) & XX$V2[i]!="","<br><br>",""),XX$html[i],sep="")),bracket)}
for (i in c(101:107,201:207,301:303)) {bracket <- gsub(paste("Game",i),"",bracket)}
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


