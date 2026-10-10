
seasons <- read.csv("../Data/cfseasons.csv")
cfgames <- read.csv("../Data/cfgames.csv")
names(seasons)[names(seasons) == "conf"] <- "conference"
cfgames$result <- ifelse(cfgames$points1>cfgames$points2,"W",ifelse(cfgames$points1<cfgames$points2,"L",
				ifelse(cfgames$points1==cfgames$points2,"T","")))

summ <- subset(seasons, year>=1895)
sched <- subset(cfgames, season>=1895)

summ$cpct <- (summ$WC + summ$TC/2) / (summ$WC + summ$LC + summ$TC)
summ$crank <- ave(-summ$cpct, paste(summ$year,summ$conference,summ$div), FUN = function(x) rank(x, ties.method = "min") )

sched2 <- merge(sched ,summ[c("year","team","conference","division","WC","LC","TC","crank")],by.x=c("season","team1"),by.y=c("year","team"),all.x=TRUE)
sched3 <- merge(sched2,summ[c("year","team","conference","division","WC","LC","TC","crank")],by.x=c("season","team2"),by.y=c("year","team"),all.x=TRUE)

## Division record ##
div1 <- subset(sched3,conf=="C" & conference.x==conference.y & division.x==division.y)
div2 <- as.data.frame.matrix(table(paste(div1$season,div1$team1), paste(div1$result,"D",sep="")))

div2$key <- rownames(div2)
div2$season <- substring(div2$key,1,4)
div2$team  <- substring(div2$key,6,100)

# Opponents WPCT ##
op1 <- subset(sched3,conf=="C")
opW <- aggregate(op1$WC.y, list(op1$season, op1$team1), sum); names(opW)[names(opW)=="x"] <- "OW"
opL <- aggregate(op1$LC.y, list(op1$season, op1$team1), sum); names(opL)[names(opL)=="x"] <- "OL"
opT <- aggregate(op1$TC.y, list(op1$season, op1$team1), sum); names(opT)[names(opT)=="x"] <- "OT"

op2 <- merge(summ,opW,by.x=c("year","team"),by.y=c("Group.1","Group.2"),all.x=TRUE)
op3 <- merge(op2 ,opL,by.x=c("year","team"),by.y=c("Group.1","Group.2"),all.x=TRUE)
op4 <- merge(op3 ,opT,by.x=c("year","team"),by.y=c("Group.1","Group.2"),all.x=TRUE)

## Head-to-Head ##
hh1 <- subset(sched3,conf=="C" & conference.x==conference.y & division.x==division.y & crank.x==crank.y)
hh2 <- as.data.frame.matrix(table(paste(hh1$season,hh1$team1), paste(hh1$result,"HH",sep="")))

hh2$key <- rownames(hh2)
hh2$season <- substring(hh2$key,1,4)
hh2$team  <- substring(hh2$key,6,100)

## Combine ###
summ2 <- merge(summ ,div2[c("season","team","WD","LD","TD"   )],by.x=c("year","team"),by.y=c("season","team"),all.x=TRUE)
summ3 <- merge(summ2, op4[c("year"  ,"team","OW","OL","OT"   )],by.x=c("year","team"),by.y=c("year","team"),all.x=TRUE)
summ4 <- merge(summ3, hh2[c("season","team","WHH","LHH","THH")],by.x=c("year","team"),by.y=c("season","team"),all.x=TRUE)

summ4$OW1 <- summ4$OW - summ4$LC
summ4$OL1 <- summ4$OL - summ4$WC
summ4$opct <- summ4$OW1 / (summ4$OW1 + summ4$OL1)
summ4$dpct <- summ4$WD / (summ4$WD + summ4$LD)
summ4$hhpct <- ifelse(is.na(summ4$WHH)==F,summ4$WHH / (summ4$WHH + summ4$LHH),0.5)

summ4 <- summ4[order(summ4$year, summ4$conference, -summ4$cpct, -summ4$hhpct, -summ4$dpct, -summ4$opct), ]


############## break head-to-head ties ####################
H2HW <- matrix(0,nrow=133,ncol=133)
H2HL <- matrix(0,nrow=133,ncol=133)
for (gg in 1:nrow(scores)) {
	if (scores$result[gg]=="1") { H2HW[scores$i1[gg],scores$i2[gg]] <- H2HW[scores$i1[gg],scores$i2[gg]]+1
						H2HL[scores$i2[gg],scores$i1[gg]] <- H2HL[scores$i2[gg],scores$i1[gg]]-1 }
	if (scores$result[gg]=="2") { H2HL[scores$i1[gg],scores$i2[gg]] <- H2HL[scores$i1[gg],scores$i2[gg]]-1
						H2HW[scores$i2[gg],scores$i1[gg]] <- H2HW[scores$i2[gg],scores$i1[gg]]+1 }
}

# Cycle 1
CON <- t(matrix(results$rank,nrow=133,ncol=133))
ties1 <- rep(0,nrow(results))
wins1 <- rep(0,nrow(results))
loss1 <- rep(0,nrow(results))

for (h1 in 1:nrow(results)) { 
	if (results$rank[h1]<=2) {
	for (h2 in results$min[h1]:results$max[h1]) {
	if (results$rank[h1]==CON[h1,h2]) {ties1[h1] <- ties1[h1] + 1; 
							wins1[h1] <- wins1[h1] + H2HW[h1,h2]; loss1[h1] <- loss1[h1] + H2HL[h1,h2] }
}}}

results <- cbind(results,ties1,wins1,loss1)
results$rank2 <- ave(-1 * ((results$WC-results$LC) + results$WC/100 + (results$wins1+results$loss1)/100000), 
					results$Conference, FUN = function(x) rank(x, ties.method = "min"))

# Cycle 2
CON <- t(matrix(results$rank2,nrow=133,ncol=133))
ties2 <- rep(0,nrow(results))
wins2 <- rep(0,nrow(results))
loss2 <- rep(0,nrow(results))

for (h1 in 1:nrow(results)) { 
	if (results$ties1[h1]>1) {
	for (h2 in results$min[h1]:results$max[h1]) {
	if (results$rank2[h1]==CON[h1,h2]) {ties2[h1] <- ties2[h1] + 1; 
							wins2[h1] <- wins2[h1] + H2HW[h1,h2]; loss2[h1] <- loss2[h1] + H2HL[h1,h2] }
}}}

results <- cbind(results,ties2,wins2,loss2)
results$rank3 <- ave(-1 * ((results$WC-results$LC) + results$WC/100 + (results$wins1+results$loss1)/100000
					+ (results$wins2+results$loss2)/10000000), 
					results$Conference, FUN = function(x) rank(x, ties.method = "min"))

# Cycle 3 - Division Record
results$rank4 <- ave(-1 * ((results$WC-results$LC) + results$WC/100 + (results$wins1+results$loss1)/100000
					+ (results$wins2+results$loss2)/10000000 + (results$WD-results$LD)/10000000000), 
					results$Conference, FUN = function(x) rank(x, ties.method = "min"))

# Cycle 4
CON <- t(matrix(results$rank4,nrow=133,ncol=133))
ties4 <- rep(0,nrow(results))
wins4 <- rep(0,nrow(results))
loss4 <- rep(0,nrow(results))

for (h1 in 1:nrow(results)) { 
	if (results$ties1[h1]>1) {
	for (h2 in results$min[h1]:results$max[h1]) {
	if (results$rank4[h1]==CON[h1,h2]) {ties4[h1] <- ties4[h1] + 1; 
							wins4[h1] <- wins4[h1] + H2HW[h1,h2]; loss4[h1] <- loss4[h1] + H2HL[h1,h2] }
}}}

results <- cbind(results,ties4,wins4,loss4)

results$rank5 <- ave(-1 * ((results$WC-results$LC) + results$WC/100 + (results$wins1+results$loss1)/100000
					+ (results$wins2+results$loss2)/10000000 + (results$WD-results$LD)/10000000000
					+ (results$wins4+results$loss4)/100000000000), 
					results$Conference, FUN = function(x) rank(x, ties.method = "min"))

results$rank6 <- ave(-1 * ((results$WC-results$LC) + results$WC/100 + (results$wins1+results$loss1)/100000
					+ (results$wins2+results$loss2)/10000000 + (results$WD-results$LD)/10000000000
					+ (results$wins4+results$loss4)/100000000000 + (results$L)/10000000000000), 
					results$Conference, FUN = function(x) rank(x, ties.method = "random"))

