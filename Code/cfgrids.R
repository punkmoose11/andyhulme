
header <- vector()
header[1] <- '<html><head><title>College Football</title>'
header[2] <- '<link rel="stylesheet" type="text/css" href="styles.css"></head>' 
header[3] <- '<body><div id="red"><h1>College Football</h1></div>'
header[4] <- '<div id="head"><table class="head"><tr class="head">
<td class="head" style="text-align:left; width:75%;"><!--Insert Link--> &nbsp;</td>
<td class="head" style="text-align:right; width:25%;">
<a href="byyear.html">Years</a> - <a href="byteam.html">Teams</a> - <a href="index.html">Home</a>
</td></tr></table></div>'

footer <- vector()
footer[1] <- '<div id="foot"> <a class="foot" href="../index.html">AndyHulme.Net</a></div></body></html>'

######

seasons <- read.csv("../Data/cfseasons.csv")
cfgames <- read.csv("../Data/cfgames.csv")
names(seasons)[names(seasons) == "conf"] <- "conference"
cfgames$result <- ifelse(cfgames$points1>cfgames$points2,"W",ifelse(cfgames$points1<cfgames$points2,"L",
				ifelse(cfgames$points1==cfgames$points2,"T","")))

## Find sorts ##
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
summ4$OT1 <- summ4$OT - summ4$TC
summ4$opct <- (summ4$OW1 + summ4$OT1/2) / (summ4$OW1 + summ4$OL1 + summ4$OT1)
summ4$dpct <- (summ4$WD  + summ4$TD /2) / (summ4$WD  + summ4$LD  + summ4$TD )
summ4$hhpct <- ifelse(is.na(summ4$WHH)==F, (summ4$WHH + summ4$THH/2) / (summ4$WHH + summ4$LHH + summ4$THH),0.5)

summ4 <- summ4[order(summ4$year, summ4$conference, summ4$division, -summ4$cpct, -summ4$hhpct, -summ4$dpct, -summ4$opct, summ4$team), ]
summ4$sort1 <- 1:nrow(summ4)
summ4$sort2 <- ave(summ4$sort1, paste(summ4$year,summ4$conference,summ4$division), FUN = function(x) rank(x, ties.method = "min") )

#################

#Find grids#
for (YEAR in 1895:max(summ4$year)) {
summ <- subset(summ4, year==YEAR)
sched <- subset(cfgames, season==YEAR)

summ$index <- rank(summ$team)

sched2 <- merge(sched ,summ[c("team","index")],by.x="team1",by.y="team")
sched2 <- merge(sched2,summ[c("team","index")],by.x="team2",by.y="team")

W <- matrix(0, nrow=nrow(summ), ncol=nrow(summ))
L <- matrix(0, nrow=nrow(summ), ncol=nrow(summ))
D <- matrix(0, nrow=nrow(summ), ncol=nrow(summ))
Q <- matrix("0-0", nrow=nrow(summ), ncol=nrow(summ))
R <- matrix("0-0", nrow=nrow(summ), ncol=nrow(summ))

sched2 <- subset(sched2, conf=="C")

for (i in 1:nrow(sched2)) {

if (sched2$result[i]=="W") {W[sched2$index.x[i],sched2$index.y[i]] <- W[sched2$index.x[i],sched2$index.y[i]] + 1}
if (sched2$result[i]=="L") {L[sched2$index.x[i],sched2$index.y[i]] <- L[sched2$index.x[i],sched2$index.y[i]] + 1}
if (sched2$result[i]=="T") {D[sched2$index.x[i],sched2$index.y[i]] <- D[sched2$index.x[i],sched2$index.y[i]] + 1}

}

for (ii in 1:nrow(summ)) {for (jj in 1:nrow(summ)) {Q[ii,jj] <- paste(W[ii,jj],L[ii,jj],D[ii,jj],sep="-") }}

for (ii in 1:nrow(summ)) {for (jj in 1:nrow(summ)) { R[ii,jj] <- 
ifelse(Q[ii,jj]=="1-0-0","<h7>&nbsp;&nbsp;W&nbsp;&nbsp;</h7>",
ifelse(Q[ii,jj]=="0-1-0","<h8>&nbsp;&nbsp;L&nbsp;&nbsp;</h8>", 
ifelse(Q[ii,jj]=="0-0-1","<h9>&nbsp;&nbsp;T&nbsp;&nbsp;</h9>", 
"")))
}}

u.conf <- unique(subset(summ,conference!="Indep" & conference!="FCSIndep" & conference!="Partial" & conference!="Minor")$conference)

sink(paste("../CFB/",YEAR,"grids.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)

cat(paste("<br><h2><center>Conference Grids<br>",YEAR,"</center></h2><br>",sep=""))
cat(paste("<center><a href='grids",YEAR-1,".html'> << ",YEAR-1,"</a> | ",
	            "<a href='grids",YEAR+1,".html'>",YEAR+1," >> </a></center><br>",sep=""))

for (jj in 1:length(u.conf))
{
conf <- subset(summ, conference==u.conf[jj])
conf <- conf[order(conf$division, conf$sort2, conf$team),]
conf$H2H <- ifelse(is.na(conf$WHH),"",paste(conf$WHH,"-",conf$LHH,ifelse(conf$THH>0,paste("-",conf$THH,sep=""),""),sep=""))
conf$DIV <- ifelse(conf$division=="","",paste(conf$WD,"-",conf$LD,ifelse(conf$TD >0,paste("-",conf$TD ,sep=""),""),sep=""))
conf$PCT <- round(conf$opct*1000,digits=0)
conf <- conf[c("division","index","team","WC","LC","TC","H2H","DIV","PCT")]

blanks <- as.data.frame(matrix("XXX", nrow=nrow(conf), ncol=nrow(conf)))
blanks <- data.frame(lapply(blanks, as.character), stringsAsFactors=FALSE)
conf <- cbind(conf,blanks)

for (i in 1:nrow(conf)) {
  for (j in 1:nrow(conf)) {
     conf[i,ncol(conf)-nrow(conf)+j] <- R[conf$index[i],conf$index[j]]
}}

#####

for (i in 1:nrow(conf)) {names(conf)[ncol(conf)-nrow(conf)+i] <- substring(conf$team[i],1,3)}
#print(u.conf[jj])
#print(conf)

cat(paste("<table class='grid' width='95%' align='center'><tr><th>",u.conf[jj],"</th><th>Div</th><th>W</th><th>L</th><th>T</th><th>H2H</th><th>DR</th><th>OPct</th>",sep=""))
for (x in 10:ncol(conf)) {cat(paste("<th>",colnames(conf)[x],"</th>",sep=""))}
cat(paste("</tr>"))

for (kk in 1:nrow(conf)) {
cat(paste("<tr><td>",paste(conf$team[kk],conf$division[kk],conf$WC[kk],conf$LC[kk],conf$TC[kk],conf$H2H[kk],conf$DIV[kk],conf$PCT[kk],sep="</td><td>"),"</td>",sep=""))
for (y in 10:ncol(conf)) {cat(paste("<td>",conf[kk,y],"</td>",sep=""))}
cat(paste("</tr>"))
}
cat(paste("</table><br>"))
}

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
print(YEAR)
}


