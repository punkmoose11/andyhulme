setwd("C:/Users/andyh/Documents/OMAHASERIES/Code")
library("XML")
library(httr)

# download list of teams with numberic codes
#teams <- "http://stats.ncaa.org/team/inst_team_list?academic_year=2021&conf_id=-1&division=1&sport_code=MBA"
#teams1<- htmlParse(teams)
#teams2<- xpathApply(teams1, "//div[@class='css-panes']/div/table/tr/td/table/tr/td/a")
#teams3<- xpathApply(teams1, "//div[@class='css-panes']/div/table/tr/td/table/tr/td/a/@href")#
#
#names <- sapply(teams2, function(x) xmlValue(x))
#codes <- gsub("(/team/)(\\d+)(/\\d+)", "\\2", teams3)
#map <- cbind(names,codes)
#write.csv(map, file = "base.csv", quote=FALSE, row.names = FALSE)
#######################################################################################################

bb <- read.csv("base2.csv")
bb$codes2 <- bb$codes; bb$myname2 <- bb$myname; bb$conf2 <- bb$conf;

for (i in 1:length(bb$codes)) {

# read team pages
u1<- paste("http://stats.ncaa.org/team/",bb$codes[i],"/16340",sep="")

#for (j in 1:10) {
#r1<- GET(u1)
#  if (r1$status_code!=200){print(paste(j,"Failure:",bb$names[i])) }
#  else {print(paste(j,"Success:",bb$names[i])); break;}
#}

h1<- htmlParse(u1)
print(paste(i,bb$names[i]))

# game level
games<- xpathApply(h1, "//table[1]/tr[1]/td[1]/fieldset/table[1]/tbody/tr")
team   <- rep(paste(bb$names[i]), times = length(games))
date   <- vector(,length(games))
opp   <- vector(,length(games))
score <- vector(,length(games))

for (j in seq(1, length(games), 2)) { 
w<- xpathApply(h1, paste("//table[1]/tr[1]/td[1]/fieldset/table[1]/tbody/tr[",j,"]/td[1]",sep=""))
x<- xpathApply(h1, paste("//table[1]/tr[1]/td[1]/fieldset/table[1]/tbody/tr[",j,"]/td[2]",sep=""))
y<- xpathApply(h1, paste("//table[1]/tr[1]/td[1]/fieldset/table[1]/tbody/tr[",j,"]/td[3]",sep=""))

date[j] <- gsub("^\\s+|\\s+$", "",gsub("\t","",gsub("\n","",xmlValue(w[[1]]))))
opp[j]  <- gsub("^\\s+|\\s+$", "",gsub("\t","",gsub("\n","",xmlValue(x[[1]]))))
score[j]<- gsub("^\\s+|\\s+$", "",gsub("\t","",gsub("\n","",xmlValue(y[[1]]))))
}

bytm <- cbind(team,date,opp,score)
if (i>1) {sched <- rbind(sched,bytm)} 
else if (i==1) {sched <- bytm}
}

write.csv(sched, file = "bbsched.csv", quote=TRUE, row.names = FALSE)
#######################################################################################################

sched <- read.csv("bbsched.csv")
sched <- subset(sched, date!="FALSE")
sched <- subset(sched, score!="Canceled")

sched$loc <- ifelse(substring(sched$opp,1,1)=="@","A",
			ifelse(grepl("@",substring(sched$opp,4)),"N","H"))

sched$opp <- gsub("^@\\s+", "", sched$opp)
sched$loc2<- ifelse(grepl("@",sched$opp),gsub("(.*)(@\\s?)(.*)", "\\3", sched$opp),"")
sched$opp <- gsub("\\s?@.*$", "", sched$opp)

sched$opp <- ifelse(sched$opp=="Loyola Marymount","LMU (CA)",sched$opp)
sched$opp <- ifelse(sched$opp=="Nicholls St.","Nicholls",sched$opp)
sched$opp <- ifelse(sched$opp=="UAlbany","Albany",sched$opp)
sched$opp <- ifelse(sched$opp=="Fairleigh Dickinson","FDU",sched$opp)

sched <- merge(sched,bb[c("names","codes","myname","conf")],by.x="team",by.y="names",all.x=TRUE)
sched <- merge(sched,bb[c("names","codes2","myname2","conf2")],by.x="opp",by.y="names",all.x=TRUE)

subset(sched, is.na(codes2))$opp

sched$yday <- as.numeric(as.Date(sched$date, "%m/%d/%Y") - as.Date("02/20/2021", "%m/%d/%Y"))
sched$week <- floor(sched$yday/7)

############################ HARDCODE games to non-conference #########################################
sched$cg <- ifelse(sched$conf==sched$conf2,"C","")


#######################################################################################################

sched$result <- ifelse(sched$score=="","P",substring(sched$score,1,1))
sched$resultc<- paste(ifelse(sched$cg=="C","C","N"),sched$result,sep="")
sched$resultl<- paste(sched$result,sched$loc,sep="")
sched$resultd<- ifelse(is.na(sched$codes2),"XXX",paste("R",sched$result,sched$loc,sep=""))

sched$rf <- ifelse(sched$result=="P","",gsub("(\\w\\s)(\\d+)(-)(\\d+)(.*)", "\\2", sched$score))
sched$ra <- ifelse(sched$result=="P","",gsub("(\\w\\s)(\\d+)(-)(\\d+)(.*)", "\\4", sched$score))
sched$inn<- ifelse(sched$result=="P","",gsub("(\\w\\s)(\\d+)(-)(\\d+)(.*)", "\\5", sched$score))

WL <- as.data.frame.matrix(table(sched$myname,sched$result ));  WL$myname<- rownames( WL);
RF <- aggregate(as.numeric(sched$rf), list(sched$myname), sum, na.rm=TRUE); names(RF)[names(RF)=="x"] <- "rf";
RA <- aggregate(as.numeric(sched$ra), list(sched$myname), sum, na.rm=TRUE); names(RA)[names(RA)=="x"] <- "ra";
CWL<- as.data.frame.matrix(table(sched$myname,sched$resultc)); CWL$myname<- rownames(CWL);
	if (! "CW" %in% names(CWL)) {CWL$CW<-0}; 
	if (! "CL" %in% names(CWL)) {CWL$CL<-0}; 
	if (! "CT" %in% names(CWL)) {CWL$CT<-0};
HA <- as.data.frame.matrix(table(sched$myname,sched$resultl));  HA$myname<- rownames( HA);
RPI<- as.data.frame.matrix(table(sched$myname,sched$resultd)); RPI$myname<- rownames(RPI);

summ <- merge(bb[c("myname","codes","conf")],WL,by="myname",all.x=TRUE)
summ <- merge(summ,RF ,by.x="myname",by.y="Group.1",all.x=TRUE)
summ <- merge(summ,RA ,by.x="myname",by.y="Group.1",all.x=TRUE)
summ <- merge(summ,CWL,by="myname",all.x=TRUE)
summ <- merge(summ,HA ,by="myname",all.x=TRUE)
summ <- merge(summ,RPI,by="myname",all.x=TRUE)
summ$pts <- summ$CW - summ$CL

summ$maxpt <- ave( summ$pts, summ$conf, FUN = function(x) max(x) )
summ$diff  <- summ$maxpt-summ$pts

summ$GB <- ifelse(summ$diff==0,"&#8212;",
		ifelse(summ$diff==1,"&#189;", 
		ifelse(summ$diff %% 2==1,paste((summ$diff-1)/2,"&#189;",sep=""),paste(summ$diff/2))))

summ$pct <- summ$W / (summ$W + summ$L)
summ$rank <- rank(-summ$pct, ties.method="min")

##########################################
############# generate header / footer ###
##########################################

header <- vector()
header[1] <- '<html><head><title>College World Series</title>'
header[2] <- '<link rel="stylesheet" type="text/css" href="styles.css"></head>' 
header[3] <- '<body><div id="red"><h1><a class="red" href="index.html">College World Series</a></h1></div><div id="yellow"></div>'
header[4] <- '<div id="blue"><h4>2023 STANDINGS</h4></div>'
footer <- vector()
footer[1] <- '<div id="foot"> </div></body></html>'

##### generate conference page #####
summ$class <- 0

summ$row <-  paste("<tr class='d",summ$class,"'><td>",paste(
				paste(summ$myname,sep=""),
				paste(summ$CP+summ$CW+summ$CL+summ$CT,summ$CW+summ$CL+summ$CT,sep=" / "),
				paste(summ$CW,"-",summ$CL,ifelse(summ$CT>0,paste("-",summ$CT,sep=""),""),sep=""),
				summ$GB,
				paste(summ$P+summ$W+summ$L+summ$T,summ$W+summ$L+summ$T,sep=" / "),
				paste(summ$W,"-",summ$L,ifelse(summ$T>0,paste("-",summ$T,sep=""),""),sep=""),
				paste(summ$rf,summ$ra,sep="-"),
			sep="</td><td align='center'>"),"</td></tr>",sep="")

summ <- summ[order(summ$conf, -summ$CW+summ$CL, -summ$CW, -summ$W+summ$L, -summ$W, summ$myname),]

u.conf <- unique(summ$conf)

sink("../CWS/cbstand.html")
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("<br><h3><center> College Baseball Conference Standings &#8212; ",format(Sys.Date(), format="%B %d"),"</center></h3><br>")

for (i in 1:length(u.conf))
{
#cat(paste("<br><a name='",paste(subset(summ,conf == u.conf[i])["codes"]),"'></a>",sep=""))
cat("<h3><center>",paste(u.conf[i]),"</center></h3><table align='center' width='70%'>")
#cat("<col width='15%'><col width='5%'><col width='5%'><col width='25%'><col width='10%'><col width='40%'>")
cat("<tr><th colspan='1'></th><th colspan='3' class='hl'>Conference</th><th colspan='3' class='hl'>Overall</th></tr>")
cat("<tr><th></th><th>Sch / Pld</th><th>Record</th><th>GB</th><th>Sch / Pld</th><th>Record</th><th>RF-RA</th></tr>")
write.table(subset(summ,conf == u.conf[i])["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
}

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

subset(summ[order(-summ$pct, -summ$W),], rank <= 50)[c("myname","W","L","pct","rank")]

#######################################################################################

summ$index <- rank(summ$myname)

sched2 <- merge(sched ,summ[c("myname","index")],by.x="myname" ,by.y="myname")
sched2 <- merge(sched2,summ[c("myname","index")],by.x="myname2",by.y="myname")

W <- matrix(0, nrow=nrow(summ), ncol=nrow(summ))
L <- matrix(0, nrow=nrow(summ), ncol=nrow(summ))
R <- matrix("0-0", nrow=nrow(summ), ncol=nrow(summ))

sched2 <- subset(sched2, result=="W" | result=="L")

for (i in 1:nrow(sched2)) {

if (sched2$result[i]=="W") {W[sched2$index.x[i],sched2$index.y[i]] <- W[sched2$index.x[i],sched2$index.y[i]] + 1}
if (sched2$result[i]=="L") {L[sched2$index.x[i],sched2$index.y[i]] <- L[sched2$index.x[i],sched2$index.y[i]] + 1}

}

for (ii in 1:nrow(summ)) {for (jj in 1:nrow(summ)) {R[ii,jj] <- paste(W[ii,jj],L[ii,jj],sep="-") }}

summ$cpct <- summ$CW / (summ$CW + summ$CL)

sec <- subset(summ, conf=="Big12")
sec <- sec[order(-sec$cpct, sec$myname),]
sec <- sec[c("index","myname","CW","CL")]

blanks <- as.data.frame(matrix("XXX", nrow=nrow(sec), ncol=nrow(sec)))
blanks <- data.frame(lapply(blanks, as.character), stringsAsFactors=FALSE)
sec <- cbind(sec,blanks)

for (i in 1:nrow(sec)) {
  for (j in 1:nrow(sec)) {
     sec[i,ncol(sec)-nrow(sec)+j] <- R[sec$index[i],sec$index[j]]
}}

for (i in 1:nrow(sec)) {names(sec)[ncol(sec)-nrow(sec)+i] <- substring(sec$myname[i],1,3)}
sec

####################################################################################

elo <- subset(sched2, index.x < index.y & result!="T")
elo$win <- ifelse(elo$result=="W",1,0)
elo <- elo[c("date","index.x","team","rf","index.y","opp","ra","result","win","loc")]
elo <- elo[order(elo$date, elo$index.x),]



