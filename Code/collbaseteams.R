

for (i in c(2002:2018,2020:2022)) {

bb <- read.csv(paste("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/collbase/base",i,".csv",sep=""))
names(bb)[names(bb) == "codes"] <- paste("codes",i,sep="")
assign(paste("bb",i,sep=""),bb)
}

super <- merge(bb2022,bb2021,by="names",all=T)
super <- merge(super ,bb2020,by="names",all=T)
super <- merge(super ,bb2018,by="names",all=T)
super <- merge(super ,bb2017,by="names",all=T)
super <- merge(super ,bb2016,by="names",all=T)
super <- merge(super ,bb2015,by="names",all=T)
super <- merge(super ,bb2014,by="names",all=T)
super <- merge(super ,bb2013,by="names",all=T)
super <- merge(super ,bb2012,by="names",all=T)
super <- merge(super ,bb2011,by="names",all=T)
super <- merge(super ,bb2010,by="names",all=T)
super <- merge(super ,bb2009,by="names",all=T)
super <- merge(super ,bb2008,by="names",all=T)
super <- merge(super ,bb2007,by="names",all=T)
super <- merge(super ,bb2006,by="names",all=T)
super <- merge(super ,bb2005,by="names",all=T)
super <- merge(super ,bb2004,by="names",all=T)
super <- merge(super ,bb2003,by="names",all=T)
super <- merge(super ,bb2002,by="names",all=T)

super$codes <- apply(super[,c(2:21)], 1, max, na.rm=T)

write.csv(super, file = "C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/collbase/super.csv")

