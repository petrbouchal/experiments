library(dplyr)

csbaseurl <- "http://scitace.prahounakole.cz/data/"

# download data

sources <- read.csv("cykloscitace/datasourcelist.csv")

i <- 1
newnames <- c("time","count1","count2","totalcount","tempave")
newnames_single <- c("time","count1","totalcount","tempave")
emptyfiles <- c("Chodov","Dubeč","Letňany","Podolí","Rohanské_nábřeží",
                "Vršovická","Vítkov","Šárecké_údolí")
singledirfiles <- c("Košíře")
for (rownum in 1:nrow(sources)) {
  if(sources[rownum,1] %in% paste0(emptyfiles,".csv")) { next }
  
  if(i==1) {
    csdata <- read.csv(paste0(csbaseurl, sources[rownum,1]), sep=";")
    if(sources[rownum,1] %in% paste0(singledirfiles,".csv")) {
      csdata <- csdata %>% 
        mutate(count2 = NA) %>% 
        select(time = 1, count1 = 2, count2 = 5, totalcount = 3, tempave = 4)
    }
  
    names(csdata) <- newnames
    csdata$misto <- sources[rownum,2]
  } else {
    print(sources[rownum,1])
    thisdata <- read.csv(paste0(csbaseurl, sources[rownum,1]), sep=";")
    
    if(sources[rownum,1] %in% paste0(singledirfiles,".csv")) {
      thisdata <- thisdata %>% 
        mutate(count2 = NA) %>% 
        select(time = 1, count1 = 2, count2 = 5, totalcount = 3, tempave = 4)
    }
    
    names(thisdata) <- newnames
    thisdata$misto <- sources[rownum,2]
    csdata <- rbind(csdata, thisdata)
  }
  i <- i+1
}
rm(thisdata)
save(csdata, file="cykloscitace/cyklodata_all.Rda")
