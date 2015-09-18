# Tento kód generuje dataset pro zkušební úlohu č. 2 ZD evaluace

library(dplyr)

# Projekty

projectID <- paste0("projekt_",ceiling(runif(300,min=0, max=6000)))
length(unique(projectID)) # are project IDs unique? - some duplicates are good

projectCash <- abs(rnorm(300,mean=1000000, sd=100000))
hist(projectCash, breaks=100)

projekty <- data.frame("projectID"=projectID, "projectCash"=projectCash)

projectCash[projectCash<quantile(projectCash)[2]] <- (projectCash[projectCash<quantile(projectCash)[2]])^1.2
hist(projectCash, breaks = 100)
summary(projectCash)
                    
# Obce

obceCZ <- read.csv("~/github/cz-elections/data-socec/obce2010.csv")

obceCZ$random <- rnorm(n=nrow(obceCZ),mean=0, sd=2)
obceCZ <- arrange(obceCZ, random)

obce123 <- head(obceCZ, n = 123)

# match the two randomly
for (i in 1:nrow(projekty)) {
  projekty$obecrealizace[i] <- obce123[runif(1, 0, 123),"KODOBCE"][1]
}

dtst <- merge(projekty, obce123, by.x="obecrealizace", by.y = "KODOBCE", all.x = TRUE)

table(obce123$KODOBCE %in% dtst$obecrealizace)

dtst <- dtst %>% group_by(obecrealizace) %>% mutate(cashvobci=sum(projectCash, na.rm=T))

dtst$pocetKOpred <- -6 + dtst$VekPrum*0.33 + .000001*dtst$cashvobci + round(rnorm(nrow(dtst),1.5,2))
plot(dtst$pocetKOpred, dtst$cashvobci)

  