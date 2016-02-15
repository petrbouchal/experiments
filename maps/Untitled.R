library(foreign)
library(readr)

ciselnik <- read.dbf("~/Downloads/PS2013reg20131026/PSRK.dbf",as.is = TRUE)

ciselnik[c(4:7,9:13)] <- lapply(ciselnik[c(4:7,9:13)], iconv, from="CP852",to="UTF-8")

filepath <- "~/Downloads/psp_2013_obce.csv"
p2013 <- read.csv(filepath, fileEncoding = "WINDOWS-1250", sep=";")

tmp <- tempfile()
download.file("http://ovolby.cz/data/psp_2013_obce.csv.gz", destfile = tmp)

zz <- gzcon(url("http://ovolby.cz/data/psp_2013_obce.csv.gz"))
dd <- textConnection(readLines(zz,encoding = "WINDOWS-1250"), encoding = "UTF-8")
close(zz)
p2013 <- read.csv(dd, fileEncoding = "WINDOWS-1250", sep=";")
close(dd)

ciselnik$tituly <- nchar(ciselnik$TITULPRED) + nchar(ciselnik$TITULZA)
ciselnik$zena <- grepl("á$", ciselnik$PRIJMENI)
plot(ciselnik$tituly, ciselnik$POCPROCVSE)
cor(ciselnik$tituly, ciselnik$POCPROCVSE)

ciselnik$bezpp <- ciselnik$PSTRANA==99
ciselnik$veksq <- ciselnik$VEK*ciselnik$VEK

ciselnik$starosta <- grepl("[Ss]tarost",ciselnik$POVOLANI)
ciselnik$poslanec <- grepl("[Pp]oslan",ciselnik$POVOLANI)
ciselnik$politik <- grepl("[Pp]oliti",ciselnik$POVOLANI)
ciselnik$lekar <- grepl("[Ll]ékař",ciselnik$POVOLANI)

ciselnik$kraj <- as.character(ciselnik$VOLKRAJ)

ciselnik <- ciselnik[!is.na(ciselnik$VEK),]

model <- lm(POCPROCVSE ~ tituly + PORCISLO + NSTRANA + zena + VEK + veksq + bezpp + kraj + starosta + poslanec + lekar + politik,
           data=ciselnik)
model <- lm(POCPROCVSE ~ tituly + VEK + veksq,
           data=ciselnik)

ciselnik$predicted <- predict(model)
plot(ciselnik$VEK, ciselnik$predicted)
plot(ciselnik$VEK, ciselnik$POCPROCVSE)
summary(model)

library(sjPlot)
sjp.lm(model)
sjp.lm(model, type="ma", showOriginalModelOnly = FALSE)

xmax <- -model$coefficients["VEK"]/(2*model$coefficients["veksq"])

ciselnik$high <- ciselnik$VEK > xmax
table(is.na(ciselnik$high))
ciselnik$xhigh <- ifelse(ciselnik$high == TRUE, ciselnik$VEK-xmax, 0)
ciselnik$xlow <- ifelse(ciselnik$high == FALSE, ciselnik$VEK-xmax, 0)
table(is.na(ciselnik$xhigh))
table(is.na(ciselnik$xlow))

model2 <- lm(POCPROCVSE ~ xhigh + xlow + high, data=ciselnik)
summary(model2)
