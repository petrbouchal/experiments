library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(forcats)
library(lubridate)
library(downloader)

mmd <- read.xlsx("./makeovers/DC Metro Scorecard.xlsx")

varswhereupisbad <- c("Crimes","Employee Injury Rate","Customer Injury Rate")

mmdx <- mmd %>% group_by(Year, Month) %>% 
  gather(key, value, -Year,-Month) %>% 
  filter(Month != "YTD") %>%
  mutate(type = ifelse(str_detect(key, "arget"),"target","measure"),
         key = str_replace(key, c(".Target"),""), 
         key = str_replace(key, "\\.\\(per.200K.hours\\)",""), 
         key = str_replace(key, "\\.\\(per.1M.passengers\\)",""),
         key = str_replace_all(key, "\\."," "),
         upisgood = ifelse(key %in% varswhereupisbad, "FALSE","TRUE"),
         yearnum = str_extract(Year, "[0-9]{4}"),
         date = paste("01", Month, yearnum),
         datex = paste("01", Month, "2016"),
         date2 = as.Date(date, format = "%d %b %Y"),
         datex = as.Date(datex, format = "%d %b %Y")) %>%
  ungroup() %>% 
  mutate(Month = month(date2)) %>%
  spread(type, value) %>% 
  mutate(key = as.factor(key),
         key = fct_relevel(key, c("Bus On-Time Performance",
                                  "Rail On-Time Performance",
                                  "Bus Fleet Reliability",
                                  "Rail Fleet Reliability",
                                  "Elevator Reliability",
                                  "Escalator Reliability",
                                  "Customer Injury Rate",
                                  "Employee Injury Rate",
                                  "Crimes")))

mmdp <- mmdx %>%
  ggplot(aes(x=datex, group=yearnum)) +
  geom_ribbon(data = mmdx[mmdx$yearnum=="2015",],
              aes(ymin=ifelse(upisgood,0,target),
                  ymax=ifelse(upisgood,target,Inf)), fill="red", alpha = 0.2) +
  geom_ribbon(data = mmdx[mmdx$yearnum=="2015",],
              aes(ymin=ifelse(upisgood,target,0),
                  ymax=ifelse(upisgood,Inf,target)), fill="lightgreen", alpha = 0.4) +
  geom_line(aes(y=measure, colour=yearnum, size=yearnum)) +
  facet_wrap(~key, scales = "free", nrow = 5) +
  scale_colour_manual(values = c("lightgrey","grey","blue"), name="Year") +
  scale_size_manual(values = c(.5,.5,1.2), guide = "none") +
  geom_point(data = mmdx[mmdx$yearnum==2016 & mmdx$Month == 9,], colour="blue",
             aes(y=measure)) +
  scale_y_continuous(expand = c(0.05,0)) +
  scale_x_date(date_labels = "%b", expand = c(0,0)) +
  theme_minimal() +
  ggtitle(label = "DC Transport key performance metrics",
          subtitle = "Green area is performance within target, red is underperformance") +
  theme(legend.position = c(0.7, 0),
        legend.direction = "vertical",
        legend.justification = c(1,1),
        axis.title = element_blank(),
        strip.text = element_text(hjust=0, size=12, face="bold"),
        plot.margin = unit(c(1,1,1,1),"char"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="white"))

cairo_pdf(filename = "makeovers/wk51.pdf")
mmdp
dev.off()