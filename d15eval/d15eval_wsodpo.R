setwd("~/github/experiments/d15eval/")
source("./d15eval_setup.R")
setwd("~/github/experiments/d15eval/")

wsnej <- data %>% 
  group_by(wsOdpoNejlepsi) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))
wsnej
# View(wsnej)
