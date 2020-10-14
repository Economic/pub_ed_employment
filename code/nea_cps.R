library(tidyverse)
library(epiextractr)

#load CPS basic data from 2006 to 2019
cps06_19 <-load_cps(years=2006:2019, sample="basic") %>% 
  filter(age>=16) %>% 
  # exclude self-employed workers
  filter(selfemp==0) %>% 
  # keep education industries only
  filter(mind16==14) 

#load CPS basic data from Jan 2020 to Sept 2020
cps20 <- load_cps(year=2020, months=1:9, sample="basic") %>% 
  filter(age>=16) %>% 
  # exclude self-employed workers
  filter(selfemp==0) %>% 
  # keep education industries only
  filter(mind16==14)


# combine dataset, Jan 2006 to Sept 2020
cps <- rbind(cps06_19, cps20) %>% 
  mutate(date = as.Date(paste(year, month, 1, sep = "-"), "%Y-%m-%d")) 
  

### data sets for public state and public local education urates ### 

pubst_unemp <- cps %>% 
  #Keep public state ed employees only
  filter(pubst==1) %>% 
  group_by(year, month) %>% 
  summarize(pub_state_unemp = weighted.mean(unemp, w = cmpwgt)) %>% 
  mutate(date = as.Date(paste(year, month, 1, sep = "-"), "%Y-%m-%d")) %>% 
  select(date, pub_state_unemp)

publoc_unemp <- cps %>% 
  #Keep public local ed employees only
  filter(publoc==1) %>% 
  group_by(year, month) %>% 
  summarize(pub_local_unemp = weighted.mean(unemp, w = cmpwgt)) %>% 
  mutate(date = as.Date(paste(year, month, 1, sep = "-"), "%Y-%m-%d")) %>% 
  select(date, pub_local_unemp)


### data sets for public state and public local education employment levels ### 

pubst_emp <- cps %>% 
  #employed workers only
  filter(emp==1) %>% 
  #Keep public state ed employees only
  filter(pubst==1) %>% 
  mutate(date = as.Date(paste(year, month, 1, sep = "-"), "%Y-%m-%d")) %>% 
  select(date, year, month, emp, cmpwgt)
  
publoc_emp <- cps %>% 
  #keep employed workers only
  filter(emp==1) %>% 
  #Keep public local ed employees only
  filter(publoc==1) %>% 
  mutate(date = as.Date(paste(year, month, 1, sep = "-"), "%Y-%m-%d")) %>% 
  select(date, year, month, emp, cmpwgt)


# count monthly employment levels for state ed employees
st_emp <- pubst_emp %>% 
  select(date, emp, cmpwgt) %>% 
  count(emp, date, name = "pubst_emp", wt=cmpwgt)

# count monthly employment levels for local ed employees
loc_emp <- publoc_emp %>% 
  select(date, emp, cmpwgt) %>% 
  count(emp, date, name = "publoc_emp", wt=cmpwgt)


# join and export state+local ed monthly emp csv
emplvls <- full_join(st_emp, loc_emp, "date") %>% 
  select(date, pubst_emp, publoc_emp) %>% 
  write_csv(path = "data/educ_emp.csv")

# join and export state+local ed monthly urate csv
urates <- full_join(x = pubst_unemp, y=publoc_unemp, "date") %>%
  select(date, pub_state_unemp, pub_local_unemp) %>% 
  write_csv(path = "data/educ_unemp.csv")

