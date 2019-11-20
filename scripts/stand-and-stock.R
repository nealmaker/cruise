load("C:/Users/Neal/projects/cruise/rda/buck-brook-cruise-2019.rda")
library("tidyverse")
library("knitr")
library("kableExtra")

stand <- 1
ac <- (stands[stand,] %>% .$acres_calc)
plts <- (stands[stand,] %>% .$plots)

#Veneer
trees_veneer <- 
  unique(filter(logs, stand == stand, grade == 1)$tree)

temp1 <- logs %>% 
  filter(stand == stand,
         grade == 1) %>% 
  group_by(spp) %>% 
  summarize(vol1_ac = sum(vol_ac)/plts,
            vol1_total = vol1_ac * ac)

temp2 <- logs %>% 
  filter(tree %in% trees_veneer,
         grade %in% 2:3) %>% 
  group_by(spp) %>% 
  summarize(vol2_ac = sum(vol_ac)/plts,
            vol2_total = vol2_ac * ac) 

temp3 <- logs %>% 
  filter(tree %in% trees_veneer,
         grade == 5) %>% 
  group_by(spp) %>% 
  summarize(vol5_ac = sum(vol_ac)/plts,
            vol5_total = vol5_ac * ac)

temp4 <- trees %>% 
  filter(tree %in% trees_veneer) %>% 
  group_by(spp) %>% 
  summarize(ba_ac = sum(0.005454*(dbh^2)*tpa)/plts,
            tpa = sum(tpa)/plts) %>% 
  arrange(desc(ba_ac)) 

temp_veneer <- temp4 %>% 
  full_join(temp1) %>% 
  full_join(temp2) %>% 
  full_join(temp3) %>% 
mutate(ba_ac = round(ba_ac, 1),
       tpa = round(tpa, 1),
       vol1_ac = round(vol1_ac),
       vol1_total = round(vol1_total/1000, 2),
       vol2_ac = round(vol2_ac),
       vol2_total = round(vol2_total/1000, 2),
       vol5_ac = round(vol5_ac),
       vol5_total = round(vol5_total/1000, 2)) %>% 
  select(spp, ba_ac, tpa, vol1_ac, vol2_ac, vol5_ac, 
         vol1_total, vol2_total, vol5_total)

#st
trees_temp <- 
  unique(filter(logs, stand == stand, grade %in% 2:3)$tree)

trees_st <- trees_temp[!(trees_temp %in% trees_veneer)]

temp2 <- logs %>% 
  filter(tree %in% trees_st,
         grade %in% 2:3) %>% 
  group_by(spp) %>% 
  summarize(vol2_ac = sum(vol_ac)/plts,
            vol2_total = vol2_ac * ac) 

temp3 <- logs %>% 
  filter(tree %in% trees_st,
         grade == 5) %>% 
  group_by(spp) %>% 
  summarize(vol5_ac = sum(vol_ac)/plts,
            vol5_total = vol5_ac * ac)

temp4 <- trees %>% 
  filter(tree %in% trees_st) %>% 
  group_by(spp) %>% 
  summarize(ba_ac = sum(0.005454*(dbh^2)*tpa)/plts,
            tpa = sum(tpa)/plts) %>% 
  arrange(desc(ba_ac)) 

temp_st <- temp4 %>% 
  full_join(temp2) %>% 
  full_join(temp3) %>% 
  mutate(ba_ac = round(ba_ac, 1),
         tpa = round(tpa, 1),
         vol1_ac = " ",
         vol1_total = " ",
         vol2_ac = round(vol2_ac),
         vol2_total = round(vol2_total/1000, 1),
         vol5_ac = round(vol5_ac),
         vol5_total = round(vol5_total/1000, 1)) %>% 
  select(spp, ba_ac, tpa, vol1_ac, vol2_ac, vol5_ac, 
         vol1_total, vol2_total, vol5_total)
