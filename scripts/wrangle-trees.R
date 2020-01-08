file <- rchoose.files(caption = "Please select Excel file with cruise data:")

yn <- c("yes", "no")
fix_plots <- 
  rselect.list(yn, 
               title = 
                 "Reassign plots?",
               graphics = T)

library(XLConnect)
library(tidyverse)
library("here")


###############################
## Read in inventory worksheets
###############################

trees <- readWorksheetFromFile(file, sheet = 3, header = TRUE, endCol = 9)

stands_raw <- readWorksheetFromFile(file, sheet = 2, header = TRUE, endCol = 20)

prop <- readWorksheetFromFile(file, sheet = 1, header = FALSE, endCol = 2)


# error for incorrect species codes (before deleting them) ------------------
if(max((trees %>% filter(!is.na(code)))$code) > 25 | 
   min((trees %>% filter(!is.na(code)))$code) < 1){
  message("ERROR: tree(s) w/ nonexistant species code(s)")
  quit(save = "ask")
} 

# load height model & misc functions
load("../big-rdas/ht-model-op.rda")
source(here("scripts", "height.R"))
source(here("scripts", "pbal.R"))
source(here("scripts", "dib.R"))
source(here("scripts", "get-roadside.R"))


###############################
## Assign property vars
###############################

property <- prop[1,2]
month <- as.numeric(prop[2,2])
year <- as.numeric(prop[3,2])
forestname <- prop[5,2]
town <- prop[6,2]
glacres <- as.numeric(prop[7,2])
gldescrip <- prop[8,2]
span <- prop[9,2]
photo <- prop[10,2]
elevationmin <- as.numeric(prop[11,2])
elevationmax <- as.numeric(prop[12,2])
owners <- prop[13,2]
addressline1 <- prop[14,2]
addressline2 <- prop[15,2]
citystatezip <- prop[16,2]
watertext <- prop[17,2]
boundariestext <- prop[18,2]
plan_yr <- as.numeric(prop[20,2])
lat <- as.numeric(prop[21,2])
lon <- as.numeric(prop[22,2])
baf <- as.numeric(prop[23,2])


###############################
## Make tree, plot & stand dfs
###############################

trees <- trees %>% fill(plot, stand) %>% 
  filter(!is.na(spp)) %>% select(-(code)) %>%
  mutate(logs = as.character(logs),
         stand = as.numeric(stand),
         stand = round(stand),
         stand = as.character(stand),
         vigor = ifelse(is.na(vigor), 2, vigor),
         cut = ifelse(is.na(cut), 0, cut),
         cr = as.numeric(cr)*10,
         tpa = baf/(0.005454*dbh^2), 
         live = if_else(vigor == 5, 0, 1),
         # crop = if_else(cond == 1, 1, 0), Depreciate crop trees?
         # inv = if_else(cond %in% c(1:2), 1, 0), Depreciate investment grade?
         ags = if_else(vigor %in% c(1:3) & 
                         (str_detect(logs, "2") | str_detect(logs, "1")), 
                       1, 0),
         snag = if_else(vigor == 5, 1, 0),
         spp = as.factor(spp),
         sft = spp %in% c("fir", "cedar", "hemlock", "red pine",
                          "scots pine", "spruce", "tamarack",
                          "white pine", "other softwood")) %>%
  filter(dbh > 0)

plots <- trees %>% filter(dbh>=4) %>%
  group_by(plot) %>% 
  summarize(stand = stand[1],
            pct_sft = round(sum(live[sft == 1])/sum(live)*100),
            ba_live = baf*sum(live), 
            ba_ags = baf*sum(ags), 
            # ba_inv = baf*sum(inv),
            # ba_crop = baf*sum(crop),
            ba_snags = baf*sum(snag),
            tpa_live = round(sum(tpa[live == 1])),
            tpa_ags = round(sum(tpa[ags == 1])),
            # tpa_inv = round(sum(tpa[cond %in% c(1:2)])),
            # tpa_crop = round(sum(tpa[cond == 1])),
            tpa_snags = round(sum(tpa[snag]))) %>%
  mutate(qsd_live = round(sqrt((ba_live/tpa_live)/0.005454), 1),
         qsd_ags = ifelse(ba_ags > 0, round(sqrt((ba_ags/tpa_ags)/0.005454), 1), 0),
         # qsd_inv = ifelse(ba_inv > 0, round(sqrt((ba_inv/tpa_inv)/0.005454), 1), 0),
         # qsd_crop = ifelse(ba_crop > 0, round(sqrt((ba_crop/tpa_crop)/0.005454), 1), 0),
         qsd_snags = ifelse(ba_snags > 0, round(sqrt((ba_snags/tpa_snags)/0.005454), 1), 0)) %>%
  select(stand, plot, pct_sft, ba_live, ba_ags, qsd_live, 
         qsd_ags, tpa_live, tpa_ags, ba_snags)

# reassign plots to stands if that option was chosen
if(fix_plots == "yes"){
  # edit plots table by hand if you want to change stand assignments --------------
  fix(plots)
  
  # reasign stand numbers to trees table based on plot "fixes" --------------------
  trees <- trees %>% select(-stand) %>% 
    left_join(select(plots, plot, stand), "plot") %>%
    select(stand, plot, everything())
}
 
stands <- plots %>% 
  group_by(stand) %>%
  summarise(plots = n(),
            mean_ba = round(mean(ba_live), digits = 1),
            confint_ba = round(qnorm(.975)*sd(ba_live)/sqrt(n())),
            tpa = round(mean(tpa_live)),
            ba_ags = round(mean(ba_ags)),
            tpa_ags = round(mean(tpa_ags))) %>%
  mutate(qsd = round(sqrt((mean_ba/tpa)/.005454), digits = 1),
         qsd_ags = round(sqrt((ba_ags/tpa_ags)/.005454), digits = 1))


###############################
## Add qualitative data to stands
###############################

stands_raw <- stands_raw %>% 
  mutate(stand = as.character(stand),
         acres_calc = as.numeric(acres_calc),
         acres_legal = as.numeric(acres_legal)) %>%
  select(-type_code) %>%
  mutate(stand = as.character(stand),
         structure.1 = case_when(structure == 1 ~ "Even-aged",
                                 structure == 2 ~ "Two-aged",
                                 structure == 3 ~ "Uneven-aged")) %>% 
  filter(!is.na(stand))

stands <- stands %>% full_join(stands_raw)

rm(stands_raw)


###############################
## Add timber volume data
###############################

taper_coefs <- read_csv("data/taper-coefs.csv", col_names = TRUE) %>%
  mutate(spp = str_to_lower(spp),
         spp = factor(spp,
                      levels = levels(ht_model_op$trainingData$spp))) %>% 
  filter(!is.na(spp))

grade_thresholds <- read_csv("data/grade-thresholds.csv", col_names = TRUE) %>%
  mutate(spp = str_to_lower(spp),
         spp = factor(spp,
                      levels = levels(ht_model_op$trainingData$spp)))

#add height predictors & height to trees
trees <- trees %>% 
  group_by(plot) %>% 
  mutate(bal = pbal(dbh, 0.005454*(dbh^2)*tpa)) %>% 
  ungroup()

trees <- trees %>% 
  mutate(lat = lat,
         lon = lon) %>% 
  left_join(select(stands, stand, mean_ba, site_class, type),
            by = "stand") %>% 
  rename(ba = mean_ba,
         forest_type = type) %>% 
  mutate(spp = 
           factor(spp,
                  levels = 
                    levels(ht_model_op$trainingData$spp)),
         forest_type = 
           factor(forest_type,
                  levels = 
                    levels(ht_model_op$trainingData$forest_type_s)), 
         site_class = case_when(site_class == 4 ~ 4,
                                site_class == 5 ~ 5,
                                site_class == 6 ~ 6,
                                site_class == 7 ~ 7,
                                site_class == "I" ~ 4,
                                site_class == "II" ~ 5,
                                site_class == "III" ~ 6,
                                site_class == "IV" ~ 7,
                                TRUE ~ 5),
         ht = height(spp, dbh, forest_type, cr, ba, 
                     bal, lat, lon, site_class))


# add primary key to trees df -------------------------------------------------
trees <- trees %>% 
  mutate(tree = row_number()) %>% 
  left_join(taper_coefs, by = "spp")


# make logs df ----------------------------------------------------------------
logs <- trees %>%
  select(tree, plot, stand, spp, dbh, logs, sft, tpa, ht, theta1:beta2) %>%
  mutate(logs = str_trim(logs),
         # turn "*"s (nice veneer) into "0"s, so they play nice:
         logs = str_replace_all(logs, "\\*", "0"),
         # for older cruises, remove 9s that were hard stops & internal cull:
         logs = str_replace(logs, "9$", ","),
         logs = str_replace_all(logs, "9", "6"),
         # Make a column for each log section
         # (carry hard stops forward):
         grade1 = str_sub(logs, 1, 1),
         grade2 = ifelse(grade1 == ",", ",", str_sub(logs, 2, 2)),
         grade3 = ifelse(str_detect(grade2, ","), ",", str_sub(logs, 3, 3)),
         grade4 = ifelse(str_detect(grade3, ","), ",", str_sub(logs, 4, 4)),
         grade5 = ifelse(str_detect(grade4, ","), ",", str_sub(logs, 5, 5)),
         grade6 = ifelse(str_detect(grade5, ","), ",", str_sub(logs, 6, 6)),
         grade7 = ifelse(str_detect(grade6, ","), ",", str_sub(logs, 7, 7)),
         grade8 = ifelse(str_detect(grade7, ","), ",", str_sub(logs, 8, 8)),
         grade9 = ifelse(str_detect(grade8, ","), ",", str_sub(logs, 9, 9)),
         grade10 = ifelse(str_detect(grade9, ","), ",", str_sub(logs, 10, 10)),
         # Make sections above hard stops all 5s:
         grade1 = ifelse(str_detect(grade1, ","), 5, grade1),
         grade2 = ifelse(str_detect(grade2, ","), 5, grade2),
         grade3 = ifelse(str_detect(grade3, ","), 5, grade3),
         grade4 = ifelse(str_detect(grade4, ","), 5, grade4),
         grade5 = ifelse(str_detect(grade5, ","), 5, grade5),
         grade6 = ifelse(str_detect(grade6, ","), 5, grade6),
         grade7 = ifelse(str_detect(grade7, ","), 5, grade7),
         grade8 = ifelse(str_detect(grade8, ","), 5, grade8),
         grade9 = ifelse(str_detect(grade9, ","), 5, grade9),
         grade10 = ifelse(str_detect(grade10, ","), 5, grade10)) %>%
  # collapse section columns into "section" and "max grade" columns:
  gather(section, max_grade, grade1:grade10) %>%
  mutate(# populate undefined upper sections not above hard stops
         # (for softwoods they're 2s, for hardwoods they're 3s):
         max_grade = ifelse(str_detect(max_grade, "\\d"), max_grade, 
                            ifelse(sft, 2, 3)),
         max_grade = as.integer(max_grade),
         section = as.numeric(str_extract(section, "\\d+")),
         dib = dib(spp, dbh, ht, section, theta1, theta2,
                   alpha1, alpha2, gamma1, gamma2, psi,
                   lambda, beta1, beta2), 
         dib = if_else(dib > 0, dib, 0),
         dib = if_else(is.na(dib), 0, dib)) %>%
  left_join(grade_thresholds) %>%
  # determine current grade 
  mutate(grade = case_when(max_grade==0 & dib>=t0 ~ 0,
                           max_grade==0 & dib>=t1 ~ 1,
                           max_grade==0 & dib>=t2 ~ 2,
                           max_grade==0 & dib>=t3 ~ 3,
                           max_grade==0 & dib>=t5 ~ 5,
                           max_grade==0 & dib<t5 ~ 6,
                           max_grade==1 & dib>=t1 ~ 1,
                           max_grade==1 & dib>=t2 ~ 2,
                           max_grade==1 & dib>=t3 ~ 3,
                           max_grade==1 & dib>=t5 ~ 5,
                           max_grade==1 & dib<t5 ~ 6,
                           max_grade==2 & dib>=t2 ~ 2,
                           max_grade==2 & dib>=t3 ~ 3,
                           max_grade==2 & dib>=t5 ~ 5,
                           max_grade==2 & dib<t5 ~ 6,
                           max_grade==3 & dib>=t3 ~ 3,
                           max_grade==3 & dib>=t5 ~ 5,
                           max_grade==3 & dib<t5 ~ 6,
                           max_grade==5 & dib>=t5 ~ 5, 
                           TRUE ~ 6)) %>%
  # add log volumes with 1/4" international rule, convert to cords, 
  # & scale up to per acre 
  mutate(vol_log = (.905*(.22*dib^2-.71*dib)+
           .905*(.22*(dib+.5)^2-.71*(dib+.5)))/
           ((1/((554/(.15*dib^4.48)) + 1.9))*1000), 
         vol_log = ifelse(vol_log > 0, vol_log, 0),
         vol_log = ifelse(is.na(vol_log), 0, vol_log),
         vol_ac = vol_log*tpa) %>%
  filter(max_grade %in% 0:5,
         dib >= 4) %>% 
  select(tree:spp, tpa, max_grade, grade, dib, vol_log, vol_ac)


## ammend plots table with veneer, st, tie, and cordwd volumes ------------------
plots <- plots %>% 
  left_join((logs %>% 
               filter(grade %in% c(0, 1, 2, 3, 5)) %>%
               group_by(plot, grade) %>% 
               summarize(volume = sum(vol_ac)) %>% 
               # to keep grades that aren't represented:
               merge(data.frame(plot = c(9999999, 9999999, 9999999, 9999999, 9999999), 
                                grade = c(0, 1, 2, 3, 5), 
                                volume = c(0, 0, 0, 0, 0)), all = T) %>% 
               spread(grade, volume, fill = 0) %>% 
               filter(plot != 9999999) %>% 
               rename(nveneer_vol = '0', veneer_vol = '1', saw_vol = '2', 
                      tie_vol = '3', cord_vol = '5'))) %>%
  # turn 'na's to 0 & combine nice veneer and veneer:
  mutate(nveneer_vol = if_else(is.na(nveneer_vol), 0, nveneer_vol),
         veneer_vol = if_else(is.na(veneer_vol), 0, veneer_vol),
         veneer_vol = nveneer_vol + veneer_vol,
         saw_vol = if_else(is.na(saw_vol), 0, saw_vol),
         tie_vol = if_else(is.na(tie_vol), 0, tie_vol),
         cord_vol = if_else(is.na(cord_vol), 0, cord_vol)) %>% 
  select(-nveneer_vol)


# ammend stands table with volume/ac info --------------------------------------
stands <- stands %>% 
  left_join((plots %>% 
               group_by(stand) %>%
               summarize(veneer_vol = round(mean(veneer_vol)),
                         saw_vol = round(mean(saw_vol)),
                         tie_vol = round(mean(tie_vol)),
                         cord_vol = round(mean(cord_vol), 1)) %>%
               mutate(total_bf_vol = round(veneer_vol+saw_vol+tie_vol)) %>%
               select(stand, veneer_vol, saw_vol, tie_vol, 
                      total_bf_vol, cord_vol)))


###############################
## Add prices
###############################

priceit <- rselect.list(yn, 
                        title = "Add prices?",
                        graphics = T)

if(priceit == "yes"){
  
  repeat {
    trucking <- readline("trucking ($/mbf): ")
    trucking <- as.numeric(trucking)
    
    if(!is.na(trucking)) {
      break
    }
  }
  
  load(here("data", "prices.rda"))
  
  edit_prices <- rselect.list(yn,
                              title = "Update price table?",
                              graphics = T)
  
  if(edit_prices == "yes"){
    fix(prices)
    save(prices, file = here("data", "prices.rda"))
  }
  
  source(here("scripts", "stumpage.R"))
  
  logs <- logs %>% 
    filter(grade < 6) %>% 
    mutate(roadside_log = vol_log * get_roadside(spp, dib, max_grade),
           roadside_log = if_else(roadside_log > 0, roadside_log, 0),
           stump_log = stumpage(roadside_log, vol_log))
  
  trees <- trees %>% 
    left_join(summarize(group_by(logs, tree), 
                        vol = sum(vol_log),
                        roadside = sum(roadside_log),
                        stumpage = sum(stump_log))) %>% 
    mutate(vol = ifelse(is.na(vol), 0, vol),
           roadside = ifelse(is.na(roadside), 0, roadside),
           stumpage = ifelse(is.na(stumpage), 0, stumpage))
  
  plots <- plots %>% 
    left_join(summarize(group_by(trees, plot),
                        stump_ac = sum(stumpage * tpa)))
  
  stands <- stands %>% 
    left_join(summarize(group_by(plots, stand),
                        stump_ac = mean(stump_ac))) %>% 
    mutate(stump_total = stump_ac * acres_calc)
}


###############################
## Add stocking chart data 
###############################

stocking_charts <- read.csv("data/stocking-charts.csv", header = TRUE) %>%
  mutate(type = as.character(type),
         name = as.character(name),
         author = as.character(author),
         source = as.character(source),
         alab = as.character(alab),
         blab = as.character(blab),
         clab = as.character(clab))

stands <- stands %>%
  left_join(stocking_charts, by = "type")

rm(stocking_charts)


###############################
## Add objectives
###############################

if(!is.na(prop[19, 2])){
  objectives_raw <- read.csv("data/objectives.csv", header = TRUE) %>%
    mutate(id = as.factor(id),
           text = as.character(text))
  
  objectives <- prop %>%
    filter(Col1 == "objectives_codes") %>%
    mutate(id = Col2) %>%
    mutate(id = str_split(id, ",")) %>%
    unnest() %>%
    mutate(id = str_trim(id),
           id = as.factor(id)) %>%
    select(id) %>%
    left_join(objectives_raw, by = "id")
  
  # get latex formatting ---------
  objectives <- paste(objectives$text, collapse = " ") %>%
    str_replace_all("\\sn\\s", paste(" ", "\\\n\\\n", " ", sep = "")) %>%
    str_replace_all("^n", "\\\n\\\n") %>%
    str_replace_all(" hash ", "##")
}


###############################
## Add soils
###############################

soils_raw <- read.csv("data/soils.csv", header = TRUE) %>%
  mutate(id = as.factor(id),
         text = as.character(text))

soils <- stands %>%
  select(stand, soils_comma_separated) %>%
  mutate(id = soils_comma_separated,
         id = str_split(id, ",")) %>%
  unnest() %>%
  mutate(id = str_trim(id),
         id = str_remove(id, "\\D")) %>%
  left_join(soils_raw, by = "id") %>%
  select(stand, text) %>%
  mutate(text = str_trim(text),
         text = paste("START", text, "END", sep = " "))


###############################
## Data integrity errors
###############################

if(max(trees$dbh) > 40){
  message("ERROR: tree(s) > 40\" dbh")
  quit(save = "ask")
}

if(min(trees$dbh) < 1){
  message("ERROR: tree(s) < 1\" dbh")
  quit(save = "ask")
}

if(any(trees$dbh %% 1 != 0)){
  message("ERROR: tree(s) w/ fractional dbh")
  quit(save = "ask")
}

if(any(is.na(trees$logs))){
  message("ERROR: tree(s) w/ blank 'logs' field")
  quit(save = "ask")
}

if(any(trees$vigor > 5 | trees$vigor < 1)){
  message("ERROR: tree(s) w/ missing or incorrect 'vigor' field")
  quit(save = "ask")
}


###############################
## Save rda
###############################

if (priceit == "yes") {
  if ("objectives" %in% ls(envir = .GlobalEnv)) { 
    save(file, baf, logs, trees, plots, stands, plan_yr,
         grade_thresholds, property, month, year, forestname,
         town, glacres, gldescrip, span, photo, elevationmin,
         elevationmax, owners, addressline1, addressline2, citystatezip,
         watertext, boundariestext, objectives, soils, trucking, prices,
         file = paste("rda/", property, "-cruise-", year, ".rda", sep = ""))
  } else {
    save(file, baf, logs, trees, plots, stands, plan_yr,
         grade_thresholds, property, month, year, forestname,
         town, glacres, gldescrip, span, photo, elevationmin,
         elevationmax, owners, addressline1, addressline2, citystatezip,
         watertext, boundariestext, soils, trucking, prices,
         file = paste("rda/", property, "-cruise-", year, ".rda", sep = ""))
  } 
} else {
  if ("objectives" %in% ls(envir = .GlobalEnv)) { 
    save(file, baf, logs, trees, plots, stands, plan_yr,
         grade_thresholds, property, month, year, forestname,
         town, glacres, gldescrip, span, photo, elevationmin,
         elevationmax, owners, addressline1, addressline2, citystatezip,
         watertext, boundariestext, objectives, soils,
         file = paste("rda/", property, "-cruise-", year, ".rda", sep = ""))
  } else {
    save(file, baf, logs, trees, plots, stands, plan_yr,
         grade_thresholds, property, month, year, forestname,
         town, glacres, gldescrip, span, photo, elevationmin,
         elevationmax, owners, addressline1, addressline2, citystatezip,
         watertext, boundariestext, soils,
         file = paste("rda/", property, "-cruise-", year, ".rda", sep = ""))
  } 
}

            