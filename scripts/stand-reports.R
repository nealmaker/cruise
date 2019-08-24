load("rda/reid-cruise-2019.rda")
library(tidyverse)
library(extrafont)
loadfonts("win", quiet = TRUE)

#distribution of IGGS w/in each stand
plots %>% mutate(stand = as.factor(stand)) %>% 
  ggplot(aes(ba_inv, fill = stand)) + 
  geom_density() + 
  facet_wrap(~stand) + 
  theme(text = element_text(family = "Perpetua"), legend.position = "none") + 
  scale_y_continuous("relative basal area", labels = NULL, breaks = NULL) + 
  scale_x_continuous("BA of investment-grade growing stock (sqft/ac)") + 
  ggtitle("Investment-grade Growing Stock by Stand")

#distribution of basal areas in a specific stand
plots %>% filter(stand == 3) %>%
  ggplot(aes(ba_live)) +
  geom_density(fill = "orange")

#diameter dist of main spp in a specific stand (smooth density)
trees %>% filter(stand == 2, live == 1, dbh >= 4) %>%
  group_by(spp) %>%
  mutate(spp_sum = sum(live)) %>%
  ungroup() %>%
  mutate(spp = reorder(spp, -live, FUN = sum)) %>%
  filter(spp_sum/n()>=.08) %>%
  ggplot(aes(dbh, y = ..count.., fill = spp)) +
  geom_density(alpha = .6, position = "identity") +
  theme(text = element_text(family = "Perpetua"), legend.position = "none") +
  facet_wrap(~spp) +
  scale_y_continuous("relative basal area", labels = NULL, breaks = NULL) +
  ggtitle("Diameter distributions", 
          subtitle = "for common species")

trees %>% filter(stand == 3, live == 1, dbh >= 4) %>%
  group_by(spp) %>%
  mutate(spp_sum = sum(live)) %>%
  ungroup() %>%
  rename(species = spp) %>%
  mutate(species = reorder(species, live, FUN = sum)) %>%
  #filter(spp_sum/n()>=.08) %>%
  ggplot(aes(dbh, y = ..count.., fill = species)) +
  geom_density(alpha = .6, position = "stack") +
  theme(text = element_text(family = "Perpetua")) +
  scale_y_continuous("basal area", labels = NULL, breaks = NULL) +
  scale_x_continuous("dbh (in)") +
  ggtitle("Diameter distribution")

#diameter dist of main spp in a specific stand (histogram)
trees %>% filter(stand == 1, live == 1, dbh >= 4) %>%
  group_by(spp) %>%
  mutate(spp_sum = sum(live)) %>%
  ungroup() %>%
  mutate(spp = reorder(spp, -live, FUN = sum)) %>%
  filter(spp_sum/n()>=.08) %>%
  ggplot(aes(dbh, y = ..count..*10/nrow(filter(plots, stand == 3)), fill = spp)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~spp) +
  theme(text = element_text(family = "Perpetua"), legend.position = "none") +
  scale_y_continuous("basal area (sqft/ac)") +
  scale_x_continuous("dbh (in)") +
  ggtitle("Diameter distributions", 
        subtitle = "for common species")

trees %>% filter(stand == 3, live == 1, dbh >= 4) %>%
  group_by(spp) %>%
  mutate(spp_sum = sum(live)) %>%
  ungroup() %>%
  mutate(spp = reorder(spp, -live, FUN = sum)) %>%
  filter(spp_sum/n()>=.08) %>%
  ggplot(aes(dbh, y = ..count..*10/nrow(filter(plots, stand == 3)), fill = spp)) +
  geom_histogram(binwidth = 6, position = "stack") +
  theme(text = element_text(family = "Perpetua")) +
  scale_y_continuous("basal area (sqft/ac)") +
  scale_x_continuous("dbh (in)") +
  ggtitle("Diameter distributions", 
          subtitle = "for common species")

#list of spp by percent stocking in a specific stand
trees %>% filter(stand == 1, live == 1, dbh >= 6) %>%
  group_by(spp) %>%
  mutate(spp_sum = sum(live)) %>%
  ungroup() %>%
  mutate(spp = reorder(spp, -live, FUN = sum)) %>%
  mutate(spp_p = spp_sum/n()) %>%
  #filter(spp_p>=.05) %>%
  group_by(spp) %>%
  summarize(pcnt_ba = 100*round(mean(spp_p), digits = 2))


#Plot data for a specific stand
plots %>% filter(stand == 2) %>%
  select(stand, plot, ba_live, ba_ags, ba_inv)

#Summary of volumes/ac by stand:
plots %>% group_by(stand) %>%
  summarize(veneer_vol = round(mean(veneer_vol)),
            saw_vol = round(mean(saw_vol)),
            tie_vol = round(mean(tie_vol)),
            cord_vol = round(mean(cord_vol), 1)) %>%
  mutate(total_bf_vol = round(veneer_vol+saw_vol+tie_vol)) %>%
  select(stand, veneer_vol, saw_vol, tie_vol, total_bf_vol, cord_vol)

#Summary of volumes/ac propertywide:
plots %>% summarize(veneer_vol = round(mean(veneer_vol)),
                    saw_vol = round(mean(saw_vol)),
                    tie_vol = round(mean(tie_vol)),
                    cord_vol = round(mean(cord_vol), 1)) %>%
  mutate(total_bf_vol = round(veneer_vol+saw_vol+tie_vol)) %>%
  select(veneer_vol, saw_vol, tie_vol, total_bf_vol, cord_vol)

#ags igs ugs table
temp1 <- stands %>% filter(stand == 1)
ttotal <- c(temp1$mean_ba, temp1$qsd, temp1$tpa)
tags <- c(temp1$ba_ags, temp1$qsd_ags, temp1$tpa_ags)
tinv <- c(temp1$ba_inv, temp1$qsd_in, temp1$tpa_inv)
temp0 <- c('Basal area (sqft/ac)', 'QSD (in)', 'Stems/ac')
temp5 <- data.frame(temp0, ttotal, tags, tinv)
temp5

#diam table
n_plots <- sum(plots$stand == 1)

trees %>% filter(stand == 1, live == 1) %>%
  mutate(size_class = factor(case_when(dbh<6 ~ "saplings", 
                                dbh<=10 ~ "poles", 
                                dbh<=16 ~ "small sawtimber",
                                TRUE ~ "large sawtimber"), 
                             levels = c("saplings", 
                                        "poles", 
                                        "small sawtimber", 
                                        "large sawtimber"))) %>%
  select(live, inv, ags, size_class) %>%
  filter(size_class != "saplings") %>%
  group_by(size_class) %>%
  summarize(total = n()*10/n_plots, ags = sum(ags)*10/n_plots, inv = sum(inv)*10/n_plots)



dbhlabs <- paste(dbhbold, '"', sep = "")
dbhlabsloc <- tibble(x = c(493, 443, 372, 293, 216, 164, 127, 
                           101, 82, 62), 
                     y = c(43, 87, 130, 160, 170, 175, 177, 
                           178.4, 178.8), 178.8)

#stocking charts

this_stand <- stands %>%
  filter(stand == 1)

dbhbold <- seq(2, 26, 2)
dbhlight <- seq(1, 25, 2)

dbhlabs <- paste(c(4,6,8,10,12,14,16,20), '"', sep = "")
dbhlabstop <- tibble(y = rep(max(this_stand$ay1, 
                             max((plots %>% 
                                    filter(stand == 6))$ba_live))+5, 
                             times = 8))
dbhlabstop <- dbhlabstop %>% 
  mutate(x = y/(.005454*c(4,6,8,10,12,14,16,20)^2))

dbhlabsright <- tibble(x = rep(max(this_stand$ax2,
                                   max((plots %>%
                                          filter(stand == 6))$tpa_live))+50,
                               times = 8))
dbhlabsright <- dbhlabsright %>%
  mutate(y = x*.005454*c(4,6,8,10,12,14,16,20)^2)

dbhlabsloc <- tibble(x = c(493, 443, 372, 293, 216, 164, 127, 
                           82),
                     y = c(43, 87, 130, 160, 170, 175, 177, 
                           178.8))

plots %>% filter(stand == 6) %>%
  ggplot(aes(tpa_live, ba_live)) +
  geom_abline(slope = .005454*dbhbold^2, col = "dark gray") +
  geom_abline(slope = .005454*dbhlight^2, col = "gray80") +
  annotate("text", x = dbhlabstop$x, 
           y = dbhlabstop$y, 
           label = dbhlabs,
           color = "gray55",
           family = "Perpetua") +
  annotate("text", x = dbhlabsright$x, 
           y = dbhlabsright$y, 
           label = dbhlabs,
           color = "gray55",
           family = "Perpetua") +
  geom_curve(aes(x = this_stand$ax1,
                 y = this_stand$ay1,
                 xend = this_stand$ax2,
                 yend = this_stand$ay2),
             curvature = this_stand$acurv,
             angle = this_stand$aangle) + 
  geom_curve(aes(x = this_stand$bx1,
                 y = this_stand$by1,
                 xend = this_stand$bx2,
                 yend = this_stand$by2),
             curvature = this_stand$bcurv,
             angle = this_stand$bangle) + 
  geom_curve(aes(x = this_stand$cx1,
                 y = this_stand$cy1,
                 xend = this_stand$cx2,
                 yend = this_stand$cy2),
             curvature = this_stand$ccurv,
             angle = this_stand$cangle) +
  geom_text(aes(this_stand$ax2, 
                this_stand$ay2, 
                label = this_stand$alab),
            nudge_x = this_stand$anudge,
            family = "Perpetua") +
  geom_text(aes(this_stand$bx2, 
                this_stand$by2, 
                label = this_stand$blab),
            nudge_x = this_stand$bnudge,
            family = "Perpetua") +
  geom_text(aes(this_stand$cx2, 
                this_stand$cy2, 
                label = this_stand$clab),
            nudge_x = this_stand$cnudge,
            family = "Perpetua") +
  geom_point() +
  geom_point(aes(this_stand$tpa, 
                 this_stand$mean_ba),
             size = 3, 
             shape = 8, 
             stroke = 1.5) +
  theme(text = element_text(family = "Perpetua"), 
        legend.position = "right") +
  scale_x_continuous(name = "trees per acre",
                     limits = c(min(this_stand$cx1, 
                                    min((plots %>% 
                                           filter(stand == 6))$tpa_live)),
                                max(this_stand$ax2,
                                    max((plots %>% 
                                           filter(stand ==6))$tpa_live))+50)) +
  scale_y_continuous(name = "basal area per acre (sq ft)",
                     limits = c(this_stand$cy2, this_stand$ay1+5)) +
  labs(title = "Stocking chart",
       caption = paste("Reproduced from ", this_stand$name, 
                       " stocking guide: ", this_stand$author,
                       ". ", this_stand$source, sep = ""))


young_stand <- trees %>% 
  filter(stand == 2) %>%
  mutate(young = 0) %>%
  select(young, everything()) %>%
  fix()

young_stand <- young_stand %>% select(plot, spp, dbh, logs, cond, tpa)

write.csv(young_stand, file = "bothfeld-young-stand-data.csv", col.names = TRUE)

young_stand %>%
  ggplot(aes(dbh, y = ..count.., fill = spp)) +
  geom_density(alpha = .6, position = "identity") +
  theme(text = element_text(family = "Perpetua"), legend.position = "none") +
  facet_wrap(~spp) +
  scale_y_continuous("relative basal area", labels = NULL, breaks = NULL) +
  ggtitle("Diameter distributions", 
          subtitle = "for common species")

View(young_stand %>% group_by(plot, spp) %>%
  summarize(tpa = sum(tpa), ba = 10*n(), qsd = sqrt((ba/tpa)/.005454)))
