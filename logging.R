load("rda/reid-cruise-2019.rda")
library(tidyverse)

# decide what to cut based on size, maturity and condition -------------

trees <- trees %>%
  mutate(maturity_class = case_when(spp %in% c('black cherry', 'hard maple', 'hickory',
                                               'red oak', 'white oak', 'white pine',
                                               'yellow birch', 'spruce', 'tamarack') ~ 1,
                                    spp %in% c('aspen', 'fir') ~ 3,
                                    spp == 'ash' ~ 4,
                                    TRUE ~ 2),
         mature = case_when(maturity_class == 1 & dbh >= 24 ~ 1,
                            maturity_class == 2 & dbh >= 18 ~ 1,
                            maturity_class == 3 & dbh >= 14 ~ 1,
                            maturity_class == 4 & dbh >= 12 ~ 1,
                            TRUE ~ 0),
         cut = if_else((mature == 1 | cond >= 4) & dbh >= 7, 1, 0))


# get cordwood estimates of trees not represented in logs tibble ------

temp <- trees %>% 
  filter(cut == 1) %>%
  anti_join(logs, by = 'tree') %>%
  left_join(tree_volumes, by = 'dbh') %>%
  mutate(cd_vol_peracre = cds * tpa) %>%
  group_by(stand, plot, spp) %>%
  summarise(bf_vol_peracre = sum(cd_vol_peracre)*500) %>%
  mutate(grade = 5) %>%
  select(stand, plot, spp, grade, bf_vol_peracre)


# make volume tibbles of cut trees ------------------------------------

plot_cut_specs <- logs %>% left_join(trees, by = 'tree') %>%
  filter(cut == 1) %>%
  mutate(plot = plot.x,
         spp = spp.x,
         stand = stand.x) %>%
  group_by(stand, plot, spp, grade) %>%
  summarize(bf_vol_peracre = sum(vol_ac)) %>%
  merge(temp, all.y = TRUE, all.x = TRUE) %>%
  arrange(stand, plot, spp, grade)

plot_cut_summaries <- plot_cut_specs %>%
  group_by(stand, plot, grade) %>%
  summarize(bf_vol_peracre = sum(bf_vol_peracre))

stand_cut_specs <- plot_cut_specs %>% 
  group_by(stand, spp, grade) %>%
  summarize(vol = sum(bf_vol_peracre)) %>%
  left_join(stands %>% select(stand, plots, acres_calc), by = 'stand') %>%
  mutate(bf_vol_peracre = vol/plots,
         bf_vol_total = bf_vol_peracre * acres_calc) %>%
  select(-vol, -plots, -acres_calc)

stand_cut_summaries <- stand_cut_specs %>%
  group_by(stand, grade) %>%
  summarize(mbf_peracre = round(sum(bf_vol_peracre)/1000, 3),
            mbf_total = round(sum(bf_vol_total)/1000)) %>%
  group_by(stand) %>%
  mutate(percent_of_cut = round(100*(mbf_total/sum(mbf_total)))) %>%
  ungroup()

View(stand_cut_summaries)


# residual stand----------------------------------------------

plots_residual <- trees %>% filter(cut == 0, dbh >= 4) %>%
  group_by(plot) %>% 
  summarize(stand = stand[1],
            pct_sft = round(sum(live[sft == 1])/sum(live)*100),
            ba_live = baf*sum(live), 
            ba_ags = baf*sum(ags), 
            ba_inv = baf*sum(inv),
            ba_crop = baf*sum(crop),
            ba_snags = baf*sum(snag),
            tpa_live = round(sum(tpa[cond != 7])),
            tpa_ags = round(sum(tpa[cond %in% c(1:4)])),
            tpa_inv = round(sum(tpa[cond %in% c(1:2)])),
            tpa_crop = round(sum(tpa[cond == 1])),
            tpa_snags = round(sum(tpa[cond ==7]))) %>%
  mutate(qsd_live = round(sqrt((ba_live/tpa_live)/0.005454), 1),
         qsd_ags = ifelse(ba_ags > 0, round(sqrt((ba_ags/tpa_ags)/0.005454), 1), 0),
         qsd_inv = ifelse(ba_inv > 0, round(sqrt((ba_inv/tpa_inv)/0.005454), 1), 0),
         qsd_crop = ifelse(ba_crop > 0, round(sqrt((ba_crop/tpa_crop)/0.005454), 1), 0),
         qsd_snags = ifelse(ba_snags > 0, round(sqrt((ba_snags/tpa_snags)/0.005454), 1), 0)) %>%
  arrange(stand, plot)

View(plots_residual)

stands_residual <- plots_residual %>% 
  group_by(stand) %>%
  summarise(plots = n(),
            mean_ba = round(mean(ba_live), digits = 1),
            min_ba = round(min(ba_live)),
            max_ba = round(max(ba_live)),
            qsd = round(mean(qsd_live), digits = 1),
            tpa = round(mean(tpa_live)),
            ba_ags = round(mean(ba_ags)),
            qsd_ags = round(mean(qsd_ags), digits = 1),
            tpa_ags = round(mean(tpa_ags)),
            ba_inv = round(mean(ba_inv)),
            qsd_in = round(mean(qsd_inv), digits = 1),
            tpa_inv = round(mean(tpa_inv)),
            tpa_crop = round(mean(tpa_crop)))

View(stands_residual)



trees %>% filter(stand == 3, live == 1, cut == 0) %>%
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
