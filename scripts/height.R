# Predicts heights from measured attributes
height <- function(spp, dbh, forest_type,  # defaults are regional means
                   cr = 41, ba = 126, bal = 60, lat = 44.6, 
                   lon = -73.7, site_class = 5){
  
  tree_start <- 
    data.frame(spp = 
                 factor(spp, 
                        levels = 
                          levels(ht_model_op$trainingData$spp)), 
               dbh_s = dbh,
               forest_type_s = 
                 factor(forest_type, 
                        levels = 
                          levels(ht_model_op$trainingData$forest_type_s)),
               cr_s = cr,
               ba_s = ba, 
               bal_s = bal,
               lat = lat,
               lon = lon,
               site_class = site_class)
  
  return(predict(ht_model_op, tree_start))
}
