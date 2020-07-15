price_coeffs <- read_csv("data/price-coefficients.csv", 
                         col_names = TRUE) %>%
  mutate(spp_grp = as.factor(spp_grp))


# Returns unit price for each log (roadside per cord)
# from price data based on spp & grade & dib 

# BETTER TO WRITE IT VECTORIZED!!!!!! 
get_roadside <- Vectorize(function(species, dib, pot_grade){
  
  pulp_rdsd <- filter(prices, spp == species)$pulp_roadside/2
  
  if(pot_grade %in% 0:3){
    species_grp <- filter(prices, spp == species)$spp_grp
    
    a <- filter(price_coeffs, spp_grp == species_grp, 
                grade == pot_grade, coef == "a")$value
    b <- filter(price_coeffs, spp_grp == species_grp, 
                grade == pot_grade, coef == "b")$value
    c <- filter(price_coeffs, spp_grp == species_grp, 
                grade == pot_grade, coef == "c")$value
    d <- filter(price_coeffs, spp_grp == species_grp,
                grade == pot_grade, coef == "d")$value
    switch_bottom <- 
      filter(price_coeffs, spp_grp == species_grp, 
             grade == pot_grade, coef == "switch_bottom")$value
    switch_top <- 
      filter(price_coeffs, spp_grp == species_grp, 
             grade == pot_grade, coef == "switch_top")$value
    
    # mill price/mbf for #2 log of this spp (base price)
    base <- (filter(prices, spp == species)$mill_grade2)/2
    
    if(dib <= switch_bottom) {
      return(pulp_rdsd)
      
    } else if(dib <= switch_top) {
      top_rdsd <- base*((a/(1+2.718281828459^(-(b/a)*(switch_top-c))))+d) - 
        (trucking/2)
      
      if(top_rdsd > pulp_rdsd){
        return(((dib - switch_bottom)/(switch_top - switch_bottom))*
                 (top_rdsd - pulp_rdsd) + pulp_rdsd)
      } else {
        return(pulp_rdsd)
      }
      
    } else {
      rdsd <- base*((a/(1+2.718281828459^(-(b/a)*(dib-c))))+d) - 
        (trucking/2)
      
      if(rdsd > pulp_rdsd) {
        return(rdsd)
        
      } else {
        return(pulp_rdsd)
        
      }
    }
  } else {
    return(pulp_rdsd)
    
  }
})
