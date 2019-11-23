price_coeffs <- read_csv(here("data", "price-coefficients.csv"), 
                         col_names = TRUE) %>%
  mutate(spp_grp = as.factor(spp_grp))

prices <- read_csv(here("data", "prices.csv"), 
                   col_names = TRUE) %>%
  mutate(mill_grade2 = as.numeric(mill_grade2),
         pulp_roadside = as.numeric(pulp_roadside))


# Returns unit price for each log (roadside)
# from price data based on spp & grade & dib (shape matters; sigmoidal or quadratic)
# proably a function that references a table of coefficients for each spp.

# BETTER TO WRITE IT VECTORIZED!!!!!! 
get_roadside <- Vectorize(function(species, dib, pot_grade){
  
  if(pot_grade %in% 1:3){
    species_grp <- filter(prices, spp == species)$spp_grp
    a <- filter(price_coeffs, spp_grp == species_grp, grade == pot_grade, coef == "a")$value
    b <- filter(price_coeffs, spp_grp == species_grp, grade == pot_grade, coef == "b")$value
    c <- filter(price_coeffs, spp_grp == species_grp, grade == pot_grade, coef == "c")$value
    d <- filter(price_coeffs, spp_grp == species_grp, grade == pot_grade, coef == "d")$value
    
    # mill price/mbf for #2 log of this spp (base price)
    base <- filter(prices, spp == species)$mill_grade2 
    
    # multiplier to get current price based on base price & dib
    price_factor <- (a/(1+2.718281828459^(-(b/a)*(dib-c))))+d 
    
    # final price/bf
    return((base*price_factor-trucking)/1000)
  }
  
  else return(filter(prices, spp == species)$pulp_roadside/1000)
})
