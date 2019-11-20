# calculates dib for log section based on Westfall & Scott 2010
dib <- function(spp, dbh, ht, section){
  
  # get spp-specific coefficients
  t1 <- taper_coefs[which(taper_coefs$spp == spp), 2][[1]]
  t2 <- taper_coefs[which(taper_coefs$spp == spp), 3][[1]]
  a1 <- taper_coefs[which(taper_coefs$spp == spp), 4][[1]]
  a2 <- taper_coefs[which(taper_coefs$spp == spp), 5][[1]]
  g1 <- taper_coefs[which(taper_coefs$spp == spp), 6][[1]]
  g2 <- taper_coefs[which(taper_coefs$spp == spp), 7][[1]]
  p <- taper_coefs[which(taper_coefs$spp == spp), 8][[1]]
  l <- taper_coefs[which(taper_coefs$spp == spp), 9][[1]]
  b1 <- taper_coefs[which(taper_coefs$spp == spp), 10][[1]]
  b2 <- taper_coefs[which(taper_coefs$spp == spp), 11][[1]]

  # convert to metric
  dbh <- dbh*2.54
  ht <- ht/3.2808
  sct_ht <- (1 + 8.5*as.numeric(section))/3.2808

  # w & s equations
  s1 <- t1/(1 + ((sct_ht/ht)/t2)^l)
  s2 <- (((sct_ht/ht)/b1)^(b2*(dbh/ht))) / (1 + ((sct_ht/ht)/b1)^(b2*(dbh/ht)))
  dob <- sqrt(dbh^2 * ((1.37/ht)/g1)^p * ((1 - (sct_ht/ht))/(1 - g1))^(a1 + s1) *
    ((1 - (sct_ht/ht))/(1 - g2))^(a2*s2))

  return(dob/2.54 - 1)
}
