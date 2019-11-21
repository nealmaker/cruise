# calculates dib for log section based on Westfall & Scott 2010
dib <- function(spp, dbh, ht, section, theta1, theta2, alpha1,
                alpha2, gamma1, gamma2, psi, lambda, beta1, beta2){

  # convert to metric
  dbh <- dbh*2.54
  ht <- ht/3.2808
  sct_ht <- (1 + 8.5*as.numeric(section))/3.2808

  # w & s equations
  s1 <- theta1/(1 + ((sct_ht/ht)/theta2)^lambda)
  s2 <- (((sct_ht/ht)/beta1)^(beta2*(dbh/ht))) / 
    (1 + ((sct_ht/ht)/beta1)^(beta2*(dbh/ht)))
  dob <- sqrt(dbh^2 * ((1.37/ht)/gamma1)^psi * 
                ((1 - (sct_ht/ht))/(1 - gamma1))^(alpha1 + s1) *
                ((1 - (sct_ht/ht))/(1 - gamma2))^(alpha2*s2))

  # convert back and subtract bark
  return(dob/2.54 - 1) # 1" is naive est. of 2*bark thickness 
}
