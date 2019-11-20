# Calculates overtopping basal area (BAL) assuming all input trees are 
# in same plot and ba is adjusted based on tpa:

pbal <- function(dbh, ba){
  sapply(dbh, function(x){
    index <- dbh > x
    return(sum(ba[index]))
  })
}
