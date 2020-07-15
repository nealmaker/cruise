# Calculates log stumpage based on roadside log price and log cord volume
# built by making up some reasonable stumpage numbers and using cubic regression

stumpage <- Vectorize(function(roadside, volume){
  rdsd <- roadside/volume
  stump <- (rdsd-40)*.5
  
  if(stump > 0) {
    return(stump*volume)
  } else {
    return(0)
  }
}) 
