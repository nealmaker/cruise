library("here")
library("rChoiceDialogs")

options <- c("process cruise data from an excel sheet", 
             "generate a report from already processed data",
             "load processed data to play with it in R")

choice <- rselect.list(options, title = "What would you like to do?", graphics = T)

if(choice == "process cruise data from an excel sheet"){
  options <- c("VT UVA",
               "VT unenrolled",
               "NY 480a",
               "NY unenrolled")
  
  choice <- rselect.list(options, title = "Property type", graphics = T)
  
  if(choice == "NY 480a") {
    source(here("scripts", "wrangle-trees-480a.R"))
  } else {
    source(here("scripts", "wrangle-trees.R")) 
  }

}else if(choice == "generate a report from already processed data"){
  library("rmarkdown")
  reports <- c("VT FMP", "NY FMP", "Stand & Stock")
  report <- rselect.list(reports,
                         title = "What type of report?",
                         graphics = T)
  
  cruise <- choose.files(caption = "Select .rda file with cruise data:",
                         multi = F)
  
  if(report == "VT FMP"){
    render(here("reports", "templates", "fmp-vt", "fmp-vt.Rmd"), 
           params = list(file = cruise))
  } 
  
  else if(report == "NY FMP"){
    render(here("reports", "templates", "fmp-ny", "fmp-ny.Rmd"), 
           params = list(file = cruise))
  } 
  
  else if(report == "Stand & Stock"){
    render(here("reports", "templates", "stand-and-stock", "stand-and-stock.Rmd"), 
           params = list(file = cruise))
  } 
  
  else print("Error: invalid report type selected")
  
}else{
  cruise <- rchoose.files(caption = "Please select .rda file with cruise data:")
  load(cruise)
}
