for (i in 12:18) {
  if (i<10) {
    Item_ = paste0("Analysis-NEDS0",i,".R")
  }
  if (i>=10) {
    Item_ = paste0("Analysis-NEDS",i,".R")
  }
  source(Item_)
  rm(list=ls())
  gc()
}