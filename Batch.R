# Batch Load files
for (i in 18:19) {
  if (i<10) {
    Item_ = paste0("01_Load-Clean-Data/Load-NEDS0",i,".R")
  }
  if (i>=10) {
    Item_ = paste0("01_Load-Clean-Data/Load-NEDS",i,".R")
  }
  source(Item_)
  print(paste0(Item_," completed."))
  rm(list=ls())
  gc()
}


# Batch Analysis files
for (i in 8:19) {
  if (i<10) {
    Item_ = paste0("01_Load-Clean-Data/Analysis-NEDS0",i,".R")
  }
  if (i>=10) {
    Item_ = paste0("01_Load-Clean-Data/Analysis-NEDS",i,".R")
  }
  source(Item_)
  rm(list=ls())
  gc()
}