# Check All Scripts
cpath=getwd()
scripts=list.files(paste(cpath,"/Scripts", sep = ""), full.names = TRUE, recursive = TRUE)
for (x in scripts) {
  print(x)
  source(x)
} 