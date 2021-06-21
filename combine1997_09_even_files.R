for (i in 1:5){
  nhis <- data.frame()
  for (j in 1998:2008) {
    if ((j%%2)==0){
       dirname <- paste0("/home/cheunglc/lyg/nhis1997_09_even", i, j)
       load(file=paste0(dirname,"/y_mod.RData"))
       nhis <- rbind(nhis,y)
    }
  }
  save(nhis,file=paste0("/home/cheunglc/lyg/nhis1997_09_even_mod", i,".RData"))
}

