rm(list=ls(all=TRUE))

for (i in 1:5){
  load(file=paste0("/home/cheunglc/lyg/nhis_imputed_1997_09_even_", i,".RData"))
  for (j in 1998:2008) {
    if ((j%%2)==0){
      eval(parse(text=paste0("y <- subset(nhis,year==",j,")")))
      dirname <- paste0("/home/cheunglc/lyg/nhis1997_09_even", i, j)
      com <- paste("mkdir", dirname)
      system(com)
      save(y,file=paste0(dirname,"/y.RData"))
      
      #create R file
      write(paste0("load('",dirname,"/y.RData')"),paste0(dirname,"/validate_",i,j,".R"))
      com <- "load(file='/home/cheunglc/lyg/mortality.model.V4.RData')"
      cat(com,file=paste0(dirname,"/validate_",i,j,".R"), append=TRUE)      
      file.append(paste0(dirname,"/validate_",i,j,".R"),"/home/cheunglc/lyg/validate.R")
      com <- paste0("save(y,file='",dirname,"/y_mod.RData')")
      cat(com,file=paste0(dirname,"/validate_",i,j,".R"), append=TRUE)      
      
      #create swarm file
      command <- paste0("R --vanilla <",dirname,"/validate_",i,j,".R >",dirname,"/validate_",i,j,".out")
      write(command,paste0(dirname,"/swarm"))
      
      #submit swarm file
      com<-paste0("swarm -g 1 -f ",dirname,"/swarm"," --module R/3.3.2_gcc-4.9.1", " --time 3-00:00:00")
      system(com)      
    }
  }
}

