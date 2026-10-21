


print('',quote=F)
print('Concatenate processed ELM output into a single RDS file',quote=F)
#print(caseidprefix,quote=F)
print(caseloop,quote=F)
print(cases,quote=F)


# caseidprefix loop 
#for(cid in 1:length(caseidprefix)) {

#  null_wd_out <- F 
#  if(is.null(wd_out)) {
#    wd_out <- paste0(wd_mod_out,'/',caseidprefix[cid],'_processed')
#    setwd(wd_mod_out)
#    null_wd_out <- T
#  }
  
  print('',quote=F)
  #print('Caseidprefix:',quote=F)
  #print(caseidprefix[cid],quote=F)
  print('Caseloop:',quote=F)
  print(caseloop[cid],quote=F)
  print('output directory:',quote=F)
  print(wd_out,quote=F)
  print('',quote=F)
  
  
  # cases loop
  for(c in 1:length(cases)) {
    
    # file names 
    cases_current <- cases[[c]] 
    uq_member <- paste0('g',formatC(1:uq,width=5,flag=0))
    #new_fname <- paste(caseidprefix[cid],sites,names(cases)[c],sep='_')
    new_fname <- paste(od,sites,names(cases)[c],sep='_')
    #if(!is.null(uq)) new_fname <- paste(new_fname,uq_member,sep='_')
    if(!is.null(uq)) {
      new_fname_concat <- paste0(new_fname,'_UQconcatenated.RDS')
      new_fname        <- paste(new_fname,uq_member, sep='_' )
    }
    new_fname <- paste0(new_fname,'.RDS')    


    # create expanded data array and add first dataset
    print('',quote=F)
    print('Reading RDS file ... ',quote=F)
    print(new_fname[1],quote=F)
    setwd(wd_out)
    l1 <- readRDS(new_fname[1])

    anew <-
      lapply(l1$data_arrays, 
             function(a) {
               if('time'%in%names(dimnames(a))) {
                 adim <- dim(a)
                 a1   <- array(dim=c(adim,uq=uq), dimnames=c(dimnames(a), list(uq=uq_member)) )
                 amss <- as.matrix(expand.grid(lapply(adim, function(i) 1:i)))
                 a1[cbind(amss,1)] <- a
               } else {
                 a1 <- a
               }
               a1
             })


    # add data to expanded data array 
    #for(u in 2:uq) {
    for(u in nuq[-1]) {
      print(new_fname[u],quote=F)
      setwd(wd_out)
      l1 <- readRDS(new_fname[u])
      
      for(an in 1:length(l1$data_arrays)) {
        if('time'%in%names(dimnames(l1$data_arrays[[an]]))) {
          adim <- dim(l1$data_arrays[[an]])
          amss <- as.matrix(expand.grid(lapply(adim, function(i) 1:i)))
          anew[[an]][cbind(amss,u)] <- l1$data_arrays[[an]]
        }
      }
    }
    
    l1$data_arrays <- anew


    # save RDS file
    print('',quote=F)
    print('Writing RDS file(s) ... ',quote=F)
    print(new_fname_concat,quote=F)
    setwd(wd_out)
    saveRDS(l1, new_fname_concat )

    # can add switch to delete ensemble member RDS's

}
#}



### END ###
