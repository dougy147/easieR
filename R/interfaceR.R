interfaceR <-
  function(){
    options (warn=-1) 
    packages <- c("svDialogs","pkgmaker")
    lapply(packages, require,character.only=T)
    Results <- list()
    write.pkgbib(packages, file='references')
    
    
    choice <- dlgList(c("get the working directory","specify the working directory", "Deleting an object in memory", 
                       "list of objects in memory", "search for a new function", "update packages","Check the installation of packages"), preselect=NULL, multiple = FALSE, title="What is your choice ?")$res
    while(length(choice)==0) return(easieR())
    
    switch(choice, 
           "get the working directory" = Results$"Work directory" <- getwd(),
           "list of objects in memory"= Results$"Objects in memory" <- ls(envir=.GlobalEnv),
           "specify the working directory"={
             repertoire <- dlgDir(title="Please choose the working directory")$res
             if(length(repertoire)==0) repertoire <- getwd()
             setwd(repertoire)
             Results$"new directory" <- paste("The working directory is now", repertoire)
           },
           "Deleting an object in memory"={
             ls(envir=.GlobalEnv)->tout
             Filter( function(x) 'function' %in% class( get(x) ), ls(envir=.GlobalEnv) )->fonctions
             tout[!is.element(tout,fonctions)]->tout
             X<-dlgList(tout, multiple = TRUE, title="Objects to delete")$res
             if(length(X)==0) return(easieR())
             rm(list=X, envir=.GlobalEnv)
             Results <- list()
             Results$"List of objects still in memory of R" <- ls(envir=.GlobalEnv)
           },
           "search for a new function"={
             require(sos)
             writeLines("Pour trouver une nouvelle analyse, il est necessaire de faire votre recherche en anglais. Vous pouvez utiliser plusieurs mots dans la recherche.
Une page html reprenant l'ensemble des packages faisant reference a l'analyse recherchee va s'ouvrir.")
             critere <- dlgInput("What analysis are you looking for?", "Type your search here")$res
             if(length(critere)==0) return(easieR())
             critere <- strsplit(critere, ":")
             critere <- tail(critere[[1]],n=1)
             Results<- findFn(critere)
             return(Results)
           },
           "update packages"= {update.packages(ask=FALSE)},
           "Check the installation of packages"=vef.pack()->Results$"Package verification")
    bibtex::read.bib('references.bib')->Results$"References of the packages used"
    file.remove('references.bib')
    
    return(Results)
  }
