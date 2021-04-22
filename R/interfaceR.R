interfaceR <-
  function(){
    options (warn=-1) 
    packages <- c("svDialogs","pkgmaker")
    lapply(packages, require,character.only=T)
    Resultats <- list()
    write.pkgbib(packages, file='references')
    
    
    choix <- dlgList(c("obtenir le repertoire de travail","specify the working directory", "Deleting an object in memory", 
                       "liste des objets en memoire", "rechercher une nouvelle fonction", "mise a jour des packages","Check the installation of packages"), preselect=NULL, multiple = FALSE, title="Quel est votre choix ?")$res
    while(length(choix)==0) return(easieR())
    
    switch(choix, 
           "obtenir le repertoire de travail" = Resultats$"Repertoire de travail" <- getwd(),
           "liste des objets en memoire"= Resultats$"Objets en memoire" <- ls(envir=.GlobalEnv),
           "specify the working directory"={
             repertoire <- dlgDir(title="Please choose the working directory")$res
             if(length(repertoire)==0) repertoire <- getwd()
             setwd(repertoire)
             Resultats$"nouveau repertoire" <- paste("The working directory is now", repertoire)
           },
           "Deleting an object in memory"={
             ls(envir=.GlobalEnv)->tout
             Filter( function(x) 'function' %in% class( get(x) ), ls(envir=.GlobalEnv) )->fonctions
             tout[!is.element(tout,fonctions)]->tout
             X<-dlgList(tout, multiple = TRUE, title="Objets a supprimer")$res
             if(length(X)==0) return(easieR())
             rm(list=X, envir=.GlobalEnv)
             Resultats <- list()
             Resultats$"List of objects still in memory of R" <- ls(envir=.GlobalEnv)
           },
           "rechercher une nouvelle fonction"={
             require(sos)
             writeLines("Pour trouver une nouvelle analyse, il est necessaire de faire votre recherche en anglais. Vous pouvez utiliser plusieurs mots dans la recherche.
Une page html reprenant l'ensemble des packages faisant reference a l'analyse recherchee va s'ouvrir.")
             critere <- dlgInput("What analysis are you looking for?", "Tapez votre recherche ici")$res
             if(length(critere)==0) return(easieR())
             critere <- strsplit(critere, ":")
             critere <- tail(critere[[1]],n=1)
             Resultats<- findFn(critere)
             return(Resultats)
           },
           "mise a jour des packages"= {update.packages(ask=FALSE)},
           "Check the installation of packages"=vef.pack()->Resultats$"Verification des packages")
    bibtex::read.bib('references.bib')->Resultats$"References of the packages used"
    file.remove('references.bib')
    
    return(Resultats)
  }
