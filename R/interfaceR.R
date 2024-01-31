interfaceR <-
  function(){
    options (warn=-1) 
    packages <- c("svDialogs")
    lapply(packages, require,character.only=T)
    Resultats <- list()

    
    
    choix <- dlgList(c(TXT_get_working_dir,TXT_specify_working_dir, TXT_remove_object_in_memory, 
                       TXT_list_of_objects_in_mem, TXT_search_for_new_function, TXT_packages_update,TXT_verify_packages_install), preselect=NULL, multiple = FALSE, title=ASK_what_is_your_choice)$res
    while(length(choix)==0) return(easieR())
    
    switch(choix, 
           TXT_get_working_dir = Resultats$TXT_working_dir <- getwd(),
           TXT_list_of_objects_in_mem= Resultats$TXT_objects_in_mem <- ls(envir=.GlobalEnv),
           TXT_specify_working_dir={
             repertoire <- dlgDir(title=ASK_chose_the_working_dir)$res
             if(length(repertoire)==0) repertoire <- getwd()
             setwd(repertoire)
             Resultats$TXT_new_dir <- paste(INFO_working_dir_is_now, repertoire)
           },
           TXT_remove_object_in_memory={
             ls(envir=.GlobalEnv)->tout
             Filter( function(x) 'function' %in% class( get(x) ), ls(envir=.GlobalEnv) )->fonctions
             tout[!is.element(tout,fonctions)]->tout
             X<-dlgList(tout, multiple = TRUE, title=TXT_object_to_remove)$res
             if(length(X)==0) return(easieR())
             rm(list=X, envir=.GlobalEnv)
             Resultats <- list()
             Resultats$INFO_list_of_objects_still_in_mem <- ls(envir=.GlobalEnv)
           },
           TXT_search_for_new_function={
             require(sos)
             writeLines("Pour trouver une nouvelle analyse, il est necessaire de faire votre recherche en anglais. Vous pouvez utiliser plusieurs mots dans la recherche.
Une page html reprenant l'ensemble des packages faisant reference a l'analyse recherchee va s'ouvrir.")
             critere <- dlgInput(ASK_which_analysis_you_looking_for, INFO_search_here)$res
             if(length(critere)==0) return(easieR())
             critere <- strsplit(critere, ":")
             critere <- tail(critere[[1]],n=1)
             Resultats<- findFn(critere)
             return(Resultats)
           },
           TXT_packages_update= {update.packages(ask=FALSE)},
           TXT_verify_packages_install=vef.pack()->Resultats$TXT_packages_verification)
             if(choix ==TXT_search_for_new_function) packages<-c(packages, "sos")
    Resultats$ref<- ref1(packages)
    return(Resultats)
  }
