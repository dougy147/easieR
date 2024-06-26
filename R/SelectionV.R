SelectionV <-
  function(data=NULL,info=TRUE){options (warn=-1)
    packages<-c('svDialogs')
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages)
      require(packages)}
    list()->Resultats
    choix.data()->data
    if(length(data)==0) return(preprocess())
    if(info==TRUE) print(.dico[["ask_variables"]])
    X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), .dico[["txt_other_data"]]), multiple = TRUE,
               title=.dico[["txt_variable"]])$res
    if(length(X)==0) return(preprocess())
    if( X== .dico[["txt_other_data"]]) return(SelectionV())
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% X)[,2]->X
    data[,X]->data
    fichier<- dlgInput(.dico[["ask_filename"]], .dico[["txt_selection"]])$res
    if(length(fichier)==0) fichier<-.dico[["txt_selection"]]
    strsplit(fichier, ":")->fichier
    tail(fichier[[1]],n=1)->fichier
    assign(x=fichier, value=data, envir=.GlobalEnv)
    View(data, .dico[["txt_selected_data"]])
    Resultats<-paste(.dico[["desc_variables_are_in"]], fichier)
    return(Resultats)
  }
