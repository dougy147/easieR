selectionO <-
  function(data=NULL, info=TRUE){options (warn=-1)
    packages<-c("svDialogs")
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)}
    list()->Resultats
    choix.data()->data
    if(length(data)==0) {return(preprocess())}
    if(info==TRUE) writeLines("Il est possible d'appliquer plusieurs criteres de selection simultanement, impliquant ou non plusieurs variables. 
                              Veuillez preciser le nombre de variables sur lesquelles vous desirez appliquer un ou plusieurs criteres de selection. 
                              Veuillez choisir les variables sur lesquelles vous deirez appliquer une selection") 
    X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees"), multiple = TRUE, 
               title="Variable")$res
    if(length(X)==0 ) return(preprocess())
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% X)[,2]->X
    
    for(i in 1:length(X)) {  
      if(class(data[,X[i]])=="factor"){
        if(info==TRUE) {writeLines("Please select the terms you want to keep.")
          writeLines(paste("What modalities do you want to select for the variable", names(data[,X])[i],"?" ))}
        Y<-dlgList(levels(data[,X[i]]), multiple = TRUE, 
                   title=paste("What modalities do you want to select for the variable", names(data[,X])[i],"?" ))$res
        if(length(Y)==0) return(selectionO())
        data[data[,X[i]]%in% Y,]->data
        factor(data[,X[i]])->data[,X [i]]}else{
          if(info==TRUE) {print("Veuillez specifier les criteres des observations que vous desirez conserver/garder.")
            writeLines(paste("What criteria do you want to use for the variable", names(data[,X])[i], "?"))}
          dlgList(c("superieur a","superieur ou egal a", "inferieur a", "inferieur ou egal a", "egal a", "est different de", "entre", 
                    "beyond (with a lower and upper limit"), 
                  preselect=NULL, multiple = FALSE, title=paste("What criteria do you want to use for the variable", names(data[,X])[i], "?"))$res->choix
          if(length(choix)==0) return(selectionO())
          if(choix=="superieur a"|choix=="inferieur a"|choix=="egal a"|choix=="superieur ou egal a"|
             choix=="inferieur ou egal a"|choix=="est different de"){
            if(info==TRUE) writeLines("Please specify the value on which the observations should be selected.")
            seuil<- dlgInput("Precisez la valeur?", 0)$res
            if(length(seuil)==0) return(selectionO()) else {
              strsplit(seuil, ":")->seuil
              tail(seuil[[1]],n=1)->seuil
              as.numeric(seuil)->seuil}} else{seuil.inf<- dlgInput("Limite inferieure?", 0)$res
              while(length(seuil.inf)==0) {writeLines("you must specify the lower limit")
                dlgMessage("You did not specify the lower limit. Do you want to leave the selection?", "yesno")$res->quitte
                if(quitte=="yes") return(selectionO())
                seuil.inf<- dlgInput("Limite inferieure?", 0)$res}
              strsplit(seuil.inf, ":")->seuil.inf
              tail(seuil.inf[[1]],n=1)->seuil.inf
              as.numeric(seuil.inf)->seuil.inf
              seuil.sup<- dlgInput("Limite superieure?", 0)$res
              while(length(seuil.sup)==0) {writeLines("you must specify the upper limit")
                dlgMessage("You did not specify the upper limit. Do you want to leave the selection?", "yesno")$res->quitte
                if(quitte=="yes") return(selectionO())
                seuil.sup<- dlgInput("Limite superieure?", 0)$res}
              strsplit(seuil.sup, ":")->seuil.sup
              tail(seuil.sup[[1]],n=1)->seuil.sup
              as.numeric(seuil.sup)->seuil.sup}
          if(choix=="superieur a"){data[data[,X[i]]>seuil,]->data}
          if(choix=="inferieur a"){data[data[,X[i]]<seuil,]->data}
          if(choix=="egal a"){data[data[,X[i]]==seuil,]->data}
          if(choix=="est different de"){data[data[,X[i]]!=seuil,]->data}
          if(choix=="superieur ou egal a"){data[data[,X[i]]>=seuil,]->data}
          if(choix=="inferieur ou egal a"){data[data[,X[i]]<=seuil,]->data}
          if(choix=="entre"){data[data[,X[i]]>=seuil.inf & data[,X[i]]<=seuil.sup,]->data}
          if(choix=="beyond (with a lower and upper limit"){data[data[,X[i]]<seuil.inf & data[,X[i]]>seuil.sup,]->data}
        }
    }
    
    fichier<- dlgInput("What name do you want to give to the file?", "selection")$res
    if(length(fichier)==0) return(selectionO())
    strsplit(fichier, ":")->fichier
    tail(fichier[[1]],n=1)->fichier
    assign(x=fichier, value=data, envir=.GlobalEnv)
    View(data, "donnees que vous venez de selectionner")
    Resultats<-paste("the observations you have selected are in", fichier)
    return(Resultats)
  }
