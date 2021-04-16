exporterD <-
  function(data=NULL, nom=NULL){options (warn=-1)   
    packages<-c("svDialogs")
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)}
    list()->Resultats
    data <- dlgList(Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv)), multiple = FALSE, 
                    title="What data do you want to export?")$res 
    if(length(data)==0) return(donnees())
    data<-get(data)
    nom <- dlgInput("What name do you want to give to the file?", "Nouveau.fichier")$res
    if(length(nom)==0) nom<-"Nouveau.fichier"
    strsplit(nom, ":")->nom
    tail(nom[[1]],n=1)->nom
    write.csv(data, file=paste(nom, ".csv"))
    paste("the file is saved in", getwd())->Resultats
    return(Resultats)
  }
