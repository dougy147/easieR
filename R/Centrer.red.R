Centrer.red <-
  function(x, data=NULL, info=TRUE){options (warn=-1) 
    packages<-c("svDialogs")
    #faire l analyse par groupe # regler le probleme des noms
    list()->Resultats
    X<-"other data" 
    while(any(X=="other data")){nom <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    if(info==TRUE) {print(("please choose the database"))}
    nom<-dlgList(c(nom,"other data") , multiple = FALSE, title="Choice of dataframe")$res
    if(length(nom)==0) return(preprocess())
    data<-get(nom)
    if(info==TRUE) {print(("veuillez choisir la ou les variables "))}
    X<-dlgList(names(data), multiple = TRUE, title="Variable (s)")$res
    if(length(X)==0) X<-donnees()
    if(any(sapply(data[,X], class) %in% c("integer", "numeric")==FALSE)) {print("at least one variable is not numeric")
      X<-"other data"
      str(data)}
    }
    
    
    if(info==TRUE) {writeLines(
      "Centrer permet d'avoir une moyenne a zero en maintenant l'ecart-type. Centrer reduire correspond a la formule du z. 
      La moyenne est de 0 et l'ecart-type vaut 1. La probabilite inferieure correspond a la probabilite d'avoir un z inferieur ou egal au z.
      La probabilite superieure correspond a la probabilite d'avoir un z superieur ou egal au z")}
    dlgList(c("center", "center reduce", "lower probability", "higher probability"), preselect="center reduce", multiple = TRUE, title="What do you want to do ?")$res->choix
    if(length(choix)==0) return(preprocess())
    
    for(i in 1:length(choix)){
      if(choix[i]=="center") {S<-FALSE 
      nn<-"center"}else {S<-TRUE
      nn<-"center.reduced"}
      scale(data[,X], scale=S)->centree
      matrix(centree, ncol=length(X))->centree
      if(choix[i]=="higher probability"|choix[i]=="lower probability"){
        if(choix[i]=="higher probability"){
          nn<-"p.sup"
          lower<-FALSE
        }else {
          nn<-"p.inf"
          lower<-TRUE
        }
        round(pnorm(centree, lower.tail = lower),4)->centree
      }
      data.frame(data, centree)->data
      names(data)[(length(data)+1-length(X)):length(data)]<-paste(X, nn, sep=".")  
    }
    
    assign(nom, data, envir=.GlobalEnv)
    View(data)
    Resultats<-paste("The operation was performed correctly")
    return(Resultats)
    }
