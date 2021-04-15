CBetweenr.red <-
  function(x, data=NULL, info=TRUE){options (warn=-1) 
    packages<-c("svDialogs")
    #faire l analyse par groupe # regler le probleme des noms
    list()->Results
    X<-"other data" 
    while(any(X=="other data")){nom <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    if(info==TRUE) {print(("please choose the database"))}
    nom<-dlgList(c(nom,"other data") , multiple = FALSE, title="Choice of dataframe")$res
    if(length(nom)==0) return(preprocess())
    data<-get(nom)
    if(info==TRUE) {print(("please choose the variable (s) "))}
    X<-dlgList(names(data), multiple = TRUE, title="Variable (s)")$res
    if(length(X)==0) X<-donnees()
    if(any(sapply(data[,X], class) %in% c("integer", "numeric")==FALSE)) {print("at least one variable is not numeric")
      X<-"other data"
      str(data)}
    }
    
    
    if(info==TRUE) {writeLines(
      "CBetweenr permet d'avoir une mediumne a zero en maintenant l'ecart-type. CBetweenr reduire correspond a la formule du z. 
      La mediumne est de 0 et l'ecart-type vaut 1. La lower probability correspond a la probabilite d'avoir un z less than or equal tou z.
      La higher probability correspond a la probabilite d'avoir un z greater than or equal tou z")}
    dlgList(c("cBetweenr", "center reduce", "lower probability", "higher probability"), preselect="center reduce", multiple = TRUE, title="What do you want to do ?")$res->choice
    if(length(choice)==0) return(preprocess())
    
    for(i in 1:length(choice)){
      if(choice[i]=="cBetweenr") {S<-FALSE 
      nn<-"cBetweenr"}else {S<-TRUE
      nn<-"center.reduced"}
      scale(data[,X], scale=S)->cBetweene
      matrix(cBetweene, ncol=length(X))->cBetweene
      if(choice[i]=="higher probability"|choice[i]=="lower probability"){
        if(choice[i]=="higher probability"){
          nn<-"p.sup"
          lower<-FALSE
        }else {
          nn<-"p.inf"
          lower<-TRUE
        }
        round(pnorm(cBetweene, lower.tail = lower),4)->cBetweene
      }
      data.frame(data, cBetweene)->data
      names(data)[(length(data)+1-length(X)):length(data)]<-paste(X, nn, sep=".")  
    }
    
    assign(nom, data, envir=.GlobalEnv)
    View(data)
    Results<-paste("The operation was performed correctly")
    return(Results)
    }
