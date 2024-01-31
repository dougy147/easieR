Centrer.red <-
  function(x, data=NULL, info=TRUE){options (warn=-1) 
    packages<-c("svDialogs")
    #faire l analyse par groupe # regler le probleme des noms
    list()->Resultats
    X<-TXT_other_data 
    while(any(X==TXT_other_data)){nom <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    if(info==TRUE) {print(("veuillez choisir la base de donnees"))}
    nom<-dlgList(c(nom,TXT_other_data) , multiple = FALSE, title=TXT_dataframe_choice)$res
    if(length(nom)==0) return(preprocess())
    data<-get(nom)
    if(info==TRUE) {print((ASK_chose_variables))}
    X<-dlgList(names(data), multiple = TRUE, title=TXT_variables)$res
    if(length(X)==0) X<-donnees()
    if(any(sapply(data[,X], class) %in% c("integer", "numeric")==FALSE)) {print(INFO_at_least_one_non_numeric)
      X<-TXT_other_data
      str(data)}
    }
    
    
    if(info==TRUE) {writeLines(
      "Centrer permet d'avoir une moyenne a zero en maintenant l'ecart-type. Centrer reduire correspond a la formule du z. 
      La moyenne est de 0 et l'ecart-type vaut 1. La probabilite inferieure correspond a la probabilite d'avoir un z inferieur ou egal au z.
      La probabilite superieure correspond a la probabilite d'avoir un z superieur ou egal au z")}
    dlgList(c(TXT_center, TXT_center_reduce, TXT_inferior_proba, TXT_superior_proba), preselect=TXT_center_reduce, multiple = TRUE, title=ASK_what_to_do)$res->choix
    if(length(choix)==0) return(preprocess())
    
    for(i in 1:length(choix)){
      if(choix[i]==TXT_center) {S<-FALSE 
      nn<-TXT_center}else {S<-TRUE
      nn<-"centrer.reduite"}
      scale(data[,X], scale=S)->centree
      matrix(centree, ncol=length(X))->centree
      if(choix[i]==TXT_superior_proba|choix[i]==TXT_inferior_proba){
        if(choix[i]==TXT_superior_proba){
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
    Resultats<-paste(INFO_succesful_operation)
    return(Resultats)
    }
