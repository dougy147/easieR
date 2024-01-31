VI.multiples <-
  function(data){ require("pych") 
    Resultats<-list()
    nvar<-length(data)
    try(psych::outlier(data, bad=T, na.rm=T,plot=T),silent=T)->essai
    if(class(essai)=="try-error"){
      msgBox("Votre matrice est singuliere, ce qui pose souci. Nous tentons de  de resoudre le souci. Si possible, la distance de Mahalanobis sera alors calculee sur le maximum d'information tout en evitant la singularite.")
      data->data2
      rankifremoved <- sapply(1:ncol(data2), function (x) qr(data2[,-x])$rank)
      which(rankifremoved == max(rankifremoved))->rangs
      if(length(rangs)==length(data2)){ 
        sample(rangs,1)->rang2
        data2[,-rang2]->data2
      } else {
        while(length(rangs)!=length(data2)){
          sample(rangs,1)->rang2
          data2[,-rang2]->data2
          rankifremoved <- sapply(1:ncol(data2), function (x) qr(data2[,-x])$rank)
          which(rankifremoved == max(rankifremoved))->rangs
        }
      }
      try(psych::outlier(data2), silent=T)->essai
      if(class(essai)=="try-error") {
        corr.test(data2)$r->matrice
        if(any(abs(matrice)==1)) {
          msgBox(INFO_perfectly_correlated_variables_in_matrix_trying_to_solve)
          which(abs(matrice)==1, arr.ind=TRUE)->un
          un<-un[-which(un[,1]==un[,2]),]
          data2[,-un[,2]]->data2
          try(psych::outlier(data2), silent=T)->essai
          if(class(essai)=="try-error") {
            writeLines(INFO_cannot_compute_mahalanobis)
            0->data$D.Mahalanobis  }
        }else{essai-> data$D.Mahalanobis}
      } else{ essai-> data$D.Mahalanobis
      }
    }else{
      essai-> data$D.Mahalanobis  
    }
    
    qchisq(p=0.001, df=nvar, ncp = 0, lower.tail = FALSE, log.p = FALSE)->seuil
    data[which(data$D.Mahalanobis>seuil),]->outliers
    length(outliers[,1])/length(data[,1])*100->pourcent
    
    msgBox(paste(round(pourcent,2), INFO_percentage_outliers))
    
    
    if(pourcent!=0){
      writeLines("Supprimer l'ensemble des outliers supprime l'ensemble des valeurs au-dela p(chi.deux)< 0.001.   
                 Supprimer une observation a la fois permet de faire une analyse detaillee de chaque observation  
                 consideree comme influente en partant de la valeur la plus extreme. La procedure s'arrete  
                 quand plus aucune observation n'est consideree comme influente")  
      
      suppr<- dlgList(c(TXT_suppress_all_outliers, TXT_suppress_outliers_manually), 
                      preselect=c(TXT_suppress_all_outliers), multiple = FALSE, title=ASK_how_to_remove)$res
      if(length(suppr)==0) return(NULL)
      if(suppr==TXT_suppress_all_outliers) {data[which(data$D.Mahalanobis<seuil),]->data 
        outliers->Resultats$TXT_labeled_outliers}else{
          suppression<-"yes"
          outliers<-data.frame()
          while(suppression=="yes"){
            print(data[which.max(data$D.Mahalanobis),])
            cat ("Appuyez [entree] pour continuer")
            line <- readline()
            dlgMessage(ASK_suppress_this_obs, "yesno")$res->suppression
            if(suppression=="yes") {rbind(outliers, data[which.max(data$D.Mahalanobis),])->outliers
              data[-which.max(data$D.Mahalanobis),]->data
              
            }
          }
          Resultats$TXT_labeled_outliers<-outliers
        }
    }
    Resultats$data<-data
    return(Resultats)
  }
