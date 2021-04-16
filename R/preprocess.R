preprocess <-
  function(){
    choix<-dlgList(c("Rangs", "Imputation of missing values",
                     "Select observations","Selectionner des variables","Centrer / centrer reduire","Trier",
                     "Mathematical operations on variables","Tableau croise dynamique",
                    "Wide format to long format"), multiple = F, preselect="rangs", title="What do you want to do ?")$res
    if(length(choix)==0) return(easieR())
    switch(choix, "Rangs"= ez.rank()->Resultats,
           "Imputation of missing values"=ez.imp()->Resultats,
           "Select observations"=selectionO()->Resultats,
           "Selectionner des variables"=SelectionV()->Resultats,
           "Centrer / centrer reduire"= Centrer.red()->Resultats,"Trier"= trier()->Resultats,
           "Mathematical operations on variables"= maths()->Resultats,
           "Wide format to long format"=ez.reshape()->Resultats,
           "Tableau croise dynamique"={ 
             try(library("rpivotTable"), silent=T)->test2
             if(class(test2)== "try-error") return(ez.install())
             return( rpivotTable(choix.data(nom=F)))
           }
    )
    return(Resultats)
  }
