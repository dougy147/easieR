preprocess <-
  function(){
    choix<-dlgList(c("Rangs", "Imputation of missing values",
                     "Selectionner des observations","Selectionner des variables","Centrer / centrer reduire","Trier",
                     "Mathematical operations on variables","Tableau croise dynamique",
                    "Format large au format long"), multiple = F, preselect="rangs", title="Que voulez-vous faire ?")$res
    if(length(choix)==0) return(easieR())
    switch(choix, "Rangs"= ez.rank()->Resultats,
           "Imputation of missing values"=ez.imp()->Resultats,
           "Selectionner des observations"=selectionO()->Resultats,
           "Selectionner des variables"=SelectionV()->Resultats,
           "Centrer / centrer reduire"= Centrer.red()->Resultats,"Trier"= trier()->Resultats,
           "Mathematical operations on variables"= maths()->Resultats,
           "Format large au format long"=ez.reshape()->Resultats,
           "Tableau croise dynamique"={ 
             try(library("rpivotTable"), silent=T)->test2
             if(class(test2)== "try-error") return(ez.install())
             return( rpivotTable(choix.data(nom=F)))
           }
    )
    return(Resultats)
  }
