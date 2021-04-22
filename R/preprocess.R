preprocess <-
  function(){
    choix<-dlgList(c("Ranks", "Imputation of missing values",
                     "Select observations","Select variables","Centrer / centrer reduire","Sort",
                     "Mathematical operations on variables","Pivot table",
                    "Wide format to long format"), multiple = F, preselect="rangs", title="What do you want to do ?")$res
    if(length(choix)==0) return(easieR())
    switch(choix, "Ranks"= ez.rank()->Resultats,
           "Imputation of missing values"=ez.imp()->Resultats,
           "Select observations"=selectionO()->Resultats,
           "Select variables"=SelectionV()->Resultats,
           "Centrer / centrer reduire"= Centrer.red()->Resultats,"Sort"= trier()->Resultats,
           "Mathematical operations on variables"= maths()->Resultats,
           "Wide format to long format"=ez.reshape()->Resultats,
           "Pivot table"={ 
             try(library("rpivotTable"), silent=T)->test2
             if(class(test2)== "try-error") return(ez.install())
             return( rpivotTable(choix.data(nom=F)))
           }
    )
    return(Resultats)
  }
