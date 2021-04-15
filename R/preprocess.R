preprocess <-
  function(){
    choice<-dlgList(c("Ranks", "Imputation of missing values",
                     "Select observations","Select variables","CBetweenr / center reduce","Sort",
                     "Mathematical operations on variables","Pivot table",
                    "Wide format to long format"), multiple = F, preselect="rangs", title="What do you want to do ?")$res
    if(length(choice)==0) return(easieR())
    switch(choice, "Ranks"= ez.rank()->Results,
           "Imputation of missing values"=ez.imp()->Results,
           "Select observations"=selectionO()->Results,
           "Select variables"=SelectionV()->Results,
           "CBetweenr / center reduce"= CBetweenr.red()->Results,"Sort"= trier()->Results,
           "Mathematical operations on variables"= maths()->Results,
           "Wide format to long format"=ez.reshape()->Results,
           "Pivot table"={ 
             try(library("rpivotTable"), silent=T)->test2
             if(class(test2)== "try-error") return(ez.install())
             return( rpivotTable(choice.data(nom=F)))
           }
    )
    return(Results)
  }
