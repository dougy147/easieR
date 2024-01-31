preprocess <-
  function(){
    choix<-dlgList(c(TXT_ranks_upper, TXT_imput_missing_values,
                     TXT_select_obs,TXT_select_variables,TXT_center_or_center_reduce,TXT_order,
                     TXT_mathematical_operations_on_variables,TXT_dynamic_crossed_table,
                    TXT_long_or_large_format), multiple = F, preselect=TXT_ranks_lower, title=ASK_what_to_do)$res
    if(length(choix)==0) return(easieR())
    switch(choix, TXT_ranks_upper= ez.rank()->Resultats,
           TXT_imput_missing_values=ez.imp()->Resultats,
           TXT_select_obs=selectionO()->Resultats,
           TXT_select_variables=SelectionV()->Resultats,
           TXT_center_or_center_reduce= Centrer.red()->Resultats,TXT_order= trier()->Resultats,
           TXT_mathematical_operations_on_variables= maths()->Resultats,
           TXT_long_or_large_format=ez.reshape()->Resultats,
           TXT_dynamic_crossed_table={ 
             try(library("rpivotTable"), silent=T)->test2
             if(class(test2)== "try-error") return(ez.install())
             return( rpivotTable(choix.data(nom=F)))
           }
    )
    return(Resultats)
  }
