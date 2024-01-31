choix.reg <-
  function(html=T){
    try(library(svDialogs), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    
    dlgList(c(TXT_regressions, 
              TXT_mediation_effect, 
              TXT_logistic_regressions), preselect=TXT_regressions, multiple = FALSE, title=ASK_which_regression_type)$res->choix
    if(length(choix)==0) return(analyse())
    if(choix==TXT_regressions) regressions(html=html)->Resultats
    if(choix==TXT_mediation_effect) ez.mediation(html=html)->Resultats
    if(choix==TXT_logistic_regressions) regressions.log(html=html)->Resultats
    return(Resultats)
    
  }

