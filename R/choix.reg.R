choix.reg <-
  function(html=T){
    try(library(svDialogs), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    
    dlgList(c("Regressions", 
              "Effets de mediation", 
              "Logistics regressions"), preselect="Regressions", multiple = FALSE, title="What kind of regression?")$res->choix
    if(length(choix)==0) return(analyse())
    if(choix=="Regressions") regressions(html=html)->Resultats
    if(choix=="Effets de mediation") ez.mediation(html=html)->Resultats
    if(choix=="Logistics regressions") regressions.log(html=html)->Resultats
    return(Resultats)
    
  }

