choice.reg <-
  function(html=T){
    try(library(svDialogs), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    
    dlgList(c("Regressions", 
              "Mediation effects", 
              "Logistics regressions"), preselect="Regressions", multiple = FALSE, title="What kind of regression?")$res->choice
    if(length(choice)==0) return(analyse())
    if(choice=="Regressions") regressions(html=html)->Results
    if(choice=="Mediation effects") ez.mediation(html=html)->Results
    if(choice=="Logistics regressions") regressions.log(html=html)->Results
    return(Results)
    
  }

