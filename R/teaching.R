teaching <-
  function(){
    tcl<-function(){
      clt.examp(1)
      msgBox(ASK_are_you_ready)
      for(i in 1:50){
        clt.examp(i*2)
        Sys.sleep(1)
      }
    }
    
    c("psych", "svDialogs", "TeachingDemos", "tkrplot")->packages
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    
    choix <- dlgList(c(TXT_understanding_confidance_interval, TXT_understanding_alpha_and_power,
                       TXT_understanding_corr,
                       TXT_understanding_central_limit_theorem,TXT_understanding_corr_2,
                       TXT_understanding_prev_sens_specificity,
                       TXT_understanding_prev_sens_specificity_2,
                       TXT_understanding_negative_positive_predic_power,
                       TXT_understanding_bayesian_inference,
                       TXT_understanding_likelihood,
                       TXT_understanding_heterogenous_variance_effects), preselect=NULL, multiple = FALSE, title=ASK_what_do_you_want)$res
    if(length(choix)==0) return(easieR())
    
    switch(choix, 
           TXT_understanding_confidance_interval=ci.examp(), # peut etre completer par des arguments
           TXT_understanding_central_limit_theorem=tcl(),
           TXT_understanding_prev_sens_specificity= plotFagan2(),
           TXT_understanding_bayesian_inference=plotFagan(),
           TXT_understanding_likelihood=mle.demo() , #des arguments peuvent etre utilises
           TXT_understanding_alpha_and_power=run.power.examp(hscale=1.5, vscale=1.5, wait=FALSE), 
           TXT_understanding_corr = put.points.demo(),
           TXT_understanding_heterogenous_variance_effects={
             writeLines("Avec deux moyennes egales, ou pratiquement egales, le taux d'erreurs doit etre de 5%.
                        Modifiez progressivement l'ecart entre les ecart-types et voyez comment le taux d'erreur alpha va etre modifie")
             run.Pvalue.norm.sim()
           },
           TXT_understanding_prev_sens_specificity_2= roc.demo(),
           TXT_understanding_corr_2=run.cor2.examp(),
           TXT_understanding_negative_positive_predic_power= {
             for(i in seq(1,11,2)) {
               SensSpec.demo(sens=0.95, spec=0.99, prev=0.01, step=i) # on peut modifier sensibilite et specificite
               if( interactive() ) {
                 readline("Press Enter to continue")  
               }
               
               
             }
           }
           
           
    )
    ref1(packages)->Resultats
    return(Resultats)
    
           }
