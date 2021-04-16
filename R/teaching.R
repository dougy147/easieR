teaching <-
  function(){
    tcl<-function(){
      clt.examp(1)
      msgBox("etes-vous pret?")
      for(i in 1:50){
        clt.examp(i*2)
        Sys.sleep(1)
      }
    }
    
    c("psych", "svDialogs", "TeachingDemos")->packages
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    
    choix <- dlgList(c("Understanding a confidence interval", "Understanding alpha and potency",
                       "Understanding the correlation",
                       "Understanding the central limit theorem","Understanding a correlation 2",
                       "Understand prevalence, susceptibility and specificity",
                       "Understand prevalence, susceptibility and specificity 2",
                       "Understanding positive predictive power and negative predictive power",
                       "Understanding a Bayesian inference",
                       "Understanding maximum likelihood",
                       "Understanding the effects of heterogeneous variances"), preselect=NULL, multiple = FALSE, title="Que voulez-vous ?")$res
    if(length(choix)==0) return(easieR())
    
    switch(choix, 
           "Understanding a confidence interval"=ci.examp(), # peut etre completer par des arguments
           "Understanding the central limit theorem"=tcl(),
           "Understand prevalence, susceptibility and specificity"= plotFagan2(),
           "Understanding a Bayesian inference"=plotFagan(),
           "Understanding maximum likelihood"=mle.demo() , #des arguments peuvent etre utilises
           "Understanding alpha and potency"=run.power.examp(hscale=1.5, vscale=1.5, wait=FALSE), 
           "Understanding the correlation" = put.points.demo(),
           "Understanding the effects of heterogeneous variances"={
             writeLines("Avec deux moyennes egales, ou pratiquement egales, le taux d'erreurs doit etre de 5%.
                        Modifiez progressivement l'ecart entre les ecart-types et voyez comment le taux d'erreur alpha va etre modifie")
             run.Pvalue.norm.sim()
           },
           "Understand prevalence, susceptibility and specificity 2"= roc.demo(),
           "Understanding a correlation 2"=run.cor2.examp(),
           "Understanding positive predictive power and negative predictive power"= {
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
