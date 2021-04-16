choice.corr <-
  function(html=T){options (warn=-1) 
    c( "svDialogs")->packages
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)} 
    writeLines("l'analyse detaillee permet d'avoir les descriptive statistics, les tests de normalite, le nuage de points,
               \n des statisticals robusts, l'ensemble des coefficients de correlations. 
               \n la matrice de correlation permet de contrôler l'error de 1e espece et est adaptee pour un grand nombre de correlations
               \n la comparison of correlations permet de comparer 2 correlations dependantes ou independantes
               \n Le choice + autre correlations + permet d'avoir les correlation tetrachoriques et polychoriques")
    dlgList(c("Analyse detaillee (Bravais Pearson/Spearman.tau) pour une ou peu de correlations", 
              "Correlation matrices", 
              "Comparison of two correlations",
              "Other correlations"), preselect=NULL, multiple = FALSE, title="What analysis do you want?")$res->choice
    if(length(choice)==0) return(analyse())
    switch(choice,
           "Analyse detaillee (Bravais Pearson/Spearman.tau) pour une ou peu de correlations"=corr.complet(html=html)->Results,
           "Correlation matrices"= corr.matrice(html=html)->Results,
           "Comparison of two correlations"= comp.corr(html=html)->Results,
           "Other correlations"= tetrapoly(html=html)->Results
    )
    return(Results)
  }
