choix.corr <-
  function(html=T){options (warn=-1) 
    c( "svDialogs")->packages
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)} 
    writeLines("l'analyse detaillee permet d'avoir les statistiques descriptives, les tests de normalite, le nuage de points,
               \n des statistiques robustes, l'ensemble des coefficients de correlations. 
               \n la matrice de correlation permet de contrôler l'erreur de 1e espece et est adaptee pour un grand nombre de correlations
               \n la comparaison de correlations permet de comparer 2 correlations dependantes ou independantes
               \n Le choix + autre correlations + permet d'avoir les correlation tetrachoriques et polychoriques")
    dlgList(c(TXT_detailed_corr_analysis, 
              TXT_correlations_matrix, 
              TXT_compare_two_correlations,
              TXT_other_correlations), preselect=NULL, multiple = FALSE, title=ASK_which_analysis)$res->choix
    if(length(choix)==0) return(analyse())
    switch(choix,
           TXT_detailed_corr_analysis=corr.complet(html=html)->Resultats,
           TXT_correlations_matrix= corr.matrice(html=html)->Resultats,
           TXT_compare_two_correlations= comp.corr(html=html)->Resultats,
           TXT_other_correlations= tetrapoly(html=html)->Resultats
    )
    return(Resultats)
  }
