factor.an <-
  function(data=NULL, X=NULL, nF=NULL, rotation="none", methode="ml", sat=0.3, outlier=c(TXT_complete_dataset),
           imp=NULL, ord=NULL, sauvegarde=FALSE, scor.fac=FALSE,n.boot=1, hier=F, nfact2=1, choix="afe",info=T, html=T){
    
    # data : dataframe
    # X : character. Vector of variable names
    # nF : number of factors
    # rotation : character. One among c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor",
    # "promax",  "oblimin",  "simplimax","bentlerQ", "geominQ","biquartimin", "cluster")
    # methode : character. One among c("ml", "minres" "minchi", "wls","gls","pa")
    # sat : numeric. Level of loading below which loading is not printed. 
    # outlier : one among TXT_complete_dataset or TXT_without_outliers
    # imp : character. How should missing values be treated ? One among "mean" (use mean), "median" (use median), "amelia", "rm" (remove)
    # ord : character vector. Which variables among X are ordinal ? (or dichotomous)
    # sauvegarde : logical. Should result be saved in rtf ? 
    # n.boot : integer. Number of iterations for bootstrap. 
    # hier : Logical. Should hierarchical factor analysis be done. Possible only if nF>1, methode is not "pa" and rotation is oblique. 
    # nfact2 : number of factors for hierarchical level. Must be inferior to nF/2 
    # choix : character. One among "afe" and "acp". If afc is choosen, open dialog box for confirmatory factor analysis
    # info : Logical. Should information be printed in the console when using dialog boxes. 
    
    
    fa.in<-function(data=NULL, choix=NULL, X=NULL, imp=NULL, ord=NULL, nF=NULL, rotation="none", methode="minres", sat=NULL, 
                    scor.fac=FALSE,n.boot=NULL, info=T, outlier=NULL,hier=NULL, nfact2=1, sauvegarde=F){
      
      Resultats<-list()
      if(is.null(data) | is.null(X))  {dial<-TRUE}else dial<-F
      if(dial || is.null(choix) || length(choix)!=1 ||choix %in% c(TXT_factorial_exploratory_analysis,"afe",
                                                                   "afc","acp",TXT_confirmatory_factorial_analysis,TXT_principal_component_analysis)==FALSE){
        dial<-T  
        if(info) writeLines(ASK_chose_analysis)
        dlgList(c(TXT_factorial_exploratory_analysis, 
                  TXT_confirmatory_factorial_analysis,
                  TXT_principal_component_analysis), preselect=NULL, multiple = FALSE, title=ASK_which_analysis)$res->choix
        if(length(choix)==0) return(NULL)
        if(choix==TXT_confirmatory_factorial_analysis) return(ez.cfa())
        try( windows(record=T), silent=T)->win
        if(class(win)=="try-error") quartz()
        
      }
      
      
      if(dial || class(data)!="data.frame"){
        data<-choix.data(data=data, info=info, nom=T)
        if(length(data)==0) return(NULL) 
        nom<-data[[1]]
        data<-data[[2]]  
      }else{
        deparse(substitute(data))->nom  
      }
      if(choix=="fa" | choix==TXT_factorial_exploratory_analysis) msg3<-ASK_chose_variables_at_least_five else{
        msg3<-ASK_chose_variables_at_least_three
      }
      
      X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title=TXT_variables, out=NULL)
      data<-X$data
      X<-X$X
      if(is.null(X) || length(X)<3) {
        Resultats<-fa.in()
        return(Resultats)}
      
      
      
      if(dial || length(outlier)>1 || outlier %in% c(TXT_complete_dataset, TXT_without_outliers) ==FALSE){
        if(info) writeLines(ASK_analysis_on_complete_data_or_remove_outliers)
        if(info) writeLines(INFO_outliers_identified_on_mahalanobis)
        outlier<- dlgList(c(TXT_complete_dataset, TXT_without_outliers), preselect=TXT_complete_dataset,multiple = FALSE, title=ASK_results_desired)$res
        if(length(outlier)==0) { Resultats<-fa.in()
        return(Resultats)}
      }
      
      if(outlier==TXT_without_outliers){
        inf<-VI.multiples(data,X)
        Resultats$TXT_labeled_outliers<-inf$TXT_labeled_outliers
        data<-inf$data
      }
      
      
      
      if(dial){
        if(info) writeLines(ASK_variables_type_correlations)
        if(length(unique(unlist(data[,X])))<9) {type<-dlgList(c(TXT_dichotomic_ordinal,"continues", "mixte"), preselect=NULL, multiple = FALSE, title=ASK_variables_type)$res}else {
          type<-dlgList(c("continues", "mixte"), preselect=NULL, multiple = FALSE, title=ASK_variables_type)$res 
        }
        
        if(length(type)==0) {Resultats<-fa.in()
        return(Resultats)}
      } else{if(is.null(ord)) type<-"continues" else type<-TXT_dichotomic_ordinal
      }
      
      
      if(type=="continues"){ methode<-c("ml")
      cor<-"cor"
      Matrice<-corr.test(data[,X], method="pearson")$r }else {
        cor<-"poly"
        methode<-c("minres")
        if(type=="mixte") {cor<-"mixed"
        if(info) writeLines(ASK_ordinal_variables) 
        ord<-dlgList(X, multiple = TRUE, title=ASK_ordinal_variables)$res
        if(length(ord)==0) {Resultats<-fa.in()
        return(Resultats)}
        }else ord<-X
        Matrice<-try(tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor',imp=imp, html=F)[[1]],silent=T)
        if(all(class(Matrice)!="matrix")) {
          sortie<-dlgMessage(ASK_correlation_matrix_could_not_be_computed, type="yesno")$res
          if(sortie=="yes") return(NULL) else Matrice<-try(tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp="rm")[[1]],silent=T)
          if(class(Matrix)=="try-error")  {Matrice<-corr.test(data[,X], method="Spearman")$r
          msgBox(INFO_polyc_correlations_failed_rho_used_instead)}
        }
      }    
      
      Matrice1 <- mat.sort(Matrice)
      if(length(X)>30) numbers<-F else numbers<-T
      try(cor.plot(Matrice1, show.legend=FALSE, main=TXT_correlations_matrix_afe, labels=NULL, n.legend=0, MAR=TRUE, numbers=numbers,cex=1), silent=T)
      round(Matrice,3)->Resultats$TXT_correlations_matrix
      round(unlist(cortest.bartlett(data[,X])),4)->bartlett
      names(bartlett)<-c("chi.carre","valeur.p","ddl")
      ### doit etre significatif (attention depend de la taille de l echantillon)
      bartlett->Resultats$TXT_adequation_measurement_of_matrix$TXT_barlett_test
      KMO1<-KMO(Matrice)
      if(any(is.na(KMO1))) {msgBox(INFO_kmo_on_matrix_could_not_be_obtained_trying)
        Matrice<-cor.smooth(Matrice)
        KMO1<-KMO(Matrice)}
      if(any(is.na(KMO1))) {
        msgBox(INFO_kmo_on_matrix_could_not_be_obtained)
        Resultats$TXT_adequation_measurement_of_matrix$TXT_kaiser_meyer_olkin_index<-INFO_kmo_could_not_be_computed_verify_matrix
      } else {
        round(KMO1$MSA,3)->Resultats$TXT_adequation_measurement_of_matrix$TXT_kaiser_meyer_olkin_index ### doit etre superieur a 0.5 sinon la matrice ne convient pas pour analyse factorielle. Dans lÃÂÃÂideal, avoir au moins 0.8. Si des X presentent un KMO<0.5, on peut envisager de les supprimer. 
        round(KMO1$MSAi,3)->Resultats$TXT_adequation_measurement_of_matrix$'Indice de Kaiser-Meyer-Olkin par item'
        round(det(Matrice),5)->Resultats$TXT_adequation_measurement_of_matrix$TXT_correlation_matrix_determinant
        Resultats$TXT_adequation_measurement_of_matrix$TXT_correlation_matrix_determinant_information<-INFO_multicolinearity_risk
      }
      
      
      if(dial){
        print(Resultats$TXT_adequation_measurement_of_matrix)
        print(INFO_kmo_must_strictly_be_more_than_a_half)
        cat ("Appuyez sur [entree] pour continuer")
        line <- readline()  
        dlgMessage(c(ASK_sufficient_matrix_for_afe, ASK_continue), "okcancel")$res->res.kmo
        if(res.kmo=="cancel") {print(INFO_you_exited_afe)
          return(analyse())}
      }
      
      
      if(dial || length(methode)>1 || is.null(methode) || methode%in%c("minres","wls","gls","pa", "ml","minchi")==FALSE){
        if(info) writeLines("Pour les variables ordinales et dichomiques, preferez la methode du minimum des residus - minres -
                            ou des moindres carres ponderes - wls. Pour les variables continues, le maximum de vraisemblance si la normalite est respectee - ml")
        methode<-dlgList(c("minres","wls","gls","pa", "ml","minchi"), preselect= methode, multiple = FALSE, title=ASK_which_algorithm)$res
        if(length(methode)==0) {Resultats<-fa.in()
        return(Resultats)}
        
      }
      
      eigen(Matrice)$values->eigen
      parallel(length(data[,1]), length(X), 100)->P1
      nScree(x =eigen, aparallel=P1$eigen$mevpea)->result
      result->Resultats$TXT_parallel_analysis
      plotnScree(result)
      if(dial | is.null(nF) | !is.numeric(nF)) {
        msgBox(paste(TXT_factors_to_keep_accord_to_parallel_analysis_is,result$Components$nparallel, TXT_factors ))
        cat ("Appuyez sur [entree] pour continuer")
        line <- readline() 
        nF<-NA
        while(!is.numeric(nF)) {
          writeLines(ASK_factors_number) 
          nF <- dlgInput(ASK_factors_number, 2)$res
          if(length(nF)==0) {Resultats<-fa.in()
          return(Resultats)
          }
          strsplit(nF, ":")->nF
          tail(nF[[1]],n=1)->nF
          as.numeric(nF)->nF
          if(any((nF%%1==0)%in% c(FALSE, NA))|| nF<0 || nF>(length(X)/2) ){
            msgBox(INFO_facotrs_must_be_positive_int_inferior_to_variables_num)
            nF<-NA
          }
        }
      }
      
      
      
      if(dial & nF>1 || (length(rotation)>1 | rotation %in% c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor",
                                                              "promax",  "oblimin",  "simplimax","bentlerQ", "geominQ","biquartimin", "cluster")==FALSE)){
        if(choix=="acp" | choix==TXT_principal_component_analysis) rotation<- c("none", "varimax", "quartimax", "promax",  "oblimin",  "simplimax","cluster") else{
          rotation<-c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor", "promax",  "oblimin",  
                      "simplimax","bentlerQ", "geominQ","biquartimin", "cluster")
        }
        writeLines(ASK_chose_rotation)
        rotation<-dlgList(rotation, preselect= "oblimin", multiple = FALSE, title=ASK_which_rotation)$res
        if(length(rotation)==0) {Resultats<-fa.in()
        return(Resultats)}
      }
      if(dial | !is.logical(scor.fac)){
        writeLines(ASK_integrate_factorial_scores_in_data)
        dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title=ASK_factorial_scores)$res->scor.fac
        if(length(scor.fac)==0) {Resultats<-fa.in()
        return(Resultats)}
      }
      
      if(!is.numeric(sat) || sat>1 || sat<0 || is.null(sat)){
        sat<-NULL 
      }
      while(is.null(sat)){
        if(info)  writeLines("Le critere de saturation permet de n'afficher dans le tableau de resultats 
                             que les saturation superieure au seuil fixe")
        sat <- dlgInput(ASK_which_saturation_criterion, 0.3)$res
        
        if(length(sat)==0) {Resultats<-fa.in()
        return(Resultats)  }
        strsplit(sat, ":")->sat
        tail(sat[[1]],n=1)->sat
        as.numeric(sat)->sat
        if(is.na(sat)) {sat<-NULL
        msgBox(INFO_saturation_criterion_must_be_between_zero_and_one) }
      }
      
      
      
      if(choix==TXT_factorial_exploratory_analysis) {  
        if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
          msgBox(INFO_bootstraps_number_must_be_positive) 
          n.boot<-NULL
        }
        while(is.null(n.boot)){
          writeLines(ASK_bootstrap_numbers_1_for_none)
          n.boot<-dlgInput(ASK_bootstraps_number, 1)$res
          if(length(n.boot)==0) {Resultats<-fa.in()
          return(Resultats)}
          strsplit(n.boot, ":")->n.boot
          tail(n.boot[[1]],n=1)->n.boot
          as.numeric(n.boot)->n.boot
          if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
            msgBox(INFO_bootstraps_number_must_be_positive) 
            n.boot<-NULL
          }
        }
        if(dial & nF>1 & methode!="pa" & rotation%in%c("oblimin","simplimax", "promax") || hier==T && nFact2>=nF/2){
          if(info) writeLines(ASK_test_hierarchical_structure)
          dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title=ASK_hierarchical_analysis)$res->hier
          if(length(hier)==0) {Resultats<-fa.in()
          return(Resultats)  
          }
          if(!is.null(hier) && hier==TRUE){
            nfact2<-NA
            while(!is.numeric(nfact2)) {
              nfact2<-NA
              writeLines(ASK_factors_number_for_hierarchical_structure) 
              nfact2 <- dlgInput(ASK_factors_superior_level, 1)$res
              if(length(nfact2)==0) {Resultats<-fa.in()
              return(Resultats)
              }
              strsplit(nfact2, ":")->nfact2
              tail(nfact2[[1]],n=1)->nfact2
              as.numeric(nfact2)->nfact2
              if(any(nfact2%%1==0 %in% c(FALSE, NA))|| nfact2<0 || nfact2>=nF/2 ){
                msgBox(INFO_nb_factors_must_be_positive_integer)
                nfact2<-NA
              }
            }
            
          }
        }
      }
      
      
      
      if(dial | !is.logical(sauvegarde)){
        if(info) writeLines(ASK_save_results_in_external_file)
        dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title=ASK_save)$res->sauvegarde
        if(length(sauvegarde)==0) {Resultats<-fa.in()
        return(Resultats)    
        }
      }
      
      Resultats$choix<-choix
      Resultats$data<-data
      Resultats$nom<-nom
      Resultats$X<-X
      Resultats$Matrice<-Matrice
      Resultats$n.boot<-n.boot
      Resultats$rotation<-rotation
      Resultats$methode<-methode
      Resultats$sat<-sat
      Resultats$nF<-nF
      Resultats$type<-type
      Resultats$sauvegarde<-sauvegarde
      if(is.null(hier)) hier<-FALSE else Resultats$hier<-hier
      Resultats$cor<-cor
      Resultats$scor.fac<-scor.fac
      Resultats$ord<-ord
      Resultats$nfact2<-nfact2
      return(Resultats) 
    }
    
    fa.out<-function(Matrice, data, X, nF, methode, rotation, sat, scor.fac, n.boot, nom, hier=FALSE, cor="cor", nfact2){
      
      if( cor=="cor") { Resultats$TXT_multivariate_normality<-.normalite(data, X)} else cor<-"mixed"
      if(n.boot==1) {
        FA.results<-fa(Matrice,nfactors= nF, n.obs=length(data[,1]),fm=methode, rotate=rotation, n.iter=1) # realise l AFE
      } else {
        FA.results<-try(fa(data[,X], nfactors= nF, fm=method, rotate=rotation, n.iter=n.boot, cor=cor), silent=T)
        if(class(FA.results)=="try-error") { 
          msgBox(INFO_model_could_not_converge)
          FA.results<-try(fa(data[,X], nfactors= nF, fm=methode, rotate=rotation, n.iter=1, cor="cor", SMC=F), silent=T)
          if(class(FA.results)=="try-error"){
            msgBox(ASK_could_not_converge_model_verify_correlation_matrix)
            return(analyse())}
        }
      }
      
      
      Resultats<-list()
      Resultats$analyse<-paste(TXT_factorial_analysis_using_fa_with_method, FA.results$fm)
      if(rotation=="none") Resultats$rotation<-INFO_there_is_no_rotation else Resultats$rotation<-paste(TXT_rotation_is_a_rotation, rotation)
      FA.results<-fa.sort(FA.results,polar=FALSE)
      loadfa<-round(as(FA.results$loadings, "matrix"),3)
      loadfa[which(abs(loadfa)<sat)]<-" "
      data.frame("communaute"=round(FA.results$communality,3),
                 "specifite"=round(FA.results$uniquenesses,3),
                 TXT_complexity=round(FA.results$complexity,2))->communaute
      Resultats$INFO_standardized_saturation_on_correlation_matrix<-data.frame(loadfa, communaute)
      
      var.ex <- round(FA.results$Vaccounted,3)
      if(nF>1){dimnames(var.ex)[[1]]<-c(TXT_saturations_sum_of_squares, TXT_explained_variance_ratio,
                                        TXT_cumulated_explained_variance_ratio, TXT_explaination_ratio, 
                                        TXT_cumulated_explaination_ratio)} else {
                                          dimnames(var.ex)[[1]]<-c(TXT_saturations_sum_of_squares, TXT_explained_variance_ratio)
                                        }
      Resultats$TXT_explained_variance<-var.ex
      
      paste("ML",1:nF)->noms1
      if(nF>1 & rotation=="oblimin"){
        round(FA.results$Phi, 3)->cor.f
        Resultats$TXT_correlations_between_factors<-cor.f}
      paste(TXT_mean_complexity_is, round(mean(FA.results$complexity),3), TXT_this_tests_if, nF, TXT_sufficient_factors )-> Resultats$TXT_mean_complexity
      if(length(X)>5){
        round(matrix(c(FA.results$null.chisq, FA.results$null.dof,FA.results$null.model,
                       FA.results$dof, FA.results$objective, FA.results$RMSEA,
                       FA.results$TLI,FA.results$BIC, FA.results$SABIC,FA.results$rms, FA.results$crms, FA.results$fit.off, 
                       FA.results$chi, FA.results$EPVAL, FA.results$STATISTIC, FA.results$PVAL, FA.results$n.obs), ncol=1),4)->stats
        c(TXT_chi_squared_null_model, TXT_null_model_degrees_of_freedom, TXT_objective_function_of_null_model,
          TXT_model_degrees_of_freedom, TXT_objective_function_of_model, "RMSEA", TXT_lower_bound_rmsea, TXT_upper_bound_rmsea,
          TXT_confidance_threshold, TXT_tucker_lewis_fiability_factor, "BIC", "EBIC", 
          "RMSR", "RMSR corrige", TXT_adequation_outside_diagonal, TXT_chi_squared_empirical, TXT_empirical_chi_square_proba_value,
          TXT_chi_squared_likelihood_max, TXT_max_likelihood_chi_squared_proba_value, INFO_total_observations)->dimnames(stats)[[1]]
        
        TXT_values->dimnames(stats)[[2]]
        stats->Resultats$TXT_adequation_adjustement_indexes
        if(all(FA.results$R2<1)){
          round(rbind((FA.results$R2)^0.5,FA.results$R2,2*FA.results$R2-1),2)->stats
          dimnames(stats)[[1]]<-c(TXT_correlation_between_scores_and_factors, TXT_multiple_r_square_of_factors_scores,
                                  TXT_min_correlation_between_scores_and_factors)
          dimnames(stats)[[2]]<-noms1
          stats->Resultats$TXT_correlation_between_scores_and_factors 
        }
        
        if(n.boot>1) {
          IC<-c()
          for(i in 1:nF){
            cbind(round(FA.results$cis$ci[,i],3), 
                  round(as(FA.results$loadings, "matrix"),3)[,i],
                  round(FA.results$cis$ci[,i+nF],3))->IC2
            dimnames(IC2)[[2]]<-c("lim.inf", dimnames(FA.results$loadings)[[2]][i],"lim.sup")
            cbind(IC, IC2)->IC
          }
          IC->Resultats$TXT_confidence_interval_of_saturations_on_bootstrap
        }
      }
      print(fa.diagram(FA.results))#representation graphique des saturations}
      if(scor.fac){Scores.fac<-c()
      sapply(data[,X], scale)->centrees
      FA.results$weights->matrice2
      t(matrice2)->matrice2
      for(i in 1 : nF){
        apply(centrees%*%matrice2[i,],1,sum)->centrees2
        cbind(Scores.fac,centrees2)->Scores.fac
      }
      
      data<-data.frame(data,Scores.fac)
      names(data)[(length(data)+1-nF):length(data)]<-paste0(TXT_factor, 1:nF)
      assign(nom, data,envir=.GlobalEnv)
      
      }
      
      if(hier) {
        if(cor!="cor") poly<-TRUE else poly<-FALSE
        Resultats$TXT_hierarchical_factorial_analysis$Omega<-psych::omega(data[,X], nfactors=nF, n.iter=n.boot,fm=methode, poly=poly, flip=T, digits=3, sl=T, plot=T, n.obs=length(data[,1]), rotate=rotation)
        multi<-fa.multi(Matrice, nfactors=nF, nfact2=nfact2, n.iter=1,fm=methode, n.obs=length(data[,1]), rotate=rotation)
        multi$f2->FA.results
        
        FA.results<-fa.sort(FA.results,polar=FALSE)
        loadfa<-round(as(FA.results$loadings, "matrix"),3)
        loadfa[which(abs(loadfa)<sat)]<-" "
        data.frame("communaute"=round(FA.results$communality,3),
                   "specifite"=round(FA.results$uniquenesses,3),
                   TXT_complexity=round(FA.results$complexity,2))->communaute
        Resultats$TXT_hierarchical_factorial_analysis$INFO_standardized_saturation_on_correlation_matrix<-data.frame(loadfa, communaute)
        
        var.ex <- round(FA.results$Vaccounted,3)
        if(nfact2>1){dimnames(var.ex)[[1]]<-c(TXT_saturations_sum_of_squares, TXT_explained_variance_ratio,
                                              TXT_cumulated_explained_variance_ratio, TXT_explaination_ratio, 
                                              TXT_cumulated_explaination_ratio)} else {
                                                dimnames(var.ex)[[1]]<-c(TXT_saturations_sum_of_squares, TXT_explained_variance_ratio)
                                              }
        Resultats$TXT_hierarchical_factorial_analysis$TXT_explained_variance<-var.ex
        
        paste("ML",1:nfact2)->noms1
        
        paste(TXT_mean_complexity_is, round(mean(FA.results$complexity),3), TXT_this_tests_if, nF, TXT_sufficient_factors )-> Resultats$TXT_mean_complexity
        
        round(matrix(c( FA.results$null.dof,FA.results$null.model,
                        FA.results$dof, FA.results$objective, 
                        FA.results$rms, FA.results$fit.off), ncol=1),4)->stats
        c( TXT_null_model_degrees_of_freedom, TXT_objective_function_of_null_model,
           TXT_model_degrees_of_freedom, TXT_objective_function_of_model,    "RMSR", 
           TXT_adequation_outside_diagonal)->dimnames(stats)[[1]]
        
        TXT_values->dimnames(stats)[[2]]
        stats->Resultats$TXT_hierarchical_factorial_analysis$TXT_adequation_adjustement_indexes
        if(all(FA.results$R2<1)){
          round(rbind((FA.results$R2)^0.5,FA.results$R2,2*FA.results$R2-1),2)->stats
          dimnames(stats)[[1]]<-c(TXT_correlation_between_scores_and_factors, TXT_multiple_r_square_of_factors_scores,
                                  TXT_min_correlation_between_scores_and_factors)
          dimnames(stats)[[2]]<-noms1
          stats->Resultats$TXT_hierarchical_factorial_analysis$TXT_correlation_between_scores_and_factors
          fa.multi.diagram(multi)
        }
      }
      return(Resultats)
      
    } 
    acp.out<-function(Matrice, data, X, nF, methode, rotation, sat, scor.fac, nom){
      principal(Matrice, nfactors= nF, n.obs=length(data[,1]), rotate=rotation)->PCA
      list()->Resultats
      Resultats$analyse<-paste("analyse en composante principale en utilisant la fonction [principal] du package psych, l'algorithme est", PCA$fm)
      if(!is.null(rotation)) Resultats$rotation<-paste(TXT_rotation_is_a_rotation, rotation) 
      
      PCA<-fa.sort(PCA,polar=FALSE)
      loadfa<-round(as(PCA$loadings, "matrix"),3)
      loadfa[which(abs(loadfa)<sat)]<-" " 
      data.frame("communaute"=round(PCA$communality,3),
                 "specifite"=round(PCA$uniquenesses,3),
                 TXT_complexity=round(PCA$complexity,2))->communaute
      Resultats$INFO_standardized_saturation_on_correlation_matrix<-data.frame(loadfa, communaute)
      var.ex<-round(PCA$Vaccounted,3)
      
      if(nF>1){dimnames(var.ex)[[1]]<-c(TXT_saturations_sum_of_squares, TXT_explained_variance_ratio,
                                        TXT_cumulated_explained_variance_ratio, TXT_explaination_ratio, 
                                        TXT_cumulated_explaination_ratio)} else {
                                          dimnames(var.ex)[[1]]<-c(TXT_saturations_sum_of_squares, TXT_explained_variance_ratio)
                                        }
      Resultats$TXT_explained_variance<-var.ex
      
      paste("TC",1:nF)->noms1
      if(nF>1 & rotation=="oblimin"){  round(PCA$r.scores,3)->cor.f
        Resultats$TXT_correlations_between_factors<-cor.f}
      paste(TXT_mean_complexity_is, mean(PCA$complexity), TXT_this_tests_if, nF, TXT_sufficient_factors )-> Resultats$TXT_mean_complexity
      round(matrix(c(PCA$null.dof,PCA$null.model,
                     PCA$dof, PCA$objective, 
                     PCA$rms, PCA$fit.off, 
                     PCA$chi, PCA$EPVAL, PCA$STATISTIC, PCA$PVAL, PCA$n.obs), ncol=1),4)->stats
      
      
      c(TXT_null_model_degrees_of_freedom, TXT_objective_function_of_null_model,TXT_model_degrees_of_freedom, TXT_objective_function_of_model,
        "RMSR",  TXT_adequation_outside_diagonal, TXT_chi_squared_empirical, TXT_empirical_chi_square_proba_value,
        TXT_chi_squared_likelihood_max, TXT_max_likelihood_chi_squared_proba_value, INFO_total_observations)->dimnames(stats)[[1]]
      
      TXT_values->dimnames(stats)[[2]]
      stats->Resultats$TXT_adequation_adjustement_indexes
      if(scor.fac){
        Scores.fac<-c()
        sapply(data[,X], scale)->centrees
        PCA$weights->matrice2
        t(matrice2)->matrice2
        for(i in 1 : nF){
          apply(centrees%*%matrice2[i,],1,sum)->centrees2
          cbind(Scores.fac,centrees2)->Scores.fac
        }
        data<-data.frame(data,Scores.fac)
        names(data)[(length(data)+1-nF):length(data)]<-paste0(TXT_factor, 1:nF)
        assign(nom, data,envir=.GlobalEnv)
        
      }
      return(Resultats)
    }   
    
    options (warn=-1)
    
    packages<-c("svDialogs", "GPArotation","psych","lavaan", "nFactors")
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    .e <- environment()
    list()->Resultats
    cor<-ifelse(is.null(ord), "cor", "mixed")    
    fa.options<-fa.in(data=data, choix=choix, X=X, imp=imp, ord=ord, nF=nF, rotation=rotation, methode=methode, sat=sat, scor.fac=scor.fac, n.boot=n.boot, hier=hier,nfact2=nfact2, outlier=outlier,
                      sauvegarde=sauvegarde, info=info)
    if(is.null(fa.options)) return(analyse())
    if(is.null(fa.options$choix)) return(fa.options)
    fa.options->>fa.options
    Matrice<-fa.options$Matrice
    data<-fa.options$data
    X<-fa.options$X
    nF<-fa.options$nF
    methode<-fa.options$methode
    rotation<-fa.options$rotation
    sat<-fa.options$sat
    scor.fac<-fa.options$scor.fac
    n.boot<-fa.options$n.boot
    nom<-fa.options$nom
    cor<-fa.options$cor
    hier<-fa.options$hier
    nfact2<-fa.options$nfact2
    Resultats$TXT_correlations_matrix<-fa.options$TXT_correlations_matrix
    Resultats$TXT_adequation_measurement_of_matrix<-fa.options$TXT_adequation_measurement_of_matrix
    Resultats$TXT_parallel_analysis<-fa.options$TXT_parallel_analysis
    
    
    
    if(fa.options$choix==  TXT_factorial_exploratory_analysis |choix=="afe"){
      Resultats$TXT_factorial_analysis<-fa.out(Matrice=Matrice, data=data, X=X, nF=nF, methode=methode, rotation=rotation, sat=sat, 
                                              scor.fac=scor.fac, n.boot=n.boot, nom=nom, hier=hier, cor=cor, nfact2=nfact2)  }
    
    if(fa.options$choix==  TXT_principal_component_analysis |choix=="acp"){
      Resultats$TXT_principal_component_analysis<-acp.out(Matrice=Matrice, data=data, X=X, nF=nF, methode=methode, rotation=rotation, sat=sat, scor.fac=scor.fac, nom=nom)
    }
    
    
    paste(X, collapse="','", sep="")->X
    if(!is.null(fa.options$ord)) paste(fa.options$ord, collapse="','", sep="")->ord
    Resultats$Call<-paste0("factor.an(data=", nom, ",X=c('",X, "'),nF=", nF,", rotation='", rotation, "',methode='",methode, "',sat=", sat,
                           ",outlier='", outlier, "',imp=", ifelse(is.null(imp), "NULL", paste0("'",imp,"'")),",ord=", ifelse(!is.null(ord), paste0("c('", ord,"')"), "NULL"),
                           ",sauvegarde=", sauvegarde, ",scor.fac=", scor.fac, ",n.boot=", n.boot,",hier=", hier, ",nfact2=", nfact2, ",choix='", fa.options$choix, "',info=T)"
    )
    
    
    .add.history(data=data, command=Resultats$Call, nom=nom)
    .add.result(Resultats=Resultats, name =paste(fa.options$choix, Sys.time() ))
    
    
    if(fa.options$sauvegarde) save(Resultats=Resultats, choix=fa.options$choix, env=.e)
    ref1(packages)->Resultats$INFO_references
    if(html) ez.html(Resultats)
    return(Resultats)
    
    }
