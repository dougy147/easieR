ez.cfa <-
  function(modele=NULL, X=NULL, data=NULL,ord=NULL, outlier=TXT_complete_dataset,imp="rm", output="default", info=T, sauvegarde=F, mimic=NULL, fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
           estimator="ML",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
           link="probit",int.ov.free=FALSE, int.lv.free=FALSE, std.lv=FALSE, n.boot=1000, group.w.free=F,
           group.equal=c("loadings", "intercepts", "means", "thresholds", TXT_regressions, "residuals", "residual.covariances", "lv.variances" , "lv.covariances")){
    # modele : lavaan modele if X is null
    # data : dataframe
    # X : character. names of the variables if modele is null
    # LV : Vector. names of LV=atent Variables
    # ord: Character. Vector of ordered variables among X
    # outlier : should outliers be detected and removed on Mahalanobis distance ? (TXT_without_outliers) or not (TXT_complete_dataset)
    # imp : How must missing data be dealt :"rm"= remove, "mean" = impute mean, "median"=impute median, "amelia"=use amelia algorithm for imputation. 
    # output : character vector. List of output that has to be shown. 
    # info : logical. Should information be printed in the console ? 
    # sauvegarde : logical. Must the output be saved in external file ? 
    # mimic : forced argument to determine whether to use or not dialog boxes in specifying options
    # for other options, see lavOptions
    
    options (warn=-1)
    Lav.modele<-function(X=NULL, modele=NULL, LV=NULL, info=T){
      # X : character. Names pf tje manifest variables
      # LV : character. Vector of latent variable names. 
      # modele : lavaan modele
      if(!is.null(modele)){
        semPlot.modele<-try(semPlotModel_lavaanModel(modele))
        if(class(semPlot.modele)=="try-error"){
          msgBox(INFO_model_seems_incorrect_could_not_be_created)
          return(NULL)
        }
        semPaths(semPlot.modele, edge.label.cex = 0.65,edge.color="black", exoVar = FALSE,exoCov =T, cex=0.5)
        cat ("Appuyez [entree] pour continuer")
        line <- readline()
        dlgMessage(ASK_is_model_correct, "yesno")$res->suppression
        if(suppression=="no") return( Lav.modele(X=X, modele=NULL, LV=NULL, info=T)) 
        return(modele)
      }
      
      if(is.null(LV) && length(X)>3) {
        if(info)   writeLines(ASK_latent_variables_number)
        nF<-NA
        while(!is.numeric(nF)) {
          if(info) writeLines(ASK_latent_variables_number) 
          nF <- dlgInput(ASK_factors_number, 2)$res
          if(length(nF)==0) return(NULL)
          strsplit(nF, ":")->nF
          tail(nF[[1]],n=1)->nF
          as.numeric(nF)->nF
          if(any((nF%%1==0)%in% c(FALSE, NA))|| nF<0 || nF>length(X) ){
            msgBox(INFO_facotrs_must_be_positive_int_inferior_to_variables_num)
            nF<-NA }
        }} else if(!is.null(LV)) nF<-length(LV) else nF<-1
        
        O2<-c()
        X->reste
        list()->modele2
        for(i in 1:nF){
          if(is.null(LV[i]))  {dlgInput(paste(TXT_latent_variable_name,i,  "?"), paste(TXT_factor,i, sep="."))$res->noms
            if(length(noms)==0) return(Lav.modele(X=X, LV=NULL))
            strsplit(noms, ":")->noms
            tail(noms[[1]],n=1)->noms} else noms<-LV[i]
            title<-paste(INFO_manifest_variables_of, noms)
            if(i==nF) O1<-reste else O1<- dlgList(reste, preselect=NULL, multiple = TRUE, title=title)$res
            O2<-c(O2,O1)	
            setdiff(reste,O2)->reste
            paste(noms, "=~", O1[1])->modele
            for(j in 2 :(length(O1))){paste(modele, "+", O1[j])->modele}
            modele2[[i]]<-modele
            modele2[[1]]->modele
            if(i>1) {
              for(j in 2 : i){paste(modele,"\n", modele2[[j]])->modele   }
            }
            semPlot.modele<-semPlotModel_lavaanModel(modele)
            semPaths(semPlot.modele, edge.label.cex = 0.65,edge.color="black", exoVar = FALSE,exoCov =T, cex=0.5)
        }
        
        cat ("Appuyez [entree] pour continuer")
        line <- readline()
        dlgMessage(ASK_is_model_correct, "yesno")$res->suppression
        if(suppression=="no") return( Lav.modele(X=X, modele=NULL, LV=NULL, info=T)) 
        return(modele)
    }
    .ez.lavaan.options<-function(modele=NULL, data=NULL, X=NULL, info=TRUE, opt.list=NULL, dial=T, imp=NULL, outlier=NULL,output=NULL){
      if(dial || is.null(opt.list$mimic) || !opt.list$mimic%in% c("default", "Mplus", "EQS")){dial<-T
      if(info) writeLines("Voulez-vous specifier tous les parametres [default] ou imiter un logiciel particulier ?")
      opt.list$mimic<-dlgList(c("default", "Mplus", "EQS"), preselect="default", multiple = FALSE, title=ASK_imitate)$res
      if(length(opt.list$mimic)==0) return(NULL)
      }
      
      if(dial){ 
        if(opt.list$mimic=="default"){ 
          options2<-c("Variables exogenes fixees [fixed.x=default]", "information [information=default]", "correction de continuite [zero.keep.margins=default]",
                      "Vraisemblance (seulement pour estimator=ML) [likelihood=default]") 
        } else options2<-c()
        options<-c("estimateur [estimator=ml])", "groupes [group=NULL]", "test [test=standard]", "erreur standard [se=standard]", "standardisation des variables observees [std.ov=T]", 
                   "Orthogonalite des facteurs [orthogonal=FALSE]", "Lien (seulement pour estimator=MML) [link=probit]",
                   "Intercept des variables observees [int.ov.free=FALSE]", "Intercept des variables latentes [int.lv.free=FALSE]", "Variables exogenes fixees [fixed.x=default]",
                   "Estimation des indicateurs des variables latentes [std.lv=FALSE]", options2)
        
        if(info) writeLines(ASK_which_options_to_specify)
        options<-dlgList(c(TXT_keep_default_values, options), preselect=c("estimateur [estimator=ml])","test [test=standard]", "erreur standard [se=standard]"), multiple = TRUE, title=ASK_which_options)$res
        if(length(options)==0) return(NULL)
        if(options==TXT_keep_default_values) return(list(mimic="default", fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
                                                                 estimator="ml",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
                                                                 link="probit",int.ov.free=FALSE, int.lv.free=FALSE,fixed.x="default", std.lv=FALSE, n.boot=1000, group.w.free=F,
                                                                 group.equal=c("loadings", "intercepts", "means", "thresholds", TXT_regressions, "residuals", "residual.covariances", 
                                                                               "lv.variances" , "lv.covariances")))
      } else options<-NULL
      
      
      
      if(any(options=="estimateur [estimator=ml])")|is.null(opt.list$estimator) || length(opt.list$estimator)!=1|| 
         try(opt.list$estimator %in%c("ML","GLS", "WLS", "ULS", "DWLS", "MLM","MLMV","MLMVS","MLF", "MLR", "WLSM","WLSMV", "ULSM", "ULSMV" ),silent=T)!=T){
        if(info){  writeLines("[WLS] correspond a [ADF]. Les estimateurs avec les extensions [M],[MV],[MVSF],[R] 
                              sont des versions robustes des estimateurs classiques [MV],[WLS], [DWLS], [ULS]")
          abb<-data.frame(abb=c("ML","GLS", "WLS", "ULS", "DWLS"), nom=c(TXT_max_likelihood,TXT_less_square_generalized,TXT_less_square_pondered,TXT_less_square_not_pondered,"moindre carre  pondere diagonalement"))
          print(abb)    }
        opt.list$estimator<-dlgList(c("ML","GLS", "WLS", "ULS", "DWLS", "MLM","MLMV","MLMVS","MLF", "MLR", "WLSM","WLSMV", "ULSM", "ULSMV" ), multiple = FALSE, title=ASK_which_estimator)$res
        if(length(opt.list$estimator)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      
      if(any(options=="test [test=standard]") || length(opt.list$test)!=1 || !opt.list$test%in% c("standard", "Satorra.Bentler", "Yuan.Bentler", "mean.var.adjusted",
                                                                                                  "scaled.shifted", "bootstrap","Bollen.Stine")){
        if(info) writeLines(ASK_which_test)
        opt.list$test<-dlgList(c("standard", "Satorra.Bentler", "Yuan.Bentler", "mean.var.adjusted","scaled.shifted", "bootstrap","Bollen.Stine"), multiple = FALSE, title=ASK_which_estimator)$res
        if(length(opt.list$test)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      
      if(opt.list$test%in%c("boot","bootstrap","Bollen.Stine") &&!is.null(opt.list$n.boot) && ((class(opt.list$n.boot)!="numeric" & class(opt.list$n.boot)!="integer") ||  opt.list$n.boot%%1!=0 || opt.list$n.boot<1)){
        msgBox(INFO_bootstraps_number_must_be_positive) 
        opt.list$n.boot<-NULL
      }
      if(dial & opt.list$test%in%c("boot","bootstrap","Bollen.Stine") || is.null(opt.list$n.boot) & opt.list$test%in%c("boot","bootstrap","Bollen.Stine")) {
        while(is.null(opt.list$n.boot)){
          writeLines(ASK_bootstrap_numbers_1_for_none)
          n.boot<-dlgInput(ASK_bootstraps_number, 1)$res
          if(length(n.boot)==0) {Resultats<-Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)}
          strsplit(n.boot, ":")->n.boot
          tail(n.boot[[1]],n=1)->n.boot
          as.numeric(n.boot)->opt.list$n.boot
          if(is.na(opt.list$n.boot) ||  opt.list$n.boot%%1!=0 || opt.list$n.boot<1){
            msgBox(INFO_bootstraps_number_must_be_positive) 
            opt.list$n.boot<-NULL
          }
        }
      } 
      
      if( any(is.na(data[,X])) & opt.list$estimator=="ml" & opt.list$mimic=="default") opt.list$missing<-"fiml" else opt.list$missing<-"default"
      
      if(opt.list$test%in%c("boot","bootstrap","Bollen.Stine")) se1<-c("standard","first.order", "robust", "bootstrap","none" ) else se1<-c("standard","first.order", "robust", "none" )
      if(any(options=="erreur standard [se]") || is.null(opt.list$se) || !opt.list$se%in%se1)  {
        if(info) writeLines(ASK_how_standard_error_must_be_estimated)
        opt.list$se<-dlgList(se1, multiple = FALSE, title=ASK_standard_error)$res
        if(length(opt.list$se)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      if(any(options=="groupes [group=NULL]") || !is.null(opt.list$group)){
        msg2<-ASK_chose_defining_groups
        .var.type(X=opt.list$group, info=T, data=data, type="factor", message=msg2,multiple=T, title="Variable [groupes] ?", out=X)->group
        if(is.null(group)){
          Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)
        } 
        group$data->data
        group$X->opt.list$group
        if(dial|| any(opt.list$group.equal %in% c("loadings", "intercepts","means","thresholds",TXT_regressions,"residuals","residual.covariances","lv.variances", "lv.covariances"))==FALSE){
          if(info) writeLines(ASK_which_constant_parameters)
          opt.list$group.equal<-dlgList(c("loadings", "intercepts","means","thresholds",TXT_regressions,"residuals","residual.covariances","lv.variances", "lv.covariances"), multiple = T, 
                                        preselect=c("loadings", "intercepts","means","thresholds",TXT_regressions,"residuals","residual.covariances","lv.variances", "lv.covariances"), title=ASK_constant_parameters)$res
          if(length(opt.list$group.equal)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)
          }}
        # ecrase group equal puisque aa libere les group sur cette contraintes ==> utilite ? 
        #group.partial<-dlgList(c("loadings", "intercepts","means","thresholds",TXT_regressions,"residuals","residual.covariances","lv.variances", "lv.covariances"))
        if(info) writeLines(ASK_are_frequences_free_parameters) 
        opt.list$group.w.free<-dlgList(c(TRUE, FALSE), multiple=F, preselect=FALSE, title=ASK_freq_constance)$res
        if(length(opt.list$group.w.free)==0) {Resultats<.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)
        }
      }
      ### zero.keep.margins
      if(any(options=="correction de continuite [zero.keep.margins]") || is.null(opt.list$zero.keep.margins)||(!is.logical(opt.list$zero.keep.margins) & opt.list$zero.keep.margins!="default")){
        if(info) writeLines("Faut-il ajouter une valeur aux cellules vides pour les correlations polychorique ? Pour specifier les valeurs,choisissez TRUE, sinon choisissez [default]")
        opt.list$zero.keep.margins<-dlgList(c(TRUE, FALSE,"default"), preselect="default", multiple = FALSE, title=ASK_empty_cells)$res
        if(length(opt.list$zero.keep.margins)==0) {
          Resultats<-Resultats<.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)
        }
      } 
      
      if( opt.list$zero.keep.margins==TRUE){
        if(!is.null(opt.list$zero.add) && ((class(opt.list$zero.add)!="numeric" ) || any( opt.list$zero.add<0) || any(opt.list$zero.add>1))){
          msgBox(TXT_correction_for_polyc_corr_must_be_between_zero_and_one) 
          opt.list$zero.add<-NULL
        }
        while(is.null(opt.list$zero.add)){
          writeLines(ASK_2x2_table_value)
          zero.add1<-dlgInput(ASK_2x2_table, 0.5)$res
          if(length(zero.add1)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)}
          strsplit(zero.add1, ":")->zero.add1
          tail(zero.add1[[1]],n=1)->zero.add1
          as.numeric(zero.add1)->zero.add1
          if(is.na(zero.add1) ||  zero.add1<0 || zero.add1>1){
            msgBox(INFO_value_must_be_between_zero_and_one) 
            opt.list$zero.add<-NA} else{
              writeLines(ASK_bigger_tables_value)
              zero.add2<-dlgInput("tableau > 2x2 ?", 0)$res
              if(length(zero.add2)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
              return(Resultats)}
              strsplit(zero.add2, ":")->zero.add2
              tail(zero.add2[[1]],n=1)->zero.add2
              as.numeric(zero.add2)->zero.add2
              if(is.na(zero.add2) ||  zero.add2<0 || zero.add2>1){
                msgBox(INFO_value_must_be_between_zero_and_one) 
                opt.list$zero.add<-NA}
            }
          opt.list$zero.add<-c(zero.add1,zero.add2)
          
        }
      } 
      
      
      ### fin zero.keep.margins
      if(any(options=="Vraisemblance (seulement pour estimator=ML) [likelihood=default]") & opt.list$mimic=="default" & opt.list$estimator=="ML" ||is.null(opt.list$likelihood) || length(opt.list$likelihood)!=1 || try(opt.list$likelihood%in%c("wishart","normal", "default" ),silent=T)!=T) {
        if(info) writeLines(ASK_specify_likelihood)
        opt.list$likelihood<-dlgList(c("wishart","normal", "default" ), multiple=F, preselect="default", title=ASK_likelihood)$res # depend de mimic
        if(length(opt.list$likelihood)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      if(any(options=="Lien (seulement pour estimator=MML) [link=probit]") & opt.list$estimator=="MML" ||length(opt.list$link)!=1 || try(opt.list$link%in%c("logit","probit" ),silent=T)!=T ){
        if(info) writeLines(ASK_family)
        opt.list$link<-dlgList(c("logit","probit" ), multiple=F, preselect=FALSE, title=ASK_distribution)$res
        if(length(opt.list$link)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }  
      
      
      if(any(options=="information [information=default]") ||is.null(opt.list$information) || try(opt.list$information%in%c("expected","observed", "default" ),silent=T)!=T ){
        if(info) writeLines(ASK_which_information_matrix_for_standard_error_estimation)
        opt.list$information<-dlgList(c("expected","observed", "default" ), multiple=F, preselect=FALSE, title=ASK_information_matrix)$res
        if(length(opt.list$information)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }  
      
      if(any(options=="Variables exogenes fixees [fixed.x=default]") ||length(opt.list$fixed.x)!=1 || (!is.logical(opt.list$fixed.x) & opt.list$fixed.x!="default") ){
        if(info) writeLines(INFO_if_true_covariates_as_fixed)
        opt.list$fixed.x<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title=ASK_fixed_covariables)$res
        if(length(opt.list$fixed.x)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }  
      
      if(any(options=="Orthogonalite des facteurs [orthogonal=FALSE]") ||length(opt.list$orthogonal)!=1 || !is.logical(opt.list$orthogonal) ){
        if(info) writeLines(ASK_correlated_or_orthogonal_factors)
        opt.list$orthogonal<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title=ASK_factors_ortho)$res
        if(length(opt.list$orthogonal)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      if(any(options=="standardisation des variables observees [std.ov=T]") ||length(opt.list$std.ov)!=1 || !is.logical(opt.list$std.ov) ){
        if(info) writeLines(ASK_standardize_obs_variables_before)
        opt.list$std.ov<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title=ASK_standardization)$res
        if(length(opt.list$std.ov)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      #####
      if(any(options=="Intercept des variables observees [int.ov.free=FALSE]") ||length(opt.list$int.ov.free)!=1 || !is.logical(opt.list$int.ov.free) ){
        if(info) writeLines(ASK_should_intercept_of_obs_variables_be_fixed_to_zero)
        opt.list$int.ov.free<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Intercept VO=0 ?")$res
        if(length(opt.list$int.ov.free)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }
      
      
      if(any(options=="Intercept des variables latentes [int.lv.free=FALSE]") ||length(opt.list$int.lv.free)!=1 || !is.logical(opt.list$int.lv.free) ){
        if(info) writeLines(ASK_should_intercept_of_latent_variable_be_fixed_to_zero)
        opt.list$int.lv.free<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Intercept VL=0 ?")$res
        if(length(opt.list$int.lv.free)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }
      
      
      
      if(any(options=="Estimation des indicateurs des variables latentes [std.lv=FALSE]") ||length(opt.list$std.lv)!=1 || !is.logical(opt.list$std.lv) ){
        if(info) writeLines(INFO_if_true_latent_residuals_one)
        opt.list$std.lv<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title=ASK_standardization_vl)$res
        if(length(opt.list$std.lv)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      
      return(opt.list)
    }
    
    
    
    cfa.in<-function(modele=NULL,X=NULL,LV=NULL, data=NULL, ord=NULL, outlier=NULL,imp=NULL,output=NULL, info=T, opt.list=list(), sauvegarde=F){
      
      Resultats<-list()
      if(is.null(data) | is.null(modele))  {dial<-TRUE}else dial<-F 
      if(dial || class(data)!="data.frame"){
        data<-choix.data(data=data, info=info, nom=T)
        if(length(data)==0) return(NULL) 
        nom<-data[[1]]
        data<-data[[2]]  
      }else{
        deparse(substitute(data))->nom  
      }
      
      
      if(is.null(modele)){ 
        msg3<-ASK_chose_manifest_variables_at_least_three 
        
        X<-.var.type(X=X, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=T, title=TXT_variables, out=NULL)
        data<-X$data
        X<-X$X
        if(is.null(X) || length(X)<3) return(NULL)
        
        
        if(dial || length(outlier)>1 || outlier %in% c(TXT_complete_dataset, TXT_without_outliers) ==FALSE){
          if(info) writeLines(ASK_analysis_on_complete_data_or_remove_outliers)
          if(info) writeLines(INFO_outliers_identified_on_mahalanobis)
          outlier<- dlgList(c(TXT_complete_dataset, TXT_without_outliers), preselect=TXT_complete_dataset,multiple = FALSE, title=ASK_results_desired)$res
          if(length(outlier)==0) { Resultats<-cfa.in()
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
          
          if(length(type)==0) {Resultats<-cfa.in()
          return(Resultats)}
        } else{if(is.null(ord)) type<-"continues" else type<-TXT_dichotomic_ordinal
        }
        
        if(type!="continues"){ 
          if(type=="mixte") {
            if(info) writeLines(ASK_ordinal_variables) 
            ord<-dlgList(X, multiple = TRUE, title=ASK_ordinal_variables)$res
            if(length(ord)==0) {Resultats<-cfa.in()
            return(Resultats)}
          }else ord<-X
        }
        
        modele<-Lav.modele(X=X, LV=LV)
        if(is.null(modele)) {
          Resultats<-cfa.in()
          return(Resultats) 
        }}else{
          modele<-Lav.modele(modele=modele)
          if(is.null(modele)) {
            Resultats<-cfa.in()
            return(Resultats)
          }
        }
      if(any(is.na(data[,X]))) {
        if(is.null(imp))  {msgBox(ASK_missing_values_detected_what_to_do)
          imp<- dlgList(c(TXT_do_nothing_keep_all_obs, TXT_delete_observations_with_missing_values,TXT_replace_by_median,TXT_multiple_imputation_amelia), 
                        preselect=FALSE, multiple = TRUE, title=ASK_missing_values_treatment)$res}
        if(length(imp)==0){
          Resultats<-cfa.in()
          return(Resultats)
        }
        data1<-ez.imp(data[, X], imp=imp, ord= ord)
        diff<-setdiff(names(data), X)
        data<-data.frame(data1, data[which(dimnames(data)[[1]] %in% dimnames(data1)[[1]]),diff])
      }  
      
      
      
      Resultats$opt.list<-.ez.lavaan.options(data=data, X=X, info=TRUE, opt.list=opt.list, dial=dial) 
      if(is.null( Resultats$opt.list)) {
        Resultats<-cfa.in()
        return(Resultats)
      }
      
      
      if(dial || class(output)!="character"|| any(!output%in% c("default", TXT_default_outputs, "parEst", TXT_estimated_parameters, "parSt", TXT_standardized_parameters,TXT_covariance_matrix_adjusted, "fitted.cov",
                                                                TXT_residue_standardized, "res.St","res.Unst",TXT_non_standardized_residuals,"vcov",TXT_covariance_matrix_estimated,
                                                                "AIC", "BIC", TXT_adequation_measures,"fitM", TXT_inspect_initial_values, "start", TXT_inspect_model_matrices,
                                                                "modmat", TXT_inspect_model_representation, "modrep"))==TRUE){
        if(info) writeLines(ASK_which_results_warning_on_default_output)
        output<-c( TXT_default_outputs, TXT_estimated_parameters, TXT_standardized_parameters,TXT_covariance_matrix_adjusted, 
                   TXT_residue_standardized, TXT_non_standardized_residuals,TXT_covariance_matrix_estimated,"AIC", "BIC", TXT_adequation_measures, 
                   TXT_inspect_initial_values,  TXT_inspect_model_matrices, TXT_inspect_model_representation)
        if(info) writeLines(ASK_which_output_results)
        output<- dlgList(output, preselect=TXT_default_outputs, multiple = TRUE, title=ASK_results_output)$res
        if(is.null( Resultats$opt.list)) {
          Resultats<-cfa.in()
          return(Resultats)
        }
      }
      
      
      if(dial || length(sauvegarde)!=1 || !is.logical(sauvegarde)){
        sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title=ASK_save_results)$res
        if(length(sauvegarde)==0) {
          Resultats<-cfa.in()
          return(Resultats)}
      }   
      
      Resultats$ord<-ord
      Resultats$data<-data
      Resultats$nom<-nom
      Resultats$modele<-modele 
      Resultats$output<-output 
      Resultats$sauvegarde<-sauvegarde
      
      return(Resultats)  
    }
    
    cfa.out<-function(cfa.options){
      .e <- environment()
      list()->Resultats
      
      data<-cfa.options$data   
      modele<-cfa.options$modele
      nom.v<-strsplit(modele, split="[\\|,+,='\n'~' ']+")
      var.mod<-which(names(data)%in% nom.v[[1]])
      ord<-cfa.options$ord
      output<-cfa.options$output
      sauvegarde<-cfa.options$sauvegarde
      cfa.options$opt.list->opt.list   
      
      opt.list$mimic->mimic
      opt.list$fixed.x->fixed.x
      opt.list$missing->missing
      opt.list$information->information
      opt.list$zero.keep.margins->zero.keep.margins
      opt.list$zero.add->zero.add
      if(is.null(zero.add)) zero.add<-"default"
      opt.list$estimator->estimator
      if(estimator=="ML" & (!is.null(ord)|any( unlist(sapply(data[,var.mod], class))=="factor") )) estimator<-"WLSMV"
      opt.list$group->group
      opt.list$test->test
      opt.list$se->se
      opt.list$std.ov->std.ov
      opt.list$orthogonal->orthogonal
      opt.list$likelihood->likelihood
      if(estimator!="ML")likelihood<-"default"
      opt.list$link->link
      opt.list$int.ov.free->int.ov.free
      opt.list$int.lv.free->int.lv.free
      opt.list$fixed.x->fixed.x
      opt.list$std.lv->std.lv
      opt.list$n.boot->n.boot
      opt.list$group.w.free->group.w.free
      if(is.null(group.w.free)) group.w.free<-F
      opt.list$group.equal->group.equal
      
      
      fit<-try( lavaan::cfa(modele, data = data, ordered=ord,estimator=estimator, test=test,
                            bootstrap=n.boot,meanstructure="default",zero.cell.warn=F, 
                            missing=missing, group=group, #ifelse(!is.null(group), group.equal=group.equal,group.equal="means"),
                            group.w.free= group.w.free,fixed.x=fixed.x,information=information,se=se,std.ov=as.logical(std.ov),
                            orthogonal=as.logical(orthogonal),likelihood=likelihood, link=link, int.ov.free=as.logical(int.ov.free),
                            int.lv.free=as.logical(int.lv.free),std.lv=as.logical(std.lv),zero.add=zero.add, zero.keep.margins=zero.keep.margins), silent=T)
      if(class(fit)=="try-error") {msgBox(ASK_could_not_finish_analysis_respecify_parameters)
        return(ez.cfa())}
      
      if(any(output== "default") | any(output== TXT_default_outputs))  {
        print(summary(fit, fit.measures = TRUE, standardized=T))
        Resultats<-INFO_to_display_results_use_summary
        summary(fit)->>fit
        if(length(output)==1) fit->>modele.cfa
        }
      if(any(output== "parEst") | any(output==TXT_estimated_parameters)) parameterEstimates(fit)->Resultats$TXT_estimated_parameters_not_standardized
      if(any(output== "parSt") | any(output==TXT_standardized_parameters)) standardizedSolution(fit)->Resultats$TXT_estimated_parameters_standardized
      if(any(output== TXT_covariance_matrix_adjusted) | any(output=="fitted.cov")) fitted(fit)->Resultats$TXT_covariance_matrix_adjusted
      if(any(output== TXT_residue_standardized) | any(output=="res.St")) resid(fit, type="standardized")->Resultats$TXT_residue_standardized
      if(any(output== TXT_non_standardized_residuals) | any(output=="res.Unst")) resid(fit)->Resultats$TXT_non_standardized_residuals
      if(any(output== "vcov") | any(output==TXT_covariance_matrix_estimated)) vcov(fit)->Resultat$TXT_covariance_matrix_estimated
      if(any(output== "AIC") ) AIC(fit)->Resultats$AIC
      if(any(output== "BIC") ) BIC(fit)->Resultats$BIC
      if(any(output== TXT_adequation_measures) | any(output=="fitM")) fitMeasures(fit)->Resultats$TXT_adjustement_measure
      if(any(output== TXT_inspect_initial_values) | any(output=="start"))inspect(fit, what=start)->Resultats$TXT_init_values
      if(any(output== TXT_inspect_model_matrices) | any(output=="modmat")) inspect(fit)->Resultats$TXT_model_matrix
      if(any(output== TXT_inspect_model_representation) | any(output=="modrep"))inspect(fit, what=list)->Resultats$TXT_model_representation
      semPaths(fit, what="path", whatLabels="std", edge.label.cex = 0.65,edge.color="black", exoVar = FALSE,exoCov =T)
      
      
      return(Resultats)
      
      
      
    } 
    
    packages<-c("svDialogs", "psych","lavaan","semPlot")
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    try( windows(record=T), silent=T)->win
    if(class(win)=="try-error") quartz()    
    
    Resultats<-list()
    opt.list<-list(mimic=mimic,fixed.x=fixed.x,missing=missing,information=information,zero.keep.margins=zero.keep.margins,zero.add=zero.add,
                   estimator=estimator,group=group,test=test,se=se,std.ov=std.ov,orthogonal=orthogonal,likelihood=likelihood,
                   link=link,int.ov.free=int.ov.free,int.lv.free=int.lv.free,fixed.x=fixed.x,std.lv=std.lv,n.boot=n.boot,group.w.free=group.w.free,group.equal=group.equal)
    cfa.options<-cfa.in(modele=modele,X=X, data=data, ord=ord, outlier=outlier,imp=imp,output=output, info=T, opt.list=opt.list, sauvegarde=sauvegarde)
    if(is.null(cfa.options)) return(analyse())
    AFC<-cfa.out(cfa.options)
    if(!is.null(AFC)) Resultats$AFC<-AFC
    if(AFC[[1]]=="Default ouput") print(summary(modele.cfa))
    
   try({
      def.values<-list(mimic="default", fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
                     estimator="ml",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
                     link="probit",int.ov.free=FALSE, int.lv.free=FALSE,fixed.x="default", std.lv=FALSE, n.boot=1000, group.w.free=F,
                     group.equal=c("loadings", "intercepts", "means", "thresholds", TXT_regressions, "residuals", "residual.covariances", 
                                   "lv.variances" , "lv.covariances"))
    
    if(!is.null(cfa.options$ord)) paste(cfa.options$ord, collapse="','", sep="")->ord
    paste(cfa.options$output, collapse="','", sep="")->output
    call<-paste0("ez.cfa(modele='", cfa.options$modele, "',data=", cfa.options$nom, ",ord=", ifelse(is.null(cfa.options$ord), "NULL",paste0("c('",ord,"')")),",outlier='", outlier, 
                 "', imp='",imp,"',output=c('", output,"'), sauvegarde=", cfa.options$sauvegarde, ", mimic='", cfa.options$opt.list$mimic, "'")
    
    for(i in 1:length(def.values)){
      if(names(def.values)[i]!="group" & names(def.values)[i]!="mimic") n<-which(names(cfa.options$opt.list) == names(def.values)[i]) else n<-NULL
      if(is.null(def.values[[i]])) call<-ifelse(is.null(cfa.options$opt.list$group),paste0(call, ", group=NULL"), paste0(call,", group =",cfa.options$opt.list$group)) 
      if(length(n)==1){
        if( def.values[[i]] !=cfa.options$opt.list[[n]]){
          if(is.logical(def.values[[i]]) ) call<-paste0(call, ",", names(cfa.options$opt.list)[n],"=",cfa.options$opt.list[[n]])
          if(is.character(def.values[[i]]) & length(is.character(def.values[[i]]))==1 ) call<-paste0(call, ",", names(cfa.options$opt.list)[n],"='",cfa.options$opt.list[[n]],"'")
          if(is.character(def.values[[i]]) & length(is.character(def.values[[i]]))>1 ){
            paste(cfa.options$opt.list[[n]], collapse="','", sep="")->param
            call<-paste0(call, ",", names(cfa.options$opt.list)[i],"=c('",param,"')") 
          } 
        }} 
    }
    call<-paste0(call,")")
    Resultats$Call<-call
    
    .add.history(data=cfa.options$data, command=Resultats$Call, nom=cfa.options$nom)
    .add.result(Resultats=Resultats, name =paste("AFC", Sys.time() ))  
   }, silent=T) 
    
   
    
    if(sauvegarde) save(Resultats=Resultats, choix="AFC", env=.e)
    Resultats$ref<-ref1(packages)
    try(ez.html(Resultats), silent=T)
    return(Resultats)
  }
