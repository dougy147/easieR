ez.cfa <-
  function(modele=NULL, X=NULL, data=NULL,ord=NULL, outlier="Complete data",imp="rm", output="default", info=T, sauvegarde=F, mimic=NULL, fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
           estimator="ML",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
           link="probit",int.ov.free=FALSE, int.lv.free=FALSE, std.lv=FALSE, n.boot=1000, group.w.free=F,
           group.equal=c("loadings", "intercepts", "means", "thresholds", "regressions", "residuals", "residual.covariances", "lv.variances" , "lv.covariances")){
    # modele : lavaan modele if X is null
    # data : dataframe
    # X : character. names of the variables if modele is null
    # LV : Vector. names of LV=atent Variables
    # ord: Character. Vector of ordered variables among X
    # outlier : should outliers be detected and removed on Mahalanobis distance ? ("Data without influencing value") or not ("Complete data")
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
          msgBox("The model appears to be incorrect and could not be created.")
          return(NULL)
        }
        semPaths(semPlot.modele, edge.label.cex = 0.65,edge.color="black", exoVar = FALSE,exoCov =T, cex=0.5)
        cat ("Appuyez [entree] pour continuer")
        line <- readline()
        dlgMessage("Is your model correct?", "yesno")$res->suppression
        if(suppression=="no") return( Lav.modele(X=X, modele=NULL, LV=NULL, info=T)) 
        return(modele)
      }
      
      if(is.null(LV) && length(X)>3) {
        if(info)   writeLines("Please specify the number of latent variables")
        nF<-NA
        while(!is.numeric(nF)) {
          if(info) writeLines("Please specify the number of latent variables") 
          nF <- dlgInput("Number of factors?", 2)$res
          if(length(nF)==0) return(NULL)
          strsplit(nF, ":")->nF
          tail(nF[[1]],n=1)->nF
          as.numeric(nF)->nF
          if(any((nF%%1==0)%in% c(FALSE, NA))|| nF<0 || nF>length(X) ){
            msgBox("The number of factors must be a positive integer less than the number of variables")
            nF<-NA }
        }} else if(!is.null(LV)) nF<-length(LV) else nF<-1
        
        O2<-c()
        X->reste
        list()->modele2
        for(i in 1:nF){
          if(is.null(LV[i]))  {dlgInput(paste("Name of the latent variable",i,  "?"), paste("Factor",i, sep="."))$res->noms
            if(length(noms)==0) return(Lav.modele(X=X, LV=NULL))
            strsplit(noms, ":")->noms
            tail(noms[[1]],n=1)->noms} else noms<-LV[i]
            title<-paste("Obvious variables of", noms)
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
        dlgMessage("Is your model correct?", "yesno")$res->suppression
        if(suppression=="no") return( Lav.modele(X=X, modele=NULL, LV=NULL, info=T)) 
        return(modele)
    }
    .ez.lavaan.options<-function(modele=NULL, data=NULL, X=NULL, info=TRUE, opt.list=NULL, dial=T, imp=NULL, outlier=NULL,output=NULL){
      if(dial || is.null(opt.list$mimic) || !opt.list$mimic%in% c("default", "Mplus", "EQS")){dial<-T
      if(info) writeLines("Voulez-vous specifier tous les parametres [default] ou imiter un logiciel particulier ?")
      opt.list$mimic<-dlgList(c("default", "Mplus", "EQS"), preselect="default", multiple = FALSE, title="Imitate?")$res
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
        
        if(info) writeLines("What options do you want to specify?")
        options<-dlgList(c("Keep the default values", options), preselect=c("estimateur [estimator=ml])","test [test=standard]", "erreur standard [se=standard]"), multiple = TRUE, title="What options?")$res
        if(length(options)==0) return(NULL)
        if(options=="Keep the default values") return(list(mimic="default", fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
                                                                 estimator="ml",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
                                                                 link="probit",int.ov.free=FALSE, int.lv.free=FALSE,fixed.x="default", std.lv=FALSE, n.boot=1000, group.w.free=F,
                                                                 group.equal=c("loadings", "intercepts", "means", "thresholds", "regressions", "residuals", "residual.covariances", 
                                                                               "lv.variances" , "lv.covariances")))
      } else options<-NULL
      
      
      
      if(any(options=="estimateur [estimator=ml])")|is.null(opt.list$estimator) || length(opt.list$estimator)!=1|| 
         try(opt.list$estimator %in%c("ML","GLS", "WLS", "ULS", "DWLS", "MLM","MLMV","MLMVS","MLF", "MLR", "WLSM","WLSMV", "ULSM", "ULSMV" ),silent=T)!=T){
        if(info){  writeLines("[WLS] correspond a [ADF]. Les estimateurs avec les extensions [M],[MV],[MVSF],[R] 
                              sont des versions robustes des estimateurs classiques [MV],[WLS], [DWLS], [ULS]")
          abb<-data.frame(abb=c("ML","GLS", "WLS", "ULS", "DWLS"), nom=c("maximum likelihood","lesser square generalized","least weighted square","least unweighted edge","moindre carre  pondere diagonalement"))
          print(abb)    }
        opt.list$estimator<-dlgList(c("ML","GLS", "WLS", "ULS", "DWLS", "MLM","MLMV","MLMVS","MLF", "MLR", "WLSM","WLSMV", "ULSM", "ULSMV" ), multiple = FALSE, title="Which estimator?")$res
        if(length(opt.list$estimator)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      
      if(any(options=="test [test=standard]") || length(opt.list$test)!=1 || !opt.list$test%in% c("standard", "Satorra.Bentler", "Yuan.Bentler", "mean.var.adjusted",
                                                                                                  "scaled.shifted", "bootstrap","Bollen.Stine")){
        if(info) writeLines("Which test do you want to use?")
        opt.list$test<-dlgList(c("standard", "Satorra.Bentler", "Yuan.Bentler", "mean.var.adjusted","scaled.shifted", "bootstrap","Bollen.Stine"), multiple = FALSE, title="Which estimator?")$res
        if(length(opt.list$test)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      
      if(opt.list$test%in%c("boot","bootstrap","Bollen.Stine") &&!is.null(opt.list$n.boot) && ((class(opt.list$n.boot)!="numeric" & class(opt.list$n.boot)!="integer") ||  opt.list$n.boot%%1!=0 || opt.list$n.boot<1)){
        msgBox("The number of bootstrap must be a positive integer") 
        opt.list$n.boot<-NULL
      }
      if(dial & opt.list$test%in%c("boot","bootstrap","Bollen.Stine") || is.null(opt.list$n.boot) & opt.list$test%in%c("boot","bootstrap","Bollen.Stine")) {
        while(is.null(opt.list$n.boot)){
          writeLines("Please specify the number of bootstrap. To not have a bootstrap, choose 1")
          n.boot<-dlgInput("Number of bootstrap?", 1)$res
          if(length(n.boot)==0) {Resultats<-Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)}
          strsplit(n.boot, ":")->n.boot
          tail(n.boot[[1]],n=1)->n.boot
          as.numeric(n.boot)->opt.list$n.boot
          if(is.na(opt.list$n.boot) ||  opt.list$n.boot%%1!=0 || opt.list$n.boot<1){
            msgBox("The number of bootstrap must be a positive integer") 
            opt.list$n.boot<-NULL
          }
        }
      } 
      
      if( any(is.na(data[,X])) & opt.list$estimator=="ml" & opt.list$mimic=="default") opt.list$missing<-"fiml" else opt.list$missing<-"default"
      
      if(opt.list$test%in%c("boot","bootstrap","Bollen.Stine")) se1<-c("standard","first.order", "robust", "bootstrap","none" ) else se1<-c("standard","first.order", "robust", "none" )
      if(any(options=="erreur standard [se]") || is.null(opt.list$se) || !opt.list$se%in%se1)  {
        if(info) writeLines("How should the standard error be estimated?")
        opt.list$se<-dlgList(se1, multiple = FALSE, title="Standard error?")$res
        if(length(opt.list$se)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      if(any(options=="groupes [group=NULL]") || !is.null(opt.list$group)){
        msg2<-"Please choose the defining groups"
        .var.type(X=opt.list$group, info=T, data=data, type="factor", message=msg2,multiple=T, title="Variable [groupes] ?", out=X)->group
        if(is.null(group)){
          Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)
        } 
        group$data->data
        group$X->opt.list$group
        if(dial|| any(opt.list$group.equal %in% c("loadings", "intercepts","means","thresholds","regressions","residuals","residual.covariances","lv.variances", "lv.covariances"))==FALSE){
          if(info) writeLines("What are the parameters that you want to keep constant?")
          opt.list$group.equal<-dlgList(c("loadings", "intercepts","means","thresholds","regressions","residuals","residual.covariances","lv.variances", "lv.covariances"), multiple = T, 
                                        preselect=c("loadings", "intercepts","means","thresholds","regressions","residuals","residual.covariances","lv.variances", "lv.covariances"), title="Constant parameters?")$res
          if(length(opt.list$group.equal)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)
          }}
        # ecrase group equal puisque aa libere les group sur cette contraintes ==> utilite ? 
        #group.partial<-dlgList(c("loadings", "intercepts","means","thresholds","regressions","residuals","residual.covariances","lv.variances", "lv.covariances"))
        if(info) writeLines("est-ce que les frequences des differents group est un parametre libre ? ") 
        opt.list$group.w.free<-dlgList(c(TRUE, FALSE), multiple=F, preselect=FALSE, title="Constant frequency?")$res
        if(length(opt.list$group.w.free)==0) {Resultats<.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)
        }
      }
      ### zero.keep.margins
      if(any(options=="correction de continuite [zero.keep.margins]") || is.null(opt.list$zero.keep.margins)||(!is.logical(opt.list$zero.keep.margins) & opt.list$zero.keep.margins!="default")){
        if(info) writeLines("Faut-il ajouter une valeur aux cellules vides pour les correlations polychorique ? Pour specifier les valeurs,choisissez TRUE, sinon choisissez [default]")
        opt.list$zero.keep.margins<-dlgList(c(TRUE, FALSE,"default"), preselect="default", multiple = FALSE, title="Empty cells?")$res
        if(length(opt.list$zero.keep.margins)==0) {
          Resultats<-Resultats<.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)
        }
      } 
      
      if( opt.list$zero.keep.margins==TRUE){
        if(!is.null(opt.list$zero.add) && ((class(opt.list$zero.add)!="numeric" ) || any( opt.list$zero.add<0) || any(opt.list$zero.add>1))){
          msgBox("The correction for the calculation of polycoric correlations must be between 0 and 1.") 
          opt.list$zero.add<-NULL
        }
        while(is.null(opt.list$zero.add)){
          writeLines("Please specify the value for 2x2 tables")
          zero.add1<-dlgInput("2x2 array?", 0.5)$res
          if(length(zero.add1)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)}
          strsplit(zero.add1, ":")->zero.add1
          tail(zero.add1[[1]],n=1)->zero.add1
          as.numeric(zero.add1)->zero.add1
          if(is.na(zero.add1) ||  zero.add1<0 || zero.add1>1){
            msgBox("The value must be between 0 and 1") 
            opt.list$zero.add<-NA} else{
              writeLines("Please specify the value for tables larger than 2x2")
              zero.add2<-dlgInput("array> 2x2?", 0)$res
              if(length(zero.add2)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
              return(Resultats)}
              strsplit(zero.add2, ":")->zero.add2
              tail(zero.add2[[1]],n=1)->zero.add2
              as.numeric(zero.add2)->zero.add2
              if(is.na(zero.add2) ||  zero.add2<0 || zero.add2>1){
                msgBox("The value must be between 0 and 1") 
                opt.list$zero.add<-NA}
            }
          opt.list$zero.add<-c(zero.add1,zero.add2)
          
        }
      } 
      
      
      ### fin zero.keep.margins
      if(any(options=="Vraisemblance (seulement pour estimator=ML) [likelihood=default]") & opt.list$mimic=="default" & opt.list$estimator=="ML" ||is.null(opt.list$likelihood) || length(opt.list$likelihood)!=1 || try(opt.list$likelihood%in%c("wishart","normal", "default" ),silent=T)!=T) {
        if(info) writeLines("Please specify the reasonableness.")
        opt.list$likelihood<-dlgList(c("wishart","normal", "default" ), multiple=F, preselect="default", title="Likelihood?")$res # depend de mimic
        if(length(opt.list$likelihood)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      if(any(options=="Lien (seulement pour estimator=MML) [link=probit]") & opt.list$estimator=="MML" ||length(opt.list$link)!=1 || try(opt.list$link%in%c("logit","probit" ),silent=T)!=T ){
        if(info) writeLines("Please specify the family (i.e. form of distribution).")
        opt.list$link<-dlgList(c("logit","probit" ), multiple=F, preselect=FALSE, title="Distribution ?")$res
        if(length(opt.list$link)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }  
      
      
      if(any(options=="information [information=default]") ||is.null(opt.list$information) || try(opt.list$information%in%c("expected","observed", "default" ),silent=T)!=T ){
        if(info) writeLines("On which information matrix should the estimation of standard errors be carried out?")
        opt.list$information<-dlgList(c("expected","observed", "default" ), multiple=F, preselect=FALSE, title="Information matrix?")$res
        if(length(opt.list$information)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }  
      
      if(any(options=="Variables exogenes fixees [fixed.x=default]") ||length(opt.list$fixed.x)!=1 || (!is.logical(opt.list$fixed.x) & opt.list$fixed.x!="default") ){
        if(info) writeLines("If true, we consider the exogenous covaries as fixed, otherwise we consider them as random and their parameters are free")
        opt.list$fixed.x<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Fixed covariates?")$res
        if(length(opt.list$fixed.x)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }  
      
      if(any(options=="Orthogonalite des facteurs [orthogonal=FALSE]") ||length(opt.list$orthogonal)!=1 || !is.logical(opt.list$orthogonal) ){
        if(info) writeLines("Are the factors correlated (FALSE) or are they orthogonal (TRUE)?")
        opt.list$orthogonal<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Orthogonality of factors?")$res
        if(length(opt.list$orthogonal)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      if(any(options=="standardisation des variables observees [std.ov=T]") ||length(opt.list$std.ov)!=1 || !is.logical(opt.list$std.ov) ){
        if(info) writeLines("Should we standardize (i.e. center reduce) the variables observed beforehand (TRUE) or not (FALSE)?")
        opt.list$std.ov<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Standardization?")$res
        if(length(opt.list$std.ov)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      } 
      
      #####
      if(any(options=="Intercept des variables observees [int.ov.free=FALSE]") ||length(opt.list$int.ov.free)!=1 || !is.logical(opt.list$int.ov.free) ){
        if(info) writeLines("Should we set the intercept of the observed variables to 0?")
        opt.list$int.ov.free<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Intercept VO=0 ?")$res
        if(length(opt.list$int.ov.free)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }
      
      
      if(any(options=="Intercept des variables latentes [int.lv.free=FALSE]") ||length(opt.list$int.lv.free)!=1 || !is.logical(opt.list$int.lv.free) ){
        if(info) writeLines("Should the intercept of latent variables be set to 0?")
        opt.list$int.lv.free<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Intercept VL=0 ?")$res
        if(length(opt.list$int.lv.free)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }
      
      
      
      if(any(options=="Estimation des indicateurs des variables latentes [std.lv=FALSE]") ||length(opt.list$std.lv)!=1 || !is.logical(opt.list$std.lv) ){
        if(info) writeLines("If true, the residuals of the latent variables are fixed at 1, otherwise the parameters of the latent variable are estimated by fixing the first indicator at 1")
        opt.list$std.lv<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Vehicle standardization?")$res
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
        msg3<-"Please choose the manifest variables you want to analyze. You must choose at least 3 variables" 
        
        X<-.var.type(X=X, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=T, title="Variables", out=NULL)
        data<-X$data
        X<-X$X
        if(is.null(X) || length(X)<3) return(NULL)
        
        
        if(dial || length(outlier)>1 || outlier %in% c("Complete data", "Data without influencing value") ==FALSE){
          if(info) writeLines("Do you want the analysis on the complete data or on the data for which the influencing values have been removed?")
          if(info) writeLines("influencing values are identified based on the Mahalanobis distance with a chi threshold of 0.001")
          outlier<- dlgList(c("Complete data", "Data without influencing value"), preselect="Complete data",multiple = FALSE, title="What results do you want to achieve?")$res
          if(length(outlier)==0) { Resultats<-cfa.in()
          return(Resultats)}
        }
        
        if(outlier=="Data without influencing value"){
          inf<-VI.multiples(data,X)
          Resultats$"Values considered influential"<-inf$"Values considered influential"
          data<-inf$data
        }
        
        if(dial){
          if(info) writeLines("Veuillez preciser le type de variables. Des correlations tetra/polychoriques seront realisees sur les variables dichotomiques/ordinales et Bravais-Pearson sur les variables continues")
          if(length(unique(unlist(data[,X])))<9) {type<-dlgList(c("dichotomiques/ordinales","continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature of the variables?")$res}else {
            type<-dlgList(c("continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature of the variables?")$res 
          }
          
          if(length(type)==0) {Resultats<-cfa.in()
          return(Resultats)}
        } else{if(is.null(ord)) type<-"continues" else type<-"dichotomiques/ordinales"
        }
        
        if(type!="continues"){ 
          if(type=="mixte") {
            if(info) writeLines("Please specify ordinal variables?") 
            ord<-dlgList(X, multiple = TRUE, title="Ordinal variables?")$res
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
        if(is.null(imp))  {msgBox("Missing values have been detected. How do you want to treat them?")
          imp<- dlgList(c("Do nothing - Keep all observations", "Removing cases with missing values","Replace with median","Multiple imputation - Amelia"), 
                        preselect=FALSE, multiple = TRUE, title="Treatment of missing values?")$res}
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
      
      
      if(dial || class(output)!="character"|| any(!output%in% c("default", "Default outputs", "parEst", "Estimated parameters", "parSt", "Standardized parameters","Adjusted covariance matrix", "fitted.cov",
                                                                "Standardized residues", "res.St","res.Unst","Non-standardized residues","vcov","Estimated covariance matrix",
                                                                "AIC", "BIC", "Suitability measures","fitM", "Inspect the starting values", "start", "Inspect the model dies",
                                                                "modmat", "Inspect the representation of the model", "modrep"))==TRUE){
        if(info) writeLines("What results do you want? Warning: the default outputs cannot be saved. If you want a rescue, choose the detail")
        output<-c( "Default outputs", "Estimated parameters", "Standardized parameters","Adjusted covariance matrix", 
                   "Standardized residues", "Non-standardized residues","Estimated covariance matrix","AIC", "BIC", "Suitability measures", 
                   "Inspect the starting values",  "Inspect the model dies", "Inspect the representation of the model")
        if(info) writeLines("What output results do you want?")
        output<- dlgList(output, preselect="Default outputs", multiple = TRUE, title="Outputs of results?")$res
        if(is.null( Resultats$opt.list)) {
          Resultats<-cfa.in()
          return(Resultats)
        }
      }
      
      
      if(dial || length(sauvegarde)!=1 || !is.logical(sauvegarde)){
        sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title="Do you want to save the results?")$res
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
      if(class(fit)=="try-error") {msgBox("We were unable to complete the scan correctly. Please try to respecify the parameters")
        return(ez.cfa())}
      
      if(any(output== "default") | any(output== "Default outputs"))  {
        print(summary(fit, fit.measures = TRUE, standardized=T))
        Resultats<-"To display the results, please use summary (modele.cfa)"
        summary(fit)->>fit
        if(length(output)==1) fit->>modele.cfa
        }
      if(any(output== "parEst") | any(output=="Estimated parameters")) parameterEstimates(fit)->Resultats$"Non-standard estimated parameters"
      if(any(output== "parSt") | any(output=="Standardized parameters")) standardizedSolution(fit)->Resultats$"Standardized estimated parameters"
      if(any(output== "Adjusted covariance matrix") | any(output=="fitted.cov")) fitted(fit)->Resultats$"Adjusted covariance matrix"
      if(any(output== "Standardized residues") | any(output=="res.St")) resid(fit, type="standardized")->Resultats$"Standardized residues"
      if(any(output== "Non-standardized residues") | any(output=="res.Unst")) resid(fit)->Resultats$"Non-standardized residues"
      if(any(output== "vcov") | any(output=="Estimated covariance matrix")) vcov(fit)->Resultat$"Estimated covariance matrix"
      if(any(output== "AIC") ) AIC(fit)->Resultats$AIC
      if(any(output== "BIC") ) BIC(fit)->Resultats$BIC
      if(any(output== "Suitability measures") | any(output=="fitM")) fitMeasures(fit)->Resultats$"Adjustment measure"
      if(any(output== "Inspect the starting values") | any(output=="start"))inspect(fit, what=start)->Resultats$"Starting values"
      if(any(output== "Inspect the model dies") | any(output=="modmat")) inspect(fit)->Resultats$"Model matrices"
      if(any(output== "Inspect the representation of the model") | any(output=="modrep"))inspect(fit, what=list)->Resultats$"Representation of the model"
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
                     group.equal=c("loadings", "intercepts", "means", "thresholds", "regressions", "residuals", "residual.covariances", 
                                   "lv.variances" , "lv.covariances"))
    
    if(!is.null(cfa.options$ord)) paste(cfa.options$ord, collapse="','", sep="")->ord
    paste(cfa.options$output, collapse="','", sep="")->output
    call<-paste0("ez.cfa(modele='", cfa.options$modele, "',data=", cfa.options$nom, ",ord=", ifelse(is.null(cfa.options$ord), "NULL",paste0("c('",ord,"')")),",outlier='", outlier, 
                 "', imp='",imp,"',output=c('", output,"'), save =", cfa.options$sauvegarde, ", mimic='", cfa.options$opt.list$mimic, "'")
    
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
