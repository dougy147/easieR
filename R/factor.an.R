factor.an <-
  function(data=NULL, X=NULL, nF=NULL, rotation="none", methode="ml", sat=0.3, outlier=c("Complete data"),
           imp=NULL, ord=NULL, sauvegarde=FALSE, scor.fac=FALSE,n.boot=1, hier=F, nfact2=1, choix="afe",info=T, html=T){
    
    # data : dataframe
    # X : character. Vector of variable names
    # nF : number of factors
    # rotation : character. One among c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor",
    # "promax",  "oblimin",  "simplimax","bentlerQ", "geominQ","biquartimin", "cluster")
    # methode : character. One among c("ml", "minres" "minchi", "wls","gls","pa")
    # sat : numeric. Level of loading below which loading is not printed. 
    # outlier : one among "Complete data" or "Data without influencing value"
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
      if(dial || is.null(choix) || length(choix)!=1 ||choix %in% c("Exploratory factor analysis","afe",
                                                                   "afc","acp","Confirmatory factor analysis","Principal component analysis")==FALSE){
        dial<-T  
        if(info) writeLines("Please choose the analysis you want to perform.")
        dlgList(c("Exploratory factor analysis", 
                  "Confirmatory factor analysis",
                  "Principal component analysis"), preselect=NULL, multiple = FALSE, title="What analysis do you want to perform?")$res->choix
        if(length(choix)==0) return(NULL)
        if(choix=="Confirmatory factor analysis") return(ez.cfa())
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
      if(choix=="fa" | choix=="Exploratory factor analysis") msg3<-"Please choose the variables you want to analyze. You must choose at least 5 variables" else{
        msg3<-"Please choose the variables you want to analyze. You must choose at least 3 variables"
      }
      
      X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title="Variables", out=NULL)
      data<-X$data
      X<-X$X
      if(is.null(X) || length(X)<3) {
        Resultats<-fa.in()
        return(Resultats)}
      
      
      
      if(dial || length(outlier)>1 || outlier %in% c("Complete data", "Data without influencing value") ==FALSE){
        if(info) writeLines("Do you want the analysis on the complete data or on the data for which the influencing values have been removed?")
        if(info) writeLines("influencing values are identified based on the Mahalanobis distance with a chi threshold of 0.001")
        outlier<- dlgList(c("Complete data", "Data without influencing value"), preselect="Complete data",multiple = FALSE, title="What results do you want to achieve?")$res
        if(length(outlier)==0) { Resultats<-fa.in()
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
        
        if(length(type)==0) {Resultats<-fa.in()
        return(Resultats)}
      } else{if(is.null(ord)) type<-"continues" else type<-"dichotomiques/ordinales"
      }
      
      
      if(type=="continues"){ methode<-c("ml")
      cor<-"cor"
      Matrice<-corr.test(data[,X], method="pearson")$r }else {
        cor<-"poly"
        methode<-c("minres")
        if(type=="mixte") {cor<-"mixed"
        if(info) writeLines("Please specify ordinal variables?") 
        ord<-dlgList(X, multiple = TRUE, title="Ordinal variables?")$res
        if(length(ord)==0) {Resultats<-fa.in()
        return(Resultats)}
        }else ord<-X
        Matrice<-try(tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor',imp=imp, html=F)[[1]],silent=T)
        if(all(class(Matrice)!="matrix")) {
          sortie<-dlgMessage("The correlation matrix could not be performed. Do you want to try again?", type="yesno")$res
          if(sortie=="yes") return(NULL) else Matrice<-try(tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp="rm")[[1]],silent=T)
          if(class(Matrix)=="try-error")  {Matrice<-corr.test(data[,X], method="Spearman")$r
          msgBox("Polychoric correlations have failed. The correlations used are Spearman's rho")}
        }
      }    
      
      Matrice1 <- mat.sort(Matrice)
      if(length(X)>30) numbers<-F else numbers<-T
      try(cor.plot(Matrice1, show.legend=FALSE, main="Correlation matrix used for AFE", labels=NULL, n.legend=0, MAR=TRUE, numbers=numbers,cex=1), silent=T)
      round(Matrice,3)->Resultats$"Correlation matrices"
      round(unlist(cortest.bartlett(data[,X])),4)->bartlett
      names(bartlett)<-c("chi.carre","p-value","dof")
      ### doit etre significatif (attention depend de la taille de l echantillon)
      bartlett->Resultats$"Matrix adequacy measure"$"Barlett test"
      KMO1<-KMO(Matrice)
      if(is.na(KMO1)) {msgBox("The KMO on the matrix could not be obtained. We try to perform a smoothing of the correlation matrix")
        Matrice<-cor.smooth(Matrice)
        KMO1<-KMO(Matrice)}
      if(is.na(KMO1)) {
        msgBox("The KMO on the matrix could not be obtained.")
        Resultats$"Matrix adequacy measure"$"Global Kaiser-Meyer-Olkin Index"<-"The KMO could not be calculated. Check your correlation matrix."
      } else {
        round(KMO1$MSA,3)->Resultats$"Matrix adequacy measure"$"Global Kaiser-Meyer-Olkin Index" ### doit etre superieur a 0.5 sinon la matrice ne convient pas pour analyse factorielle. Dans lÃÂÃÂideal, avoir au moins 0.8. Si des X presentent un KMO<0.5, on peut envisager de les supprimer. 
        round(KMO1$MSAi,3)->Resultats$"Matrix adequacy measure"$'Indice de Kaiser-Meyer-Olkin par item'
        round(det(Matrice),5)->Resultats$"Matrix adequacy measure"$"Determinant of the correlation matrix"
        Resultats$"Matrix adequacy measure"$"Determinant of the correlation matrix: information"<-"risk of multicollinearity if the determinant of the matrix is less than 0.00001"
      }
      
      
      if(dial){
        print(Resultats$"Matrix adequacy measure")
        print("the KMO must absolutely be greater than 0.5")
        cat ("Appuyez sur [entree] pour continuer")
        line <- readline()  
        dlgMessage(c("Is the matrix satisfactory for an AFE?", "Carry on ?"), "okcancel")$res->res.kmo
        if(res.kmo=="cancel") {print("you left AFE")
          return(analyse())}
      }
      
      
      if(dial || length(methode)>1 || is.null(methode) || methode%in%c("minres","wls","gls","pa", "ml","minchi")==FALSE){
        if(info) writeLines("Pour les variables ordinales et dichomiques, preferez la methode du minimum des residus - minres -
                            ou des moindres carres ponderes - wls. Pour les variables continues, le maximum de vraisemblance si la normalite est respectee - ml")
        methode<-dlgList(c("minres","wls","gls","pa", "ml","minchi"), preselect= methode, multiple = FALSE, title="Which algorithm would you like?")$res
        if(length(methode)==0) {Resultats<-fa.in()
        return(Resultats)}
        
      }
      
      eigen(Matrice)$values->eigen
      parallel(length(data[,1]), length(X), 100)->P1
      nScree(x =eigen, aparallel=P1$eigen$mevpea)->result
      result->Resultats$"parallel analyzes"
      plotnScree(result)
      if(dial | is.null(nF) | !is.numeric(nF)) {
        msgBox(paste("the number of factors to be retained according to the parallel analysis is",result$Components$nparallel, "factors." ))
        cat ("Appuyez sur [entree] pour continuer")
        line <- readline() 
        nF<-NA
        while(!is.numeric(nF)) {
          writeLines("Please specify the number of factors.") 
          nF <- dlgInput("Number of factors?", 2)$res
          if(length(nF)==0) {Resultats<-fa.in()
          return(Resultats)
          }
          strsplit(nF, ":")->nF
          tail(nF[[1]],n=1)->nF
          as.numeric(nF)->nF
          if(any((nF%%1==0)%in% c(FALSE, NA))|| nF<0 || nF>(length(X)/2) ){
            msgBox("The number of factors must be a positive integer less than the number of variables")
            nF<-NA
          }
        }
      }
      
      
      
      if(dial & nF>1 || (length(rotation)>1 | rotation %in% c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor",
                                                              "promax",  "oblimin",  "simplimax","bentlerQ", "geominQ","biquartimin", "cluster")==FALSE)){
        if(choix=="acp" | choix=="Principal component analysis") rotation<- c("none", "varimax", "quartimax", "promax",  "oblimin",  "simplimax","cluster") else{
          rotation<-c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor", "promax",  "oblimin",  
                      "simplimax","bentlerQ", "geominQ","biquartimin", "cluster")
        }
        writeLines("Please choose the type of rotation. Oblimin is adapted in humanities")
        rotation<-dlgList(rotation, preselect= "oblimin", multiple = FALSE, title="What a rotation")$res
        if(length(rotation)==0) {Resultats<-fa.in()
        return(Resultats)}
      }
      if(dial | !is.logical(scor.fac)){
        writeLines("Do you want factor scores included in your data?")
        dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Factor scores?")$res->scor.fac
        if(length(scor.fac)==0) {Resultats<-fa.in()
        return(Resultats)}
      }
      
      if(!is.numeric(sat) || sat>1 || sat<0 || is.null(sat)){
        sat<-NULL 
      }
      while(is.null(sat)){
        if(info)  writeLines("Le critere de saturation permet de n'afficher dans le tableau de resultats 
                             que les saturation superieure au seuil fixe")
        sat <- dlgInput("What is the saturation criterion you want to use?", 0.3)$res
        
        if(length(sat)==0) {Resultats<-fa.in()
        return(Resultats)  }
        strsplit(sat, ":")->sat
        tail(sat[[1]],n=1)->sat
        as.numeric(sat)->sat
        if(is.na(sat)) {sat<-NULL
        msgBox("The saturation criterion must be between 0 and 1.") }
      }
      
      
      
      if(choix=="Exploratory factor analysis") {  
        if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
          msgBox("The number of bootstrap must be a positive integer") 
          n.boot<-NULL
        }
        while(is.null(n.boot)){
          writeLines("Please specify the number of bootstrap. To not have a bootstrap, choose 1")
          n.boot<-dlgInput("Number of bootstrap?", 1)$res
          if(length(n.boot)==0) {Resultats<-fa.in()
          return(Resultats)}
          strsplit(n.boot, ":")->n.boot
          tail(n.boot[[1]],n=1)->n.boot
          as.numeric(n.boot)->n.boot
          if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
            msgBox("The number of bootstrap must be a positive integer") 
            n.boot<-NULL
          }
        }
        if(dial & nF>1 & methode!="pa" & rotation%in%c("oblimin","simplimax", "promax") || hier==T && nFact2>=nF/2){
          if(info) writeLines(" Desirez-vous tester une structure hierarchique ? L'omega teste une structure hierarchique et une AFE hierarchique seront realisees.")
          dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Should a hierarchical analysis be carried out?")$res->hier
          if(length(hier)==0) {Resultats<-fa.in()
          return(Resultats)  
          }
          if(!is.null(hier) && hier==TRUE){
            nfact2<-NA
            while(!is.numeric(nfact2)) {
              nfact2<-NA
              writeLines("Please specify the number of factors of the hierarchical structure.") 
              nfact2 <- dlgInput("Number of higher level factors?", 1)$res
              if(length(nfact2)==0) {Resultats<-fa.in()
              return(Resultats)
              }
              strsplit(nfact2, ":")->nfact2
              tail(nfact2[[1]],n=1)->nfact2
              as.numeric(nfact2)->nfact2
              if(any(nfact2%%1==0 %in% c(FALSE, NA))|| nfact2<0 || nfact2>=nF/2 ){
                msgBox("The number of factors must be a positive integer less than the number of factors")
                nfact2<-NA
              }
            }
            
          }
        }
      }
      
      
      
      if(dial | !is.logical(sauvegarde)){
        if(info) writeLines("Do you want to save the results in an external file?")
        dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Do you want to save?")$res->sauvegarde
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
      
      if( cor=="cor") { Resultats$"Multivariate normalcy"<-.normalite(data, X)} else cor<-"mixed"
      if(n.boot==1) {
        FA.results<-fa(Matrice,nfactors= nF, n.obs=length(data[,1]),fm=methode, rotate=rotation, n.iter=1) # realise l AFE
      } else {
        FA.results<-try(fa(data[,X], nfactors= nF, fm=method, rotate=rotation, n.iter=n.boot, cor=cor), silent=T)
        if(class(FA.results)=="try-error") { 
          msgBox("The model could not converge. The parameters have been adapted to allow the model to converge")
          FA.results<-try(fa(data[,X], nfactors= nF, fm=methode, rotate=rotation, n.iter=1, cor="cor", SMC=F), silent=T)
          if(class(FA.results)=="try-error"){
            msgBox("We have not succeeded in making the model converge. Please check your correlation matrix and try again with other parameters")
            return(analyse())}
        }
      }
      
      
      Resultats<-list()
      Resultats$analyse<-paste("factorial analysis using the function fa of the package psych with the method", FA.results$fm)
      if(rotation=="none") Resultats$rotation<-"there is no rotation" else Resultats$rotation<-paste("rotation is rotation", rotation)
      FA.results<-fa.sort(FA.results,polar=FALSE)
      loadfa<-round(as(FA.results$loadings, "matrix"),3)
      loadfa[which(abs(loadfa)<sat)]<-" "
      data.frame("communality"=round(FA.results$communality,3),
                 "specifite"=round(FA.results$uniquenesses,3),
                 "complexity"=round(FA.results$complexity,2))->communaute
      Resultats$"standardized saturations based on the correlation matrix"<-data.frame(loadfa, communaute)
      
      var.ex <- round(FA.results$Vaccounted,3)
      if(nF>1){dimnames(var.ex)[[1]]<-c("Sum of saturation squares", "proportion of variance explained",
                                        "cumulative explained variance proportion", "Proportion of explanation", 
                                        "Cumulative proportion of the explanation")} else {
                                          dimnames(var.ex)[[1]]<-c("Sum of saturation squares", "proportion of variance explained")
                                        }
      Resultats$"Variance explained"<-var.ex
      
      paste("ML",1:nF)->noms1
      if(nF>1 & rotation=="oblimin"){
        round(FA.results$Phi, 3)->cor.f
        Resultats$"correlations between factors"<-cor.f}
      paste("the average complexity is", round(mean(FA.results$complexity),3), "This tests whether", nF, "factors suffice" )-> Resultats$"Medium complexity"
      if(length(X)>5){
        round(matrix(c(FA.results$null.chisq, FA.results$null.dof,FA.results$null.model,
                       FA.results$dof, FA.results$objective, FA.results$RMSEA,
                       FA.results$TLI,FA.results$BIC, FA.results$SABIC,FA.results$rms, FA.results$crms, FA.results$fit.off, 
                       FA.results$chi, FA.results$EPVAL, FA.results$STATISTIC, FA.results$PVAL, FA.results$n.obs), ncol=1),4)->stats
        c("chi square of the null model", "Degrees of freedom of the null model", "objective function of the null model",
          "degrees of freedom of the model", "objective function of the model", "RMSEA", "lower limit of RMSEA", "upper limit of RMSEA",
          "Confidence threshold (1- alpha)", "Tucker Lewis Reliability Factor - TLI", "BIC", "EBIC", 
          "RMSR", "RMSR corrects", "Matching based on values outside the diagonal", "empirical chi square", "value of the proability of the empirical square footage",
          "chi-square of maximum likelihood", "value of the chi-square probability of the maximum likelihood", "total number of observations")->dimnames(stats)[[1]]
        
        "values"->dimnames(stats)[[2]]
        stats->Resultats$"Suitability and adjustment indices"
        if(all(FA.results$R2<1)){
          round(rbind((FA.results$R2)^0.5,FA.results$R2,2*FA.results$R2-1),2)->stats
          dimnames(stats)[[1]]<-c("Correlations of scores with factors", "R multiple square of scores with factors",
                                  "Minimum possible correlation of scores with factors")
          dimnames(stats)[[2]]<-noms1
          stats->Resultats$"Correlations of scores with factors" 
        }
        
        if(n.boot>1) {
          IC<-c()
          for(i in 1:nF){
            cbind(round(FA.results$cis$ci[,i],3), 
                  round(as(FA.results$loadings, "matrix"),3)[,i],
                  round(FA.results$cis$ci[,i+nF],3))->IC2
            dimnames(IC2)[[2]]<-c("inf.lim", dimnames(FA.results$loadings)[[2]][i],"sup.lim")
            cbind(IC, IC2)->IC
          }
          IC->Resultats$"Confidence interval of saturations based on bootstrap - may be biased in the presence of Heyhood case"
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
      names(data)[(length(data)+1-nF):length(data)]<-paste0("factor.", 1:nF)
      assign(nom, data,envir=.GlobalEnv)
      
      }
      
      if(hier) {
        if(cor!="cor") poly<-TRUE else poly<-FALSE
        Resultats$"Hierarchical factor analysis"$Omega<-psych::omega(data[,X], nfactors=nF, n.iter=n.boot,fm=methode, poly=poly, flip=T, digits=3, sl=T, plot=T, n.obs=length(data[,1]), rotate=rotation)
        multi<-fa.multi(Matrice, nfactors=nF, nfact2=nfact2, n.iter=1,fm=methode, n.obs=length(data[,1]), rotate=rotation)
        multi$f2->FA.results
        
        FA.results<-fa.sort(FA.results,polar=FALSE)
        loadfa<-round(as(FA.results$loadings, "matrix"),3)
        loadfa[which(abs(loadfa)<sat)]<-" "
        data.frame("communality"=round(FA.results$communality,3),
                   "specifite"=round(FA.results$uniquenesses,3),
                   "complexity"=round(FA.results$complexity,2))->communaute
        Resultats$"Hierarchical factor analysis"$"standardized saturations based on the correlation matrix"<-data.frame(loadfa, communaute)
        
        var.ex <- round(FA.results$Vaccounted,3)
        if(nfact2>1){dimnames(var.ex)[[1]]<-c("Sum of saturation squares", "proportion of variance explained",
                                              "cumulative explained variance proportion", "Proportion of explanation", 
                                              "Cumulative proportion of the explanation")} else {
                                                dimnames(var.ex)[[1]]<-c("Sum of saturation squares", "proportion of variance explained")
                                              }
        Resultats$"Hierarchical factor analysis"$"Variance explained"<-var.ex
        
        paste("ML",1:nfact2)->noms1
        
        paste("the average complexity is", round(mean(FA.results$complexity),3), "This tests whether", nF, "factors suffice" )-> Resultats$"Medium complexity"
        
        round(matrix(c( FA.results$null.dof,FA.results$null.model,
                        FA.results$dof, FA.results$objective, 
                        FA.results$rms, FA.results$fit.off), ncol=1),4)->stats
        c( "Degrees of freedom of the null model", "objective function of the null model",
           "degrees of freedom of the model", "objective function of the model",    "RMSR", 
           "Matching based on values outside the diagonal")->dimnames(stats)[[1]]
        
        "values"->dimnames(stats)[[2]]
        stats->Resultats$"Hierarchical factor analysis"$"Suitability and adjustment indices"
        if(all(FA.results$R2<1)){
          round(rbind((FA.results$R2)^0.5,FA.results$R2,2*FA.results$R2-1),2)->stats
          dimnames(stats)[[1]]<-c("Correlations of scores with factors", "R multiple square of scores with factors",
                                  "Minimum possible correlation of scores with factors")
          dimnames(stats)[[2]]<-noms1
          stats->Resultats$"Hierarchical factor analysis"$"Correlations of scores with factors"
          fa.multi.diagram(multi)
        }
      }
      return(Resultats)
      
    } 
    acp.out<-function(Matrice, data, X, nF, methode, rotation, sat, scor.fac, nom){
      principal(Matrice, nfactors= nF, n.obs=length(data[,1]), rotate=rotation)->PCA
      list()->Resultats
      Resultats$analyse<-paste("analyse en composante principale en utilisant la fonction [principal] du package psych, l'algorithme est", PCA$fm)
      if(!is.null(rotation)) Resultats$rotation<-paste("rotation is rotation", rotation) 
      
      PCA<-fa.sort(PCA,polar=FALSE)
      loadfa<-round(as(PCA$loadings, "matrix"),3)
      loadfa[which(abs(loadfa)<sat)]<-" " 
      data.frame("communality"=round(PCA$communality,3),
                 "specifite"=round(PCA$uniquenesses,3),
                 "complexity"=round(PCA$complexity,2))->communaute
      Resultats$"standardized saturations based on the correlation matrix"<-data.frame(loadfa, communaute)
      var.ex<-round(PCA$Vaccounted,3)
      
      if(nF>1){dimnames(var.ex)[[1]]<-c("Sum of saturation squares", "proportion of variance explained",
                                        "cumulative explained variance proportion", "Proportion of explanation", 
                                        "Cumulative proportion of the explanation")} else {
                                          dimnames(var.ex)[[1]]<-c("Sum of saturation squares", "proportion of variance explained")
                                        }
      Resultats$"Variance explained"<-var.ex
      
      paste("TC",1:nF)->noms1
      if(nF>1 & rotation=="oblimin"){  round(PCA$r.scores,3)->cor.f
        Resultats$"correlations between factors"<-cor.f}
      paste("the average complexity is", mean(PCA$complexity), "This tests whether", nF, "factors suffice" )-> Resultats$"Medium complexity"
      round(matrix(c(PCA$null.dof,PCA$null.model,
                     PCA$dof, PCA$objective, 
                     PCA$rms, PCA$fit.off, 
                     PCA$chi, PCA$EPVAL, PCA$STATISTIC, PCA$PVAL, PCA$n.obs), ncol=1),4)->stats
      
      
      c("Degrees of freedom of the null model", "objective function of the null model","degrees of freedom of the model", "objective function of the model",
        "RMSR",  "Matching based on values outside the diagonal", "empirical chi square", "value of the probability of the empirical chi-square",
        "chi-square of maximum likelihood", "value of the chi-square probability of the maximum likelihood", "total number of observations")->dimnames(stats)[[1]]
      
      "values"->dimnames(stats)[[2]]
      stats->Resultats$"Suitability and adjustment indices"
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
        names(data)[(length(data)+1-nF):length(data)]<-paste0("factor.", 1:nF)
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
    Resultats$"Correlation matrices"<-fa.options$"Correlation matrices"
    Resultats$"Matrix adequacy measure"<-fa.options$"Matrix adequacy measure"
    Resultats$"parallel analyzes"<-fa.options$"parallel analyzes"
    
    
    
    if(fa.options$choix==  "Exploratory factor analysis" |choix=="afe"){
      Resultats$"Factor analysis"<-fa.out(Matrice=Matrice, data=data, X=X, nF=nF, methode=methode, rotation=rotation, sat=sat, 
                                              scor.fac=scor.fac, n.boot=n.boot, nom=nom, hier=hier, cor=cor, nfact2=nfact2)  }
    
    if(fa.options$choix==  "Principal component analysis" |choix=="acp"){
      Resultats$"Principal component analysis"<-acp.out(Matrice=Matrice, data=data, X=X, nF=nF, methode=methode, rotation=rotation, sat=sat, scor.fac=scor.fac, nom=nom)
    }
    
    
    paste(X, collapse="','", sep="")->X
    if(!is.null(fa.options$ord)) paste(fa.options$ord, collapse="','", sep="")->ord
    Resultats$Call<-paste0("factor.an(data=", nom, ",X=c('",X, "'),nF=", nF,", rotation='", rotation, "',methode='",methode, "',sat=", sat,
                           ",outlier='", outlier, "',imp=", ifelse(is.null(imp), "NULL", paste0("'",imp,"'")),",ord=", ifelse(!is.null(ord), paste0("c('", ord,"')"), "NULL"),
                           ", backup =", sauvegarde, ",scor.fac=", scor.fac, ",n.boot=", n.boot,",hier=", hier, ",nfact2=", nfact2, ", choice = '", fa.options$choix, "',info=T)"
    )
    
    
    .add.history(data=data, command=Resultats$Call, nom=nom)
    .add.result(Resultats=Resultats, name =paste(fa.options$choix, Sys.time() ))
    
    
    if(fa.options$sauvegarde) save(Resultats=Resultats, choix=fa.options$choix, env=.e)
    ref1(packages)->Resultats$"References of the packages used for this analysis"
    if(html) ez.html(Resultats)
    return(Resultats)
    
    }
