factor.an <-
  function(data=NULL, X=NULL, nF=NULL, rotation="none", methode="ml", sat=0.3, outlier=c("Donnees completes"),
           imp=NULL, ord=NULL, sauvegarde=FALSE, scor.fac=FALSE,n.boot=1, hier=F, nfact2=1, choix="afe",info=T, html=T){
    
    # data : dataframe
    # X : character. Vector of variable names
    # nF : number of factors
    # rotation : character. One among c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor",
    # "promax",  "oblimin",  "simplimax","bentlerQ", "geominQ","biquartimin", "cluster")
    # methode : character. One among c("ml", "minres" "minchi", "wls","gls","pa")
    # sat : numeric. Level of loading below which loading is not printed. 
    # outlier : one among "Donnees completes" or "Donnees sans valeur influente"
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
      if(dial || is.null(choix) || length(choix)!=1 ||choix %in% c("Analyse factorielle exploratoire","afe",
                                                                   "afc","acp","Analyse factorielle confirmatoire","Analyse en composante principale")==FALSE){
        dial<-T  
        if(info) writeLines("Veuillez choisir l'analyse que vous desirez realiser.")
        dlgList(c("Analyse factorielle exploratoire", 
                  "Analyse factorielle confirmatoire",
                  "Analyse en composante principale"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez vous realiser?")$res->choix
        if(length(choix)==0) return(NULL)
        if(choix=="Analyse factorielle confirmatoire") return(ez.cfa())
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
      if(choix=="fa" | choix=="Analyse factorielle exploratoire") msg3<-"Veuillez choisir les variables que vous desirez analyser. Vous devez choisir au moins 5 variables" else{
        msg3<-"Veuillez choisir les variables que vous desirez analyser. Vous devez choisir au moins 3 variables"
      }
      
      X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title="Variables", out=NULL)
      data<-X$data
      X<-X$X
      if(is.null(X) || length(X)<3) {
        Resultats<-fa.in()
        return(Resultats)}
      
      
      
      if(dial || length(outlier)>1 || outlier %in% c("Donnees completes", "Donnees sans valeur influente") ==FALSE){
        if(info) writeLines("Desirez-vous l'analyse sur les donnees completes ou sur les donnees pour lesquelles les valeurs influentes ont ete enlevees ?")
        if(info) writeLines("les valeurs influentes sont identifiees sur la base de la distance de Mahalanobis avec un seuil du chi a 0.001")
        outlier<- dlgList(c("Donnees completes", "Donnees sans valeur influente"), preselect="Donnees completes",multiple = FALSE, title="Quels resultats voulez-vous obtenir ?")$res
        if(length(outlier)==0) { Resultats<-fa.in()
        return(Resultats)}
      }
      
      if(outlier=="Donnees sans valeur influente"){
        inf<-VI.multiples(data,X)
        Resultats$"Valeurs considerees comme influentes"<-inf$"Valeurs considerees comme influentes"
        data<-inf$data
      }
      
      
      
      if(dial){
        if(info) writeLines("Veuillez preciser le type de variables. Des correlations tetra/polychoriques seront realisees sur les variables dichotomiques/ordinales et Bravais-Pearson sur les variables continues")
        if(length(unique(unlist(data[,X])))<9) {type<-dlgList(c("dichotomiques/ordinales","continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res}else {
          type<-dlgList(c("continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res 
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
        if(info) writeLines("Veuillez preciser les variables ordinales ?") 
        ord<-dlgList(X, multiple = TRUE, title="Variables ordinales ?")$res
        if(length(ord)==0) {Resultats<-fa.in()
        return(Resultats)}
        }else ord<-X
        Matrice<-try(tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor',imp=imp, html=F)[[1]],silent=T)
        if(all(class(Matrice)!="matrix")) {
          sortie<-dlgMessage("La matrice de correlation n'a pu etre realisee. Voulez-vous reessayer ?", type="yesno")$res
          if(sortie=="yes") return(NULL) else Matrice<-try(tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp="rm")[[1]],silent=T)
          if(class(Matrix)=="try-error")  {Matrice<-corr.test(data[,X], method="Spearman")$r
          msgBox("Les correlations polychoriques ont echoue. Les correlations utilisees sont des rho de Spearman")}
        }
      }    
      
      Matrice1 <- mat.sort(Matrice)
      if(length(X)>30) numbers<-F else numbers<-T
      try(cor.plot(Matrice1, show.legend=FALSE, main="Matrice de correlation utilisee pour AFE", labels=NULL, n.legend=0, MAR=TRUE, numbers=numbers,cex=1), silent=T)
      round(Matrice,3)->Resultats$"Matrice de correlations"
      round(unlist(cortest.bartlett(data[,X])),4)->bartlett
      names(bartlett)<-c("chi.carre","valeur.p","ddl")
      ### doit etre significatif (attention depend de la taille de l echantillon)
      bartlett->Resultats$"Mesure d'adequation de la matrice"$"Test de Barlett"
      KMO1<-KMO(Matrice)
      if(is.na(KMO1)) {msgBox("Le KMO sur la matrice n'a pu etre obtenu. Nous tentons de realiser un lissage de la matrice de correlation")
        Matrice<-cor.smooth(Matrice)
        KMO1<-KMO(Matrice)}
      if(is.na(KMO1)) {
        msgBox("Le KMO sur la matrice n'a pu etre obtenu.")
        Resultats$"Mesure d'adequation de la matrice"$"Indice de Kaiser-Meyer-Olkin global"<-"Le KMO n'a pas pu etre calcule. Verifiez votre matrice de correlation."
      } else {
        round(KMO1$MSA,3)->Resultats$"Mesure d'adequation de la matrice"$"Indice de Kaiser-Meyer-Olkin global" ### doit etre superieur a 0.5 sinon la matrice ne convient pas pour analyse factorielle. Dans lÃÂÃÂideal, avoir au moins 0.8. Si des X presentent un KMO<0.5, on peut envisager de les supprimer. 
        round(KMO1$MSAi,3)->Resultats$"Mesure d'adequation de la matrice"$'Indice de Kaiser-Meyer-Olkin par item'
        round(det(Matrice),5)->Resultats$"Mesure d'adequation de la matrice"$"Determinant de la matrice de correlation"
        Resultats$"Mesure d'adequation de la matrice"$"Determinant de la matrice de correlations : information"<-"risque de multicolinearite si le determinant de la matrice est inferieur a 0.00001"
      }
      
      
      if(dial){
        print(Resultats$"Mesure d'adequation de la matrice")
        print("le KMO doit absolument etre superieur a 0.5")
        cat ("Appuyez sur [entree] pour continuer")
        line <- readline()  
        dlgMessage(c("La matrice est-elle satisfaisante pour une AFE ?", "Continuer ?"), "okcancel")$res->res.kmo
        if(res.kmo=="cancel") {print("vous avez quitte l'AFE")
          return(analyse())}
      }
      
      
      if(dial || length(methode)>1 || is.null(methode) || methode%in%c("minres","wls","gls","pa", "ml","minchi")==FALSE){
        if(info) writeLines("Pour les variables ordinales et dichomiques, preferez la methode du minimum des residus - minres -
                            ou des moindres carres ponderes - wls. Pour les variables continues, le maximum de vraisemblance si la normalite est respectee - ml")
        methode<-dlgList(c("minres","wls","gls","pa", "ml","minchi"), preselect= methode, multiple = FALSE, title="Quel algorithme desirez-vous?")$res
        if(length(methode)==0) {Resultats<-fa.in()
        return(Resultats)}
        
      }
      
      eigen(Matrice)$values->eigen
      parallel(length(data[,1]), length(X), 100)->P1
      nScree(x =eigen, aparallel=P1$eigen$mevpea)->result
      result->Resultats$"analyses paralleles"
      plotnScree(result)
      if(dial | is.null(nF) | !is.numeric(nF)) {
        msgBox(paste("le nombre de facteurs a retenir selon l'analyse en parallele est de",result$Components$nparallel, "facteurs." ))
        cat ("Appuyez sur [entree] pour continuer")
        line <- readline() 
        nF<-NA
        while(!is.numeric(nF)) {
          writeLines("Veuillez preciser le nombre de facteurs.") 
          nF <- dlgInput("Nombre de facteurs ?", 2)$res
          if(length(nF)==0) {Resultats<-fa.in()
          return(Resultats)
          }
          strsplit(nF, ":")->nF
          tail(nF[[1]],n=1)->nF
          as.numeric(nF)->nF
          if(any((nF%%1==0)%in% c(FALSE, NA))|| nF<0 || nF>(length(X)/2) ){
            msgBox("Le nombre de facteur doit etre un entier positif inferieur au nombre de variables")
            nF<-NA
          }
        }
      }
      
      
      
      if(dial & nF>1 || (length(rotation)>1 | rotation %in% c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor",
                                                              "promax",  "oblimin",  "simplimax","bentlerQ", "geominQ","biquartimin", "cluster")==FALSE)){
        if(choix=="acp" | choix=="Analyse en composante principale") rotation<- c("none", "varimax", "quartimax", "promax",  "oblimin",  "simplimax","cluster") else{
          rotation<-c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor", "promax",  "oblimin",  
                      "simplimax","bentlerQ", "geominQ","biquartimin", "cluster")
        }
        writeLines("Veuillez choisir le type de rotation. Oblimin est adapte en sciences humaines")
        rotation<-dlgList(rotation, preselect= "oblimin", multiple = FALSE, title="Quelle rotation")$res
        if(length(rotation)==0) {Resultats<-fa.in()
        return(Resultats)}
      }
      if(dial | !is.logical(scor.fac)){
        writeLines("Voulez-vous que les scores factoriels soient integres a vos donnees ?")
        dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Scores factoriels?")$res->scor.fac
        if(length(scor.fac)==0) {Resultats<-fa.in()
        return(Resultats)}
      }
      
      if(!is.numeric(sat) || sat>1 || sat<0 || is.null(sat)){
        sat<-NULL 
      }
      while(is.null(sat)){
        if(info)  writeLines("Le critere de saturation permet de n'afficher dans le tableau de resultats 
                             que les saturation superieure au seuil fixe")
        sat <- dlgInput("Quel est le critere de saturation que vous voulez utiliser ?", 0.3)$res
        
        if(length(sat)==0) {Resultats<-fa.in()
        return(Resultats)  }
        strsplit(sat, ":")->sat
        tail(sat[[1]],n=1)->sat
        as.numeric(sat)->sat
        if(is.na(sat)) {sat<-NULL
        msgBox("Le critere de saturation doit etre compris entre 0 et 1.") }
      }
      
      
      
      if(choix=="Analyse factorielle exploratoire") {  
        if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
          msgBox("Le nombre de bootstrap doit etre un nombre entier positif") 
          n.boot<-NULL
        }
        while(is.null(n.boot)){
          writeLines("Veuillez preciser le nombre de bootstrap. Pour ne pas avoir de bootstrap, choisir 1")
          n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
          if(length(n.boot)==0) {Resultats<-fa.in()
          return(Resultats)}
          strsplit(n.boot, ":")->n.boot
          tail(n.boot[[1]],n=1)->n.boot
          as.numeric(n.boot)->n.boot
          if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
            msgBox("Le nombre de bootstrap doit etre un nombre entier positif") 
            n.boot<-NULL
          }
        }
        if(dial & nF>1 & methode!="pa" & rotation%in%c("oblimin","simplimax", "promax") || hier==T && nFact2>=nF/2){
          if(info) writeLines(" Desirez-vous tester une structure hierarchique ? L'omega teste une structure hierarchique et une AFE hierarchique seront realisees.")
          dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Faut-il realiser une analyse hierarchique ?")$res->hier
          if(length(hier)==0) {Resultats<-fa.in()
          return(Resultats)  
          }
          if(!is.null(hier) && hier==TRUE){
            nfact2<-NA
            while(!is.numeric(nfact2)) {
              nfact2<-NA
              writeLines("Veuillez preciser le nombre de facteurs de la structure hierarchique.") 
              nfact2 <- dlgInput("Nombre de facteurs du niveau superieur ?", 1)$res
              if(length(nfact2)==0) {Resultats<-fa.in()
              return(Resultats)
              }
              strsplit(nfact2, ":")->nfact2
              tail(nfact2[[1]],n=1)->nfact2
              as.numeric(nfact2)->nfact2
              if(any(nfact2%%1==0 %in% c(FALSE, NA))|| nfact2<0 || nfact2>=nF/2 ){
                msgBox("Le nombre de facteur doit etre un entier positif inferieur au nombre de facteurs")
                nfact2<-NA
              }
            }
            
          }
        }
      }
      
      
      
      if(dial | !is.logical(sauvegarde)){
        if(info) writeLines("Desirez-vous sauvegarder les resultats dans un fichier externe ?")
        dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Voulez-vous sauvegarder?")$res->sauvegarde
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
      
      if( cor=="cor") { Resultats$"Normalite multivariee"<-.normalite(data, X)} else cor<-"mixed"
      if(n.boot==1) {
        FA.results<-fa(Matrice,nfactors= nF, n.obs=length(data[,1]),fm=methode, rotate=rotation, n.iter=1) # realise l AFE
      } else {
        FA.results<-try(fa(data[,X], nfactors= nF, fm=method, rotate=rotation, n.iter=n.boot, cor=cor), silent=T)
        if(class(FA.results)=="try-error") { 
          msgBox("Le modele n'a pas pu converger. Les parametres ont ete adaptes pour permettre au modele de converger")
          FA.results<-try(fa(data[,X], nfactors= nF, fm=methode, rotate=rotation, n.iter=1, cor="cor", SMC=F), silent=T)
          if(class(FA.results)=="try-error"){
            msgBox("Nous n'avons pas reussi a faire converger le modele. Veuillez verifier votre matrice de correlations et reessayer avec d'autres parametres")
            return(analyse())}
        }
      }
      
      
      Resultats<-list()
      Resultats$analyse<-paste("analyse factorielle en utilisant la fonction fa du package psych avec la methode", FA.results$fm)
      if(rotation=="none") Resultats$rotation<-"il n'y a pas de rotation" else Resultats$rotation<-paste("la rotation est un rotation", rotation)
      FA.results<-fa.sort(FA.results,polar=FALSE)
      loadfa<-round(as(FA.results$loadings, "matrix"),3)
      loadfa[which(abs(loadfa)<sat)]<-" "
      data.frame("communaute"=round(FA.results$communality,3),
                 "specifite"=round(FA.results$uniquenesses,3),
                 "complexite"=round(FA.results$complexity,2))->communaute
      Resultats$"saturations standardisees basees sur la matrice de correlations"<-data.frame(loadfa, communaute)
      
      var.ex <- round(FA.results$Vaccounted,3)
      if(nF>1){dimnames(var.ex)[[1]]<-c("Sommes des carres des saturations", "proportion de variance expliquee",
                                        "proportion de variance expliquee cumulee", "Proportion de l'explication", 
                                        "Proportion cumulee de l'explication")} else {
                                          dimnames(var.ex)[[1]]<-c("Sommes des carres des saturations", "proportion de variance expliquee")
                                        }
      Resultats$"Variance expliquee"<-var.ex
      
      paste("ML",1:nF)->noms1
      if(nF>1 & rotation=="oblimin"){
        round(FA.results$Phi, 3)->cor.f
        Resultats$"correlations entre facteurs"<-cor.f}
      paste("la complexite moyenne est de", round(mean(FA.results$complexity),3), "Cela teste si", nF, "facteurs suffise(nt)" )-> Resultats$"Complexite moyenne"
      if(length(X)>5){
        round(matrix(c(FA.results$null.chisq, FA.results$null.dof,FA.results$null.model,
                       FA.results$dof, FA.results$objective, FA.results$RMSEA,
                       FA.results$TLI,FA.results$BIC, FA.results$SABIC,FA.results$rms, FA.results$crms, FA.results$fit.off, 
                       FA.results$chi, FA.results$EPVAL, FA.results$STATISTIC, FA.results$PVAL, FA.results$n.obs), ncol=1),4)->stats
        c("chi carre du modele null", "Degres de liberte du modele null", "fonction objective du modele null",
          "degres de liberte du modele", "fonction objective du modele", "RMSEA", "limite inferieure du RMSEA", "limite superieure du RMSEA",
          "Seuil de confiance (1- alpha)", "facteur de fiabilite de Tucker Lewis - TLI", "BIC", "EBIC", 
          "RMSR", "RMSR corrige", "Adequation basee sur les valeurs en dehors de la diagonale", "chi carre empirique", "valeur de la proabilite du chi carre empirique",
          "chi carre du maximum de vraisemblance", "valeur de la probabilite du chi carre du maximum de vraisemblance", "nombre total d'observations")->dimnames(stats)[[1]]
        
        "valeurs"->dimnames(stats)[[2]]
        stats->Resultats$"Indices d'adequation et d'ajustement"
        if(all(FA.results$R2<1)){
          round(rbind((FA.results$R2)^0.5,FA.results$R2,2*FA.results$R2-1),2)->stats
          dimnames(stats)[[1]]<-c("Correlations des scores avec les facteurs", "R carre multiple des scores avec les facteurs",
                                  "Correlation minimale possible des scores avec les facteurs")
          dimnames(stats)[[2]]<-noms1
          stats->Resultats$"Correlations des scores avec les facteurs" 
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
          IC->Resultats$"Intervalle de confiance des saturations sur la base du bootstrap - peut etre biaise en presence de Heyhood case"
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
      names(data)[(length(data)+1-nF):length(data)]<-paste0("facteur.", 1:nF)
      assign(nom, data,envir=.GlobalEnv)
      
      }
      
      if(hier) {
        if(cor!="cor") poly<-TRUE else poly<-FALSE
        Resultats$"Analyse factorielle hierarchique"$Omega<-psych::omega(data[,X], nfactors=nF, n.iter=n.boot,fm=methode, poly=poly, flip=T, digits=3, sl=T, plot=T, n.obs=length(data[,1]), rotate=rotation)
        multi<-fa.multi(Matrice, nfactors=nF, nfact2=nfact2, n.iter=1,fm=methode, n.obs=length(data[,1]), rotate=rotation)
        multi$f2->FA.results
        
        FA.results<-fa.sort(FA.results,polar=FALSE)
        loadfa<-round(as(FA.results$loadings, "matrix"),3)
        loadfa[which(abs(loadfa)<sat)]<-" "
        data.frame("communaute"=round(FA.results$communality,3),
                   "specifite"=round(FA.results$uniquenesses,3),
                   "complexite"=round(FA.results$complexity,2))->communaute
        Resultats$"Analyse factorielle hierarchique"$"saturations standardisees basees sur la matrice de correlations"<-data.frame(loadfa, communaute)
        
        var.ex <- round(FA.results$Vaccounted,3)
        if(nfact2>1){dimnames(var.ex)[[1]]<-c("Sommes des carres des saturations", "proportion de variance expliquee",
                                              "proportion de variance expliquee cumulee", "Proportion de l'explication", 
                                              "Proportion cumulee de l'explication")} else {
                                                dimnames(var.ex)[[1]]<-c("Sommes des carres des saturations", "proportion de variance expliquee")
                                              }
        Resultats$"Analyse factorielle hierarchique"$"Variance expliquee"<-var.ex
        
        paste("ML",1:nfact2)->noms1
        
        paste("la complexite moyenne est de", round(mean(FA.results$complexity),3), "Cela teste si", nF, "facteurs suffise(nt)" )-> Resultats$"Complexite moyenne"
        
        round(matrix(c( FA.results$null.dof,FA.results$null.model,
                        FA.results$dof, FA.results$objective, 
                        FA.results$rms, FA.results$fit.off), ncol=1),4)->stats
        c( "Degres de liberte du modele null", "fonction objective du modele null",
           "degres de liberte du modele", "fonction objective du modele",    "RMSR", 
           "Adequation basee sur les valeurs en dehors de la diagonale")->dimnames(stats)[[1]]
        
        "valeurs"->dimnames(stats)[[2]]
        stats->Resultats$"Analyse factorielle hierarchique"$"Indices d'adequation et d'ajustement"
        if(all(FA.results$R2<1)){
          round(rbind((FA.results$R2)^0.5,FA.results$R2,2*FA.results$R2-1),2)->stats
          dimnames(stats)[[1]]<-c("Correlations des scores avec les facteurs", "R carre multiple des scores avec les facteurs",
                                  "Correlation minimale possible des scores avec les facteurs")
          dimnames(stats)[[2]]<-noms1
          stats->Resultats$"Analyse factorielle hierarchique"$"Correlations des scores avec les facteurs"
          fa.multi.diagram(multi)
        }
      }
      return(Resultats)
      
    } 
    acp.out<-function(Matrice, data, X, nF, methode, rotation, sat, scor.fac, nom){
      principal(Matrice, nfactors= nF, n.obs=length(data[,1]), rotate=rotation)->PCA
      list()->Resultats
      Resultats$analyse<-paste("analyse en composante principale en utilisant la fonction [principal] du package psych, l'algorithme est", PCA$fm)
      if(!is.null(rotation)) Resultats$rotation<-paste("la rotation est un rotation", rotation) 
      
      PCA<-fa.sort(PCA,polar=FALSE)
      loadfa<-round(as(PCA$loadings, "matrix"),3)
      loadfa[which(abs(loadfa)<sat)]<-" " 
      data.frame("communaute"=round(PCA$communality,3),
                 "specifite"=round(PCA$uniquenesses,3),
                 "complexite"=round(PCA$complexity,2))->communaute
      Resultats$"saturations standardisees basees sur la matrice de correlations"<-data.frame(loadfa, communaute)
      var.ex<-round(PCA$Vaccounted,3)
      
      if(nF>1){dimnames(var.ex)[[1]]<-c("Sommes des carres des saturations", "proportion de variance expliquee",
                                        "proportion de variance expliquee cumulee", "Proportion de l'explication", 
                                        "Proportion cumulee de l'explication")} else {
                                          dimnames(var.ex)[[1]]<-c("Sommes des carres des saturations", "proportion de variance expliquee")
                                        }
      Resultats$"Variance expliquee"<-var.ex
      
      paste("TC",1:nF)->noms1
      if(nF>1 & rotation=="oblimin"){  round(PCA$r.scores,3)->cor.f
        Resultats$"correlations entre facteurs"<-cor.f}
      paste("la complexite moyenne est de", mean(PCA$complexity), "Cela teste si", nF, "facteurs suffise(nt)" )-> Resultats$"Complexite moyenne"
      round(matrix(c(PCA$null.dof,PCA$null.model,
                     PCA$dof, PCA$objective, 
                     PCA$rms, PCA$fit.off, 
                     PCA$chi, PCA$EPVAL, PCA$STATISTIC, PCA$PVAL, PCA$n.obs), ncol=1),4)->stats
      
      
      c("Degres de liberte du modele null", "fonction objective du modele null","degres de liberte du modele", "fonction objective du modele",
        "RMSR",  "Adequation basee sur les valeurs en dehors de la diagonale", "chi carre empirique", "valeur de la probabilite du chi carre empirique",
        "chi carre du maximum de vraisemblance", "valeur de la probabilite du chi carre du maximum de vraisemblance", "nombre total d'observations")->dimnames(stats)[[1]]
      
      "valeurs"->dimnames(stats)[[2]]
      stats->Resultats$"Indices d'adequation et d'ajustement"
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
        names(data)[(length(data)+1-nF):length(data)]<-paste0("facteur.", 1:nF)
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
    Resultats$"Matrice de correlations"<-fa.options$"Matrice de correlations"
    Resultats$"Mesure d'adequation de la matrice"<-fa.options$"Mesure d'adequation de la matrice"
    Resultats$"analyses paralleles"<-fa.options$"analyses paralleles"
    
    
    
    if(fa.options$choix==  "Analyse factorielle exploratoire" |choix=="afe"){
      Resultats$"Analyse factorielle"<-fa.out(Matrice=Matrice, data=data, X=X, nF=nF, methode=methode, rotation=rotation, sat=sat, 
                                              scor.fac=scor.fac, n.boot=n.boot, nom=nom, hier=hier, cor=cor, nfact2=nfact2)  }
    
    if(fa.options$choix==  "Analyse en composante principale" |choix=="acp"){
      Resultats$"Analyse en composante principale"<-acp.out(Matrice=Matrice, data=data, X=X, nF=nF, methode=methode, rotation=rotation, sat=sat, scor.fac=scor.fac, nom=nom)
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
    ref1(packages)->Resultats$"References des packages utilises pour cette analyse"
    if(html) ez.html(Resultats)
    return(Resultats)
    
    }
