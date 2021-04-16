corr.matrice <-
  function(X=NULL, Y=NULL, Z=NULL,data=NULL, group=NULL,method="pearson",param=c("H0","FB"), sauvegarde=F, outlier=c("Complete data"),n.boot=1,  rscale=0.354, info=T,
           p.adjust="holm",out.m=2, na.rm=NULL, html=T) { 
    # X : character or vector. First set of variables
    # Y : character or vector. Second set of variables Must be NULL if Z is not
    # Z : character or vector. Names of the variables to control in partial correlation. Must be NULL if Y is not
    # data : dataset
    # group : character or vector. Names of the classifying variables 
    # method : one among c("pearson", "spearman", "kendall") 
    # param :  one or both among "H0" (null hypoethesis testing) et "FB"(bayesian factors)
    # sauvegarde : logical. Must the analyses be saved ? 
    # outlier : One among   c("Complete data", "Data without influencing value")
    # rscale : numeric. If not null, bayesian factors are computed. Can also be "medium", "wide", "ultralarge"
    # info : logical. Must information be displayed in dialog box interface. 
    # correction : character. Probability adjustement. See p.adjust for list of possibilities
    # out.m : 1 for deleting one observation at the time in outlier detection. 2 for all at the same time. 
    # na.rm : character. How to deal with missing values ? 
    # html : Logical. Should output be a HTML page ? 
    
    corr.matrice.in<-function(X=NULL, Y=NULL, Z=NULL, group=NULL, data=NULL, p.adjust="holm", rscale=0.354,sauvegarde=F,outlier="Complete data", info=T, method="pearson", param=c("H0","FB"), n.boot=NULL){
      Resultats<-list()
      if(!is.null(X) & !is.null(data) & (is.null(Y) | is.null(Z))) {dial<-F 
      if(is.null(Z)) choix<-"Correlations" else choix<-"Partial and semi-partial correlations"
      if(!is.null(Y)) carre<-"rectangular" else carre<-"carree"
      }  else {dial<-T
      choix<-NULL}
      
      if(is.null(choix) ){
        if(info) writeLines("Please specify the type of correlation you wish to achieve.")
        choix<-dlgList(c("Correlations", "Partial correlations"), preselect="Correlations", multiple = FALSE, title="Correlations or partial correlations?")$res
        if(length(choix)==0) return(NULL)
      }
      
      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL)
      nom<-data[[1]]
      data<-data[[2]]
      
      if(choix=="Correlations" & dial==T){
        writeLines("Une matrice carree est une matrice avec toutes les Correlations 2 a 2. 
                   Une matrice rectangulaire est une matrice dans laquelle un premier ensemble de variables est mis en correlations avec un second jeu de variables")
        carre<-dlgList(c("carree", "rectangular"), multiple = FALSE, title="die type")$res
        if(length(carre)==0){Resultats<-corr.matrice.in()
        return(Resultats)}
      } else carre<-"carree"
      
      msg3<-"Please choose the first set of variables"
      
      
      X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title="Variables", out=NULL)
      if(is.null(X)) {
        corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.353)->Resultats
        return(Resultats)}
      data<-X$data
      X1<-X$X
      if(carre=="rectangular"){
        msg4<-"Please choose the second set of variables"
        Y<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg4,  multiple=T, title="Second set of variables", out=X1)
        if(is.null(Y)) {
          corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                          n.boot=NULL, rscale=0.353)->Resultats
          return(Resultats)}
        data<-Y$data
        Y<-Y$X 
        
      }
      if(choix=="Partial correlations"){
        msg6<-"Please specify the variable (s) to control" 
        Z<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg6,  multiple=T, title="Variable-s to control", out=c(X1,Y))
        if(is.null(Z)) {
          corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                          n.boot=NULL, rscale=0.353)->Resultats
          return(Resultats)}
        data<-Z$data
        Z<-Z$X 
      }
      
      
      if(dial){
        
        if(info==TRUE) writeLines("Si vous souhaitez realiser l'analyse pour differents sous-echantillons en fonction d'un critere categoriel (i.e., realiser une analyse par groupe)
                                  \n choisissez oui. Dans ce cas, l'analyse est realisee sur l'echantillon complet et sur les sous-echantillons.
                                  \n Si vous desirez l'analyse pour l'echantillon complet uniquement, chosissez non.")
        dlgList(c("Yes", "non"), preselect="non", multiple = FALSE, title="Group analysis?")$res->par.groupe
        if(length(par.groupe)==0) {
          corr.matrice.in(X=NULL, Y=NULL, data=NULL,method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                          n.boot=NULL, rscale=0.353)->Resultats
          return(Resultats)
        } } else par.groupe<-"non"
      msg5<-"Please choose the categorical ranking factor."
      if(par.groupe=="Yes" || !is.null(group)){group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=F, message=msg5,  multiple=TRUE, title="Variable-s", out=c(X1,Y,Z)) 
      if(length(group)==0) {   corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                               n.boot=NULL, rscale=0.353)->Resultats
        return(Resultats)}
      data<-group$data
      group<-group$X 
      if(any(ftable(data[,group])<3)){
        msgBox("Some combinations of modalities have less than 3 observations. You must have at least 3 observations for each combination")
        corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.353)->Resultats
        return(Resultats)
      }
      }
      
      if(dial || length(outlier)>1 || outlier %in% c("Complete data", "Data without influencing value") ==FALSE){
        if(info) writeLines("Do you want the analysis on the complete data or on the data for which the influencing values have been removed?")
        outlier<- dlgList(c("Complete data", "Data without influencing value"), preselect=c("Complete data"),
                          multiple = FALSE, title="What results do you want to achieve?")$res
        if(length(outlier)==0) { Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                                            n.boot=NULL, rscale=0.353)
        return(Resultats)}
      }
      if(dial || length(method)>1 || method %in% c("pearson", "spearman","kendall") ==FALSE){
        if(info) writeLines("Please choose the type of correlations you want to achieve")
        method<-dlgList(c("pearson", "spearman","kendall"), preselect="pearson", multiple = FALSE, title="Type of correlations?")$res
        if(length(method)==0) { Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                                           n.boot=NULL, rscale=0.353)
        return(Resultats)}
      }
      
      
      if(is.null(Y) & is.null(Z)){
        
        if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
          msgBox("The number of bootstrap must be a positive integer") 
          n.boot<-NULL
        }
        while(is.null(n.boot)){
          writeLines("Please specify the number of bootstrap. To not have a bootstrap, choose 1")
          
          n.boot<-dlgInput("Number of bootstrap?", 1)$res
          if(length(n.boot)==0) {Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                                            n.boot=NULL, rscale=0.353)
          return(Resultats)}
          strsplit(n.boot, ":")->n.boot
          tail(n.boot[[1]],n=1)->n.boot
          as.numeric(n.boot)->n.boot
          if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
            msgBox("The number of bootstrap must be a positive integer") 
            n.boot<-NULL
          }
        }
      } 
      
      
      if((dial)|| !is.null(rscale) & ((is.numeric(rscale) & (rscale<0.1 | rscale>2)) || (!is.numeric(rscale) & rscale%in% c("medium", "wide", "ultralarge")==F))) {
        if(info) writeLines("Voulez-vous les tests d'hypothees nuls ou/et les facteurs bayesiens ?")   
        param<-dlgList(c("Bayesian factors","H0 tests"), preselect=c("Bayesian factors","H0 tests"), multiple = T, title="Statistical approach?")$res
        if(length(param)==0) { Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                                          n.boot=NULL, rscale=0.353)
        return(Resultats)}
        
        if(any(param=="Bayesian factors") | any(param=="FB")){
          if(info) writeLines("Please specify the a priori distribution of Cauchy")
          
          rscale<-dlgList(c("medium", "wide", "ultralarge"), preselect="medium", multiple = F, title="Quelle distribution voulez-vous  ?")$res 
          if(length(rscale)==0) {
            Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                       n.boot=NULL, rscale=0.353)
            return(Resultats)
          }
          ifelse(rscale=="medium", rscale<-2^0.5/4, ifelse(rscale=="wide", rscale<-0.5, ifelse(rscale=="ultralarge", rscale<-2^0.5/2, rscale<-rscale)))} else rscale<-NULL
      } 
      
      if(any(param=="H0 tests") |any(param=="H0")){
        if(dial | length(p.adjust)!=1 || p.adjust %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none")==FALSE){
          writeLines("Please specify the type of probability correction you want to perform")
          dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"), preselect=NULL, multiple = FALSE, title="Type of correction?")$res->p.adjust
          if(length(p.adjust)==0) {Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                                              n.boot=NULL, rscale=0.353)->Resultats
          return(Resultats)}
        } 
      } else p.adjust<-"none"
      if(dial | length(sauvegarde)!=1 || !is.logical(sauvegarde )){
        writeLines("do you want to save the results")
        sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = TRUE, title="Save the results?")$res
        if(length(sauvegarde)==0) {Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                                              n.boot=NULL, rscale=0.353)->Resultats
        return(Resultats)}
        
      } 
      
      if(any(is.na(data[,c(X1,Y,Z)]))){ 
        msgBox("Missing values have been detected. How do you want to treat them? Keeping all the observations can bias the results.")
        imp<- dlgList(c("Do nothing - Keep all observations", "Removing cases with missing values", "Replace with the mean",
                        "Replace with median","Multiple imputation - Amelia"), preselect=FALSE, multiple = TRUE, title="Treatment of missing values")$res
        if(length(imp)==0){
          Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                     n.boot=NULL, rscale=0.353)
          return(Resultats)
        }
        data1<-ez.imp(data[, c(X1,Y,Z)], imp=imp)
        data<-data.frame(data1, data[,group])
	names(data)<-c(names(data1), group)      
      }
      
      Resultats$nom<- nom
      Resultats$data<-data
      Resultats$X<-X1
      if(exists("Y")) Resultats$Y<-Y
      if(exists("Z")) Resultats$Z<-Z
      if(exists("group")) Resultats$group<-group
      Resultats$method<- method
      Resultats$outlier<-outlier
      Resultats$param<-param
      Resultats$rscale<-rscale
      Resultats$n.boot<-n.boot
      Resultats$sauvegarde<-sauvegarde
      Resultats$p.adjust<-p.adjust
      return(Resultats)
    }
    
    
    
    corr.matrice.out<-function(data, X, Y, Z, p.adjust, method,sauvegarde, rscale, n.boot, param){
      Resultats<-list()
      Resultats$"Descriptive statistics"<-.stat.desc.out(X=c(X,Y,Z), groupes=NULL, data=data, tr=.1, type=3, plot=F)
      Resultats$"Multivariate normalcy"<-.normalite(data, c(X,Y,Z)) 
      
      if(is.null(Z)){
        if(is.null(Y)) { Y1<-NULL
        pairs.panels(data[,X], density=T, lm=T, digits=3, ellipses=F, method=method, cor=T, jiggle=F, smoother=F, stars=T, pch=".")}else {
          Y1<-as.data.frame(data[,Y])
          names(Y1)<-Y
        }
        X1<-as.data.frame(data[,X])
        names(X1)<-X
        corr.test(x=X1, y=Y1, use = "pairwise",method=method,adjust=p.adjust, alpha=.05,ci=TRUE)->matrice  
        r1<-round(matrice$r,3)
        if(is.null(Y)) r1[which(lower.tri(r1, diag = T))]<-"-"
        Resultats$"Correlation matrix"<-as.data.frame(r1)
        
      } else{
        data[,c(X,Z)]->d2
        partial.r(d2, 1:length(X), (length(X)+1):length(d2))->matrice
        matrice<-corr.p(matrice, adjust=p.adjust, n=length(data[,1])-length(Z))
        
        r1<-round(matrice$r, 3)
        class(r1)<-"matrix"
        r1[which(lower.tri(r1, diag = T))]<-"-"
        Resultats$"Partial Correlations Matrix" <-as.data.frame(r1)
      }    
      
      class(r1)<-"matrix"
      dimnames(r1)[[1]]<-paste(dimnames(r1)[[1]], "r")
      matrice$n->Resultats$"sample size"
      
      if(any(param=="H0")|any(param=="H0 tests")) {paste("the correction applied is the correction of",p.adjust)->Resultats$Correction[1]
        if(is.null(Y)) Resultats$Correction[2]<-"Only values above the diagonals are adjusted for multiple comparisons"
        round(matrice$p,3)->r2
        class(r2)<-c("matrix", "p.value")
        Resultats$"probability matrix"<-r2
        dimnames(r2)[[1]]<-paste0(dimnames(r2)[[1]], ".p")
        if(is.null(Y)) r2[which(lower.tri(r2, diag = T))]<-NA
        r1<-rbind(r1,r2)
      }
      if(method=="kendall") {
        r2<-round(sin(0.5*pi*matrice$r)^2,3) # from David A. Walker 2003 JMASM9: Converting Kendall's Tau For Correlational Or Meta-Analytic Analyses 
        Resultats$"Information"<-"The effect size is calculated from the formula proposed by Walker, 2003"   
      } else r2<-round(matrice$r^2,3)
      
      
      
      if(!is.null(rscale)){
        r2[which(r2==1)]<-0
        if(is.null(Z))  N<-length(data[,1]) else    N<-length(data[,1])-length(Z)
        matriceBF<-function(X){return(linearReg.R2stat(N=N, 1, X, rscale = rscale, simple = TRUE))}
        r3<-round(apply(X=r2,c(1,2), FUN=matriceBF),3)
        r3<-format(r3, scientific=T)
        if(is.null(Y)) r3[which(lower.tri(r3, diag = T))]<-"-"
        dimnames(r3)[[1]]<-paste0(dimnames(r3)[[1]], ".FB")
        Resultats$"Bayesian factors"<-as.data.frame(r3)
        r1<-rbind(r1, r3)
      }
      class(r2)<-"matrix"
      if(is.null(Y)) r2[which(lower.tri(r2, diag = T))]<-"-"
      Resultats$"r.deux matrix" <-as.data.frame(r2)
      dimnames(r2)[[1]]<-paste(dimnames(r2)[[1]], "r ^ 2")
      r1<-rbind(r1, r2)
      r1<-data.frame(r1)
	    if(is.null(Y)){
      r1$tri<-1:length(dimnames(r1)[[2]])
      r1<-r1[order(r1$tri), ]
	    r1<-r1[,-length(r1)]
      r1[is.na(r1)]<-"-" 
		    }
      nice.mat<-list()
      nice.mat$"Correlation matrix"<-(r1)
      if(html) try(ez.html(nice.mat), silent =T)

      
      if(is.null(Y) & is.null(Z) & (!is.null(n.boot) && n.boot > 100)) round(cor.ci(data[,X], n.iter=n.boot, plot=FALSE)$ci,4)->Resultats$"Confidence interval estimated by bootstrap" else  round(matrice$ci,4)->Resultats$"Confidence interval" 
      names(Resultats[[length(Resultats)]])<-c("inf.lim","r","sup.lim","p-value")
      
      return(Resultats)  
      
    }
    
    options (warn=-1) 
    packages<-c("BayesFactor","nortest", "psych", "svDialogs", "ggplot2")
    
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    .e <- environment()
    Resultats<-list()
    try( windows(record=T), silent=T)->win
    if(class(win)=="try-error") quartz()
    if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data  
    
    corr.options<-corr.matrice.in(X=X, Y=Y, Z=Z, data=data, group=group, param=param, outlier=outlier, sauvegarde=sauvegarde, info=T,  rscale=rscale, n.boot=n.boot)
    if(is.null(corr.options)) return(analyse())
    
    choix<-corr.options$choix
    X<-corr.options$X
    Y<-corr.options$Y
    Z<-corr.options$Z
    group<-corr.options$group
    data<-corr.options$data
    param<-corr.options$param
    rscale<-corr.options$rscale
    sauvegarde<-corr.options$sauvegarde
    outlier<-corr.options$outlier
    method<-corr.options$method
    p.adjust<-corr.options$p.adjust
    n.boot<-corr.options$n.boot
    
    if(outlier=="Data without influencing value"){
      inf<-VI.multiples(data, X=c(X,Y,Z))
      Resultats$"Values considered influential"<-inf$"Values considered influential"
      data<-inf$data
    }
    
    Resultats$"Matrix of correlations"<-corr.matrice.out(data=data, X=X, Y=Y, Z=Z, p.adjust=p.adjust, method=method,sauvegarde=sauvegarde, rscale=rscale, n.boot=n.boot, param=param)
    
    
    
    if(!is.null(group))   {
      G<-data[,group]
      if(length(group)>1) G<-as.list(G)
      G<-split(data[,c(X,Y,Z)], G)
      for(i in 1:length(G)){
        resg<-corr.matrice.out(data=G[[i]], X=X, Y=Y, Z=Z, p.adjust=p.adjust, method=method,sauvegarde=sauvegarde, rscale=rscale, n.boot=n.boot, param=param)  
        Resultats[[length(Resultats)+1]]<-resg
        names(Resultats)[length(Resultats)]<-names(G)[i]
      }
    } 
    
    
    paste(X, collapse="','", sep="")->X
    if(!is.null(Y)) paste(Y, collapse="','", sep="")->Y
    if(!is.null(Z)) paste(Z, collapse="','", sep="")->Z
    if(!is.null(group)) paste(group, collapse="','", sep="")->group
    
    
    paste(outlier,  collapse="','", sep="")->outlier
    paste(param,  collapse="','", sep="")->param
    Resultats$Call<-paste0("corr.matrice(X=c('", X,
                           "'), Y=", ifelse(!is.null(Y),paste0("c('",Y,"')"), "NULL"), 
                           ", Z =", ifelse(!is.null(Z),paste0("c('",Z,"')"), "NULL"), ",data=",  corr.options$nom, ", p.adjust='", p.adjust,
                           "', group=", ifelse(!is.null(group),paste0("c('",group,"')"), "NULL"), 
                           ", param=c('", param, "'), save =", sauvegarde, ",outlier=c('", outlier, "'), info=T, rscale=", ifelse(!is.null(rscale),rscale, "NULL"), ", n.boot=", n.boot, ")")
    
    .add.history(data=data, command=Resultats$Call, nom=corr.options$nom)
    .add.result(Resultats=Resultats, name =paste(choix, Sys.time() ))
    
    
    
    if(sauvegarde) save(Resultats=Resultats, choix=paste("correlation of", method), env=.e)
    ref1(packages)->Resultats$"References"
    if(html) try(ez.html(Resultats), silent=T)
    return(Resultats)
    }
