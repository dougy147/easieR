.fiabilite.msg<-


fiabilite <-
  function(X=NULL,Y=NULL, data=NULL, choix=NULL, ord=NULL,outlier="Donnees completes", keys=NULL, n.boot=NULL, sauvegarde=F, 
           imp=NULL, html=TRUE){
    # choix
    
    options (warn=-1)
    packages<-c("svDialogs", "psych", "lavaan")
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    
    .e<- environment()
    Resultats<-list()
    if(is.null(data) | is.null(X))  {dial<-TRUE}else dial<-F
    if(dial || is.null(choix) || length(choix)!=1 ||choix %in% c("Alpha de Cronbach","alpha","ICC","CCK","Intra-class correlation","Kendall coefficient of concordance")==FALSE){
      dial<-T  
      writeLines("Please choose the analysis you want to perform.")
      dlgList(c("Alpha de Cronbach", "Intra-class correlation","Kendall coefficient of concordance"), preselect=NULL, multiple = FALSE, title="What analysis do you want to perform?")$res->choix
      if(length(choix)==0) return(analyse())
    }
    
    
    if(dial || class(data)!="data.frame"){
      data<-choix.data(data=data, nom=T)
      if(length(data)==0) return(analyse())
      nom<-data[[1]]
      data<-data[[2]]  
    }else{
      deparse(substitute(data))->nom  
    }
    
    if(choix=="CCK" | choix=="Kendall coefficient of concordance"){
      msg3<-"Please choose the first judge"
      type<-"factor"
      title<-"Juge 1"
      multiple<-T
    } else{
      multiple<-T
      msg3<-"Please choose the variables you want to analyze."
      type<-"numeric"
      title<-"variables"
    }
    
    X<-.var.type(X=X, data=data, type=type, check.prod=F, message=msg3,  multiple=multiple, title=title, out=NULL)
    if(is.null(X)) {
      Resultats<-fiabilite()
      return(Resultats)}
    data<-X$data
    X<-X$X
    
    if(choix %in% c("Alpha de Cronbach","Intra-class correlation","ICC","alpha") ){
      if(dial || length(outlier)>1 || outlier %in% c("Donnees completes", "Data without influencing value") ==FALSE){
     writeLines("Do you want the analysis on the complete data or on the data for which the influencing values have been removed?")
     writeLines("influencing values are identified based on the Mahalanobis distance with a chi threshold of 0.001")
        outlier<- dlgList(c("Donnees completes", "Data without influencing value"), preselect="Donnees completes",multiple = FALSE, title="What results do you want to achieve?")$res
        if(length(outlier)==0) { Resultats<-fiabilite()
        return(Resultats)}
      }
      
      if(outlier=="Data without influencing value"){
        inf<-VI.multiples(data[,X])
        Resultats$"Values considered influential"<-inf$"Values considered influential"
        data<-inf$data
      }
      
      
      if(choix %in% c("Alpha de Cronbach","alpha"))  {
        if(dial){
         writeLines("Veuillez preciser le type de variables. Des correlations tetra/polychoriques seront realisees sur les variables ordinales et Bravais-Pearson sur les variables continues")
          type<-dlgList(c("dichotomiques/ordinales", "continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature of the variables?")$res
          if(length(type)==0) {Resultats<-fiabilite()
          return(Resultats)
          }} else{if(is.null(ord)) type<-"continues" else type<-"dichotomiques/ordinales"}
        
        if(dial){
          writeLines("Are there any reverse items?") 
          rev<-dlgList(c(TRUE,FALSE), multiple = FALSE, title="items inverses?")$res
          if(length(rev)==0) {
            Resultats<-fiabilite()
            return(Resultats)
          }  } 
          
          if(rev=="TRUE" || !is.null(keys) && any(keys %in% X==FALSE)){
            writeLines("Please specify the reverse items")
            keys<-dlgList(X, multiple = TRUE, title="items inverses?")$res
            if(length(keys)==0) {
              Resultats<-fiabilite()
            return(Resultats)
            }
          }else keys<-NULL
          
          
          
          if(type=="continues"){
            if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
              msgBox("The number of bootstrap must be a positive integer") 
              n.boot<-NULL
            }
            while(is.null(n.boot)){
              writeLines("Please specify the number of bootstrap. To not have a bootstrap, choose 1")
              n.boot<-dlgInput("Number of bootstrap?", 1)$res
              if(length(n.boot)==0) {Resultats<-fiabilite()
              return(Resultats)}
              strsplit(n.boot, ":")->n.boot
              tail(n.boot[[1]],n=1)->n.boot
              as.numeric(n.boot)->n.boot
              if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
                msgBox("The number of bootstrap must be a positive integer") 
                n.boot<-NULL
              }
            }
            psych::alpha(data[,X], keys=keys, n.iter=n.boot)->cron
          }else{
            n.boot<-0
            if(type=="mixte") {
              writeLines("Please specify ordinal variables?") 
              ord<-dlgList(X, multiple = TRUE, title="Variables ordinales ?")$res
              if(length(ord)==0){
                Resultats<-fiabilite()
                return(Resultats)
              }
            }else ord<-X
            Matrice<-tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp=imp,html=F)[[1]]
            if(all(class(Matrice)!="matrix")) {
              sortie<-dlgMessage("You're trying to alpha on something other than a matrix. Do you want to get out of this analysis?", type="yesno")$res
              if(sortie=="yes") return(analyse()) else Matrice<-tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp="rm")[[1]]
            }
            
            psych::alpha(Matrice, keys=keys,n.obs=length(data[,1]))->cron
          }
          
          round(cron$total,3)->Resultats$"Cronbach's alpha over the entire scale"
          if(n.boot>1) cron$boot.ci->Resultats$"Bootstrap Confidence Interval"
          cron$total[,1]->a1
          cron$total[,6]->ase
          data.frame(Lim.inf.IC.95=a1-1.96*ase, alpha=a1, Lim.sup.IC.95=a1+1.96*ase)->Resultats$"Confidence interval based on standard error of alpha"
          round(data.frame(cron$alpha.drop, cron$item.stats ),3)->Resultats$"reliability per item removed"
          
      }
      
      if(choix=="Intra-class correlation"| choix=="ICC"){psych::ICC(data[,X], missing=FALSE)->ICC.out
        ICC.out[[1]]->Resultats$"intra-class correlation"
        names(Resultats$"intra-class correlation")<-c("type", "ICC", "F", "ddl1", "ddl2", "valeur.p", "lim.inf","lim.sup")
        Resultats$"informations"<-paste("the number of judges =", length(X), "and the number of observations =", ICC.out$n.obs) } 
    }
    
    
    if(choix=="Kendall coefficient of concordance"){  
      msg4<-"Please choose the second judge"
      Y<-.var.type(X=Y, data=data, type=type, check.prod=F, message=msg4,  multiple=F, title="Juge 2", out=X)
      if(is.null(Y)) {
        Resultats<-fiabilite()
        return(Resultats)}
      data<-Y$data
      Y<-Y$X
      cohen.kappa(data[,c(X,Y)], w=NULL,n.obs=NULL,alpha=.05)->CK.out
      dimnames(CK.out$confid)<-list(c("Unweighted kappa coefficient","Weighted kappa coefficient"),c("lim.inf","estimation","lim.sup"))
      round(CK.out$confid,3)->Resultats$"Kendall coefficient of concordance"
      CK.out$agree->Resultats$"Accord"
      Resultats$information<-paste("the number of observations =", CK.out$n.obs)
    }
    
    if(dial) dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="do you want to save?")$res->sauvegarde
    if(length(sauvegarde)==0) {
      Resultats<-fiabilite()
      return(Resultats)
    }
    
    paste(X, collapse="','", sep="")->X
    if(!is.null(ord)) paste(ord, collapse="','", sep="")->ord
    if(!is.null(keys)) paste(ord, collapse="','", sep="")->keys
    
    Resultats$Call<-paste0("fiabilite(X=c('", X,"'),Y=", ifelse(is.null(Y), "NULL", paste0("'",Y,"'")), ",data=", nom, ",choix='", choix,"',ord=", 
                           ifelse(!is.null(ord),paste0("c('", ord, "')"), "NULL" ), ",outlier='", outlier, "', keys=", ifelse(!is.null(keys), paste0("c('",keys,"')"), "NULL"),
                           ",n.boot=", ifelse(!is.null(n.boot), n.boot, "NULL"), ", sauvegarde=", sauvegarde, ")")
    
    .add.history(data=data, command=Resultats$Call, nom=nom)
    .add.result(Resultats=Resultats, name =paste("cor.polychorique", Sys.time() ))  
    
    
    if(sauvegarde)save(Resultats=Resultats, choix=choix, env=.e)
    ref1(packages)->Resultats$"References"
   if(html) try(ez.html(Resultats), silent=T)
    return(Resultats)
  }
