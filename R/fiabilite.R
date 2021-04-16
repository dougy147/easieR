.fiabilite.msg<-


fiabilite <-
  function(X=NULL,Y=NULL, data=NULL, choice=NULL, ord=NULL,outlier="Complete data", keys=NULL, n.boot=NULL, backup =F, 
           imp=NULL, html=TRUE){
    # choice
    
    options (warn=-1)
    packages<-c("svDialogs", "psych", "lavaan")
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    
    .e<- environment()
    Results<-list()
    if(is.null(data) | is.null(X))  {dial<-TRUE}else dial<-F
    if(dial || is.null(choice) || length(choice)!=1 ||choice %in% c("Cronbach's alpha","alpha","ICC","CCK","Intra-class correlation","Kendall coefficient of concordance")==FALSE){
      dial<-T  
      writeLines("Please choose the analysis you want to perform.")
      dlgList(c("Cronbach's alpha", "Intra-class correlation","Kendall coefficient of concordance"), preselect=NULL, multiple = FALSE, title="What analysis do you want to perform?")$res->choice
      if(length(choice)==0) return(analyse())
    }
    
    
    if(dial || class(data)!="data.frame"){
      data<-choice.data(data=data, nom=T)
      if(length(data)==0) return(analyse())
      nom<-data[[1]]
      data<-data[[2]]  
    }else{
      deparse(substitute(data))->nom  
    }
    
    if(choice=="CCK" | choice=="Kendall coefficient of concordance"){
      msg3<-"Please choose the first judge"
      type<-"factor"
      title<-"Judge 1"
      multiple<-T
    } else{
      multiple<-T
      msg3<-"Please choose the variables you want to analyze."
      type<-"numeric"
      title<-"variables"
    }
    
    X<-.var.type(X=X, data=data, type=type, check.prod=F, message=msg3,  multiple=multiple, title=title, out=NULL)
    if(is.null(X)) {
      Results<-fiabilite()
      return(Results)}
    data<-X$data
    X<-X$X
    
    if(choice %in% c("Cronbach's alpha","Intra-class correlation","ICC","alpha") ){
      if(dial || length(outlier)>1 || outlier %in% c("Complete data", "Data without influencing value") ==FALSE){
     writeLines("Do you want the analysis on the complete data or on the data for which the influencing values have been removed?")
     writeLines("influencing values are identified based on the Mahalanobis distance with a chi threshold of 0.001")
        outlier<- dlgList(c("Complete data", "Data without influencing value"), preselect="Complete data",multiple = FALSE, title="What results do you want to achieve?")$res
        if(length(outlier)==0) { Results<-fiabilite()
        return(Results)}
      }
      
      if(outlier=="Data without influencing value"){
        inf<-VI.multiples(data[,X])
        Results$"Values considered influential"<-inf$"Values considered influential"
        data<-inf$data
      }
      
      
      if(choice %in% c("Cronbach's alpha","alpha"))  {
        if(dial){
         writeLines("Veuillez preciser le type de variables. Des correlations tetra/polychoriques seront realisees sur les variables ordinales et Bravais-Pearson sur les variables continues")
          type<-dlgList(c("dichotomiques/ordinales", "continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature of the variables?")$res
          if(length(type)==0) {Results<-fiabilite()
          return(Results)
          }} else{if(is.null(ord)) type<-"continues" else type<-"dichotomiques/ordinales"}
        
        if(dial){
          writeLines("Are there any reverse items?") 
          rev<-dlgList(c(TRUE,FALSE), multiple = FALSE, title="inverse items?")$res
          if(length(rev)==0) {
            Results<-fiabilite()
            return(Results)
          }  } 
          
          if(rev=="TRUE" || !is.null(keys) && any(keys %in% X==FALSE)){
            writeLines("Please specify the reverse items")
            keys<-dlgList(X, multiple = TRUE, title="inverse items?")$res
            if(length(keys)==0) {
              Results<-fiabilite()
            return(Results)
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
              if(length(n.boot)==0) {Results<-fiabilite()
              return(Results)}
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
              ord<-dlgList(X, multiple = TRUE, title="Ordinal variables?")$res
              if(length(ord)==0){
                Results<-fiabilite()
                return(Results)
              }
            }else ord<-X
            Matrice<-tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp=imp,html=F)[[1]]
            if(all(class(Matrice)!="matrix")) {
              sortie<-dlgMessage("You're trying to alpha on something other than a matrix. Do you want to get out of this analysis?", type="yesno")$res
              if(sortie=="yes") return(analyse()) else Matrice<-tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp="rm")[[1]]
            }
            
            psych::alpha(Matrice, keys=keys,n.obs=length(data[,1]))->cron
          }
          
          round(cron$total,3)->Results$"Cronbach's alpha over the entire scale"
          if(n.boot>1) cron$boot.ci->Results$"Bootstrap Confidence Interval"
          cron$total[,1]->a1
          cron$total[,6]->ase
          data.frame(Lower CI limit.95=a1-1.96*ase, alpha=a1, Lim.up.CI.95=a1+1.96*ase)->Results$"Confidence interval based on standard error of alpha"
          round(data.frame(cron$alpha.drop, cron$item.stats ),3)->Results$"reliability per item removed"
          
      }
      
      if(choice=="Intra-class correlation"| choice=="ICC"){psych::ICC(data[,X], missing=FALSE)->ICC.out
        ICC.out[[1]]->Results$"intra-class correlation"
        names(Results$"intra-class correlation")<-c("type", "ICC", "F", "dof1", "dof2", "p-value", "inf.lim","sup.lim")
        Results$"informations"<-paste("the number of judges =", length(X), "and the number of observations =", ICC.out$n.obs) } 
    }
    
    
    if(choice=="Kendall coefficient of concordance"){  
      msg4<-"Please choose the second judge"
      Y<-.var.type(X=Y, data=data, type=type, check.prod=F, message=msg4,  multiple=F, title="Judge 2", out=X)
      if(is.null(Y)) {
        Results<-fiabilite()
        return(Results)}
      data<-Y$data
      Y<-Y$X
      cohen.kappa(data[,c(X,Y)], w=NULL,n.obs=NULL,alpha=.05)->CK.out
      dimnames(CK.out$confid)<-list(c("Unweighted kappa coefficient","Weighted kappa coefficient"),c("inf.lim","estimation","sup.lim"))
      round(CK.out$confid,3)->Results$"Kendall coefficient of concordance"
      CK.out$agree->Results$"Agreement"
      Results$information<-paste("the number of observations =", CK.out$n.obs)
    }
    
    if(dial) dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="do you want to save?")$res->sauvegarde
    if(length(sauvegarde)==0) {
      Results<-fiabilite()
      return(Results)
    }
    
    paste(X, collapse="','", sep="")->X
    if(!is.null(ord)) paste(ord, collapse="','", sep="")->ord
    if(!is.null(keys)) paste(ord, collapse="','", sep="")->keys
    
    Results$Call<-paste0("reliability (X = c ('", X,"'),Y=", ifelse(is.null(Y), "NULL", paste0("'",Y,"'")), ",data=", nom, ", choice = '", choice,"',ord=", 
                           ifelse(!is.null(ord),paste0("c('", ord, "')"), "NULL" ), ",outlier='", outlier, "', keys=", ifelse(!is.null(keys), paste0("c('",keys,"')"), "NULL"),
                           ",n.boot=", ifelse(!is.null(n.boot), n.boot, "NULL"), ", backup =", sauvegarde, ")")
    
    .add.history(data=data, command=Results$Call, nom=nom)
    .add.result(Results=Results, name =paste("psychoanalyst", Sys.time() ))  
    
    
    if(sauvegarde)save(Results=Results, choice=choice, env=.e)
    ref1(packages)->Results$"References"
   if(html) try(ez.html(Results), silent=T)
    return(Results)
  }
