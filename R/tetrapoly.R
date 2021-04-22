tetrapoly <-
  function(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator="two.step", output="cor", imp=NULL, html=T){
    # data : dataframe
    # X : vector of variables names 
    # sauvegarde : bolean. Should analysis be saved ? 
    # ord : Character. names of variables considered as ordinal. The other are considered as continuous.
    # info : bolean. Should information be printed in the console during analysis ? 
    # group : character. Name of the factor variable 
    # estimator : see ?lavCor for information 
    # output : see ?lavCor for information
    # html : logical. Should output be a HTML page ? 
    options (warn=-1) 
    c("lavaan", "svDialogs")->packages
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    
    .e<- environment()
    Results<-list()
    
    if(is.null(data) | is.null(X))  {dial<-TRUE
    if(info) writeLines("Please choose the type of correlations you want to achieve. For dichotomous variables, the correlations will be tetrachoric correlations")
    dlgList(c("polychoric correlations", "mixed correlations"), preselect=NULL, multiple = FALSE, title="Type of correlations?")$res->method
    if(length(method)==0) return(choice.corr())
    } else dial<-F
    
    
    if(dial || class(data)!="data.frame"){
      data<-choice.data(data=data, info=info, nom=T)
      if(length(data)==0) return(choice.corr())
      nom<-data[[1]]
      data<-data[[2]]  
    }else{
      deparse(substitute(data))->nom  
    }
    
    
    msg3<-"Veuillez choisir les variables dont il faut realiser les correlations polychorique/tetrachorique/mixte."
    X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title="Variable-s ", out=NULL)
    if(is.null(X)) {
      Results<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
      return(Results)}
    data<-X$data
    X<-X$X
    
    if(!is.null(ord) & any(ord %in%X==F)||(dial && method=="mixed correlations" ) ){
      if(info) writeLines("Please choose ordinal variables.")
      ord<-dlgList(X, preselect=X, multiple = TRUE, title="Ordinal variables?")$res
      if(length(ord)==0){
        Results<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
        return(Results)
      }
    } else ord<-X
    if(any(is.na(data[,X]))) {
      if(is.null(imp))  {msgBox("Missing values have been detected. How do you want to treat them? Keeping all the observations can bias the results.")
        imp<- dlgList(c("Do nothing - Keep all observations", "Removing cases with missing values","Replace with median","Multiple imputation - Amelia"), 
                      preselect=FALSE, multiple = TRUE, title="Treatment of missing values?")$res}
      if(length(imp)==0){
        Results<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
        return(Results)
      }
      data1<-ez.imp(data[, X], imp=imp, ord= ord)
      data<-data.frame(data1, data[which(dimnames(data)[[1]] %in% dimnames(data1)[[1]]),group])
    }  
    if(dial || !is.logical(sauvegarde)){
      sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title="Do you want to save the results?")$res
      if(length(sauvegarde)==0) {
        Results<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
        return(Results)
      }
    }
    Results$"Correlation matrix tetrachorique/polychorique ou mixte"<-lavCor(data[,c(X,group)], ordered=ord,estimator=estimator, group=group,  missing="default", output=output)
    paste(X, collapse="','", sep="")->X
    if(!is.null(ord)) paste(ord, collapse="','", sep="")->ord
    Results$Call<-paste0("tetrapoly(data=", nom,",X=c('", X,"'),sauvegarde=", sauvegarde, ",ord=", ifelse(!is.null(ord),paste0("c('",ord,"')"), "NULL"),
                           ",info=T, group=", ifelse(!is.null(group),paste0("'",group,"'"), "NULL"), ",estimator='", estimator, "',output='", output, "')")
    
    .add.history(data=data, command=Results$Call, nom=nom)
    .add.result(Results=Results, name =paste("psychoanalyst", Sys.time() ))  
    
    
    if(sauvegarde) save(Results=Results, choice="psychoanalyst", env=.e)
    
    ref1(packages)->Results$"References"
    if(html) ez.html(Results)
    return(Results) }
