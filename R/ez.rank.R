ez.rank <-
  function(data=NULL, X=NULL, ties.method="average", info=T){
    # data : name of the dataframe 
    # X : Character. Vector with the name of the variable to sort. 
    # ties.method : one among the following "average",  "first", "last", "random", "max", "min". 
    # info : logical. Should message to be print if dialog box ? 
    # default for ties.method = average
    options (warn=-1)
    c("svDialogs")->packages
    lapply(packages, require, character.only=T)
    list()->Results
    .e <- environment()
    if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data
    
    choice.data(data=data, info=TRUE, nom=T)->data 
    if(length(data)==0) { return(preprocess())} else {
      data[[1]]->nom1
      data[[2]]->data}
    if(!is.null(X)) dial<-FALSE else dial<-TRUE
    msg.pre1<-"Please specify the variables you want to rank"
    .var.type(X=X, info=T, data=data, type="numeric", message=msg.pre1,multiple=T, title="Variable-s")->X1
    if(is.null(X1)) return(preprocess())
    if(!is.null(X) && X1$X!=X) dial<-TRUE 
    X1$X->X
    if(dial){
      if(info) writeLines("Comment voulez-vous traiter les ex-aequo ? La methode *average* fait la mediumne Between les ex aequo (le plus habituel),
                        *first* attribue le premier rang ex aequo a la premiere value dans les donnees, *laste* a la derniere, *min* attribue la
                        value minimale a l'ensemble des ex aequo et *max* la value maximale.")
      ties.method<-dlgList(c("average", "first", "last", "random", "max", "min"), multiple = F, preselect="average", title="Specify workforce?")$res
    }
    if(length(X)==1) rangs<-rank(data[,X],ties.method=ties.method, na.last="keep" ) else sapply(data[,X], rank, ties.method=ties.method, na.last="keep")->rangs
    if(length(X)==1) data.frame(rangs)->rangs
    dimnames(rangs)[[2]]<-paste0(X, ".rangs")
    data.frame(data, rangs)->data
    assign(nom1,data,envir=.GlobalEnv)
    paste(X, collapse="','", sep="")->X
    Results$call<-paste0("ez.rank(data=", nom1, ", X=c('",X, "'), ties.method='",ties.method, "', info=T)")  
    .add.history(data=data, command=Results$Call, nom=nom1)
    ref1(packages)->Results$References
    return(Results)
  }
