ez.reshape<-function(data=NULL, varying = NULL, v.names = NULL,
                     idvar = "id",  IV.names=NULL, IV.levels=NULL  ){
  #data 	: a data frame
  # varying : names of sets of variables in the wide format that correspond to single variables in long format (‘time-varying’).
  #This is canonically a list of vectors of variable names,
  #but it can optionally be a matrix of names, or a single vector of names.
  #In each case, the names can be replaced by indices which are interpreted as referring to names(data).
  #v.names : names of variables in the long format that correspond to multiple variables in the wide format.
  # timevar : the variable in long format that differentiates multiple records from the same group or individual.
  # If more than one record matches, the first will be taken (with a warning).
  #idvar : Names of one or more variables in long format that identify multiple records from the same group/individual.
  #These variables may also be present in wide format.
  # times : the values to use for a newly created timevar variable in long format
  # IV.names : list with the name of the independant variables names created in the long format
  # IV.levels : list with the levels of the independant variables created in the long format

  options (warn=-1)
  # chargement des packages
  packages<-c('svDialogs', 'reshape2')
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== 'try-error') return(ez.install())
  .e <- environment()
  Resultats<-list()
  if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data


  output<-.ez.reshape.in(data=data, varying = varying, v.names = v.names,
                         idvar = idvar, IV.names=IV.names,IV.levels=IV.levels )
  if(is.null(output)) return(NULL)
  data<-output$data
  varying<-output$varying
  v.names<-output$v.names
  idvar<-output$idvar
  IV.names<-output$IV.names
  IV.levels<-output$IV.levels
  nom<-output$nom
  N.modalites2<-output$N.modalites2
  times<-output$times
  dostop<-output$dostop
  longdata<-.ez.reshape.out(data=data, varying = varying, v.names = v.names,
                            idvar = idvar,times=times,
                            IV.names=IV.names,IV.levels=IV.levels, N.modalites2=N.modalites2 )

  assign(paste0(nom, ".long"),longdata, envir = .GlobalEnv)
  View(longdata)
  if(length(IV.names)>1 & dostop){
    cat (.ez.reshape.msg("msg",9))
    line <- readline()
    dlgMessage(.ez.reshape.msg("title", 9), "yesno")$res->suppression
    if(suppression=="no") return(ez.reshape(data=data, varying=varying))
  }


  assign("intra",IV.names,envir=.e)

  varying.call<-c()
  for(i in 1:length(varying)){
    if(i>1) varying.call<-paste0(varying.call, ",")
    varying.call0<-paste0("c('",  paste(varying[[i]], collapse="','", sep=""))

    varying.call0<-paste0(varying.call0, "')")
    varying.call<-paste0(varying.call, varying.call0)
  }

  v.names.call<-paste(v.names, collapse="','", sep="")
  idvar.call<-paste(idvar, collapse="','", sep="")
  IV.names.call<-paste(IV.names, collapse="','", sep="")
  if(!is.null(IV.levels)){
    IV.levels.call<-c()
    for(i in 1:length(IV.levels)){
      if(i>1) IV.levels.call<-paste0(IV.levels.call, ",")
      IV.levels.call0<-paste0("c('",  paste(IV.levels[[i]], collapse="','", sep=""))

      IV.levels.call0<-paste0(IV.levels.call0, "')")
      IV.levels.call<-paste(IV.levels.call, IV.levels.call0)
    }
    IV.levels.call<-paste0("list(", IV.levels.call ,"))" )
  }else IV.levels.call<-"NULL)"



  Call<-paste0("ez.reshape(data=",nom, ", varying =list(", varying.call,"), v.names =c('",v.names.call,
               "'),idvar =c('", idvar.call,"'),IV.names=list('", IV.names.call,
               "'), IV.levels=" , IV.levels.call)

  writeLines(Call)
  .add.history(data=data, command=Call, nom=nom)

  return(longdata)
}

.ez.reshape.msg<-function(type, number){
  # type : either "msg" or "title"
  # number : number of message
  #if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())) {
  msg<-c(.dico[["ask_chose_cols_corresponding_to_repeated_measures"]],
         .dico[["txt_col_correspoding_to_variable"]],
         .dico[["desc_should_specify_nb_factors_repeated_measure"]],
         .dico[["ask_did_not_specify_nb_factors_repeated_measure_exit"]],
         .dico[["desc_non_numeric_value"]],
         .dico[["desc_you_have_selected"]], .dico[["txt_cols"]],
         .dico[["desc_modalities_product_must_correspond_to_cols_selected"]],
         .dico[["ask_press_enter_to_continue"]])


  title<-c(.dico[["txt_cols_in_repeated_measure"]] ,.dico[["txt_nb_variables_measured"]], .dico[["txt_measured_variable_name"]],
           .dico[["ask_nb_factors_repeated_measure"]],
           .dico[["txt_factor_name"]],.dico[["ask_how_many_modalities"]],.dico[["txt_modality"]], .dico[["txt_modalities_name_for"]],
           .dico[["ask_is_long_format_correct"]])

  #} else {
  #  msg<-c("Please choose all the columns corresponding to the repeaed measure levels",
  #         "Columns corresponding to variable",
  #         "you must state the number of repeated measure factors",
  #         "You have not state the number of repeated measures factors. Do you want to abort ?",
  #         "The value is not numeric. You must enter a numeric value",
  #         "you have selected","columns",
  #         "The product of the number of levels of each factor must  equal to the number of selected columns.",
  #         "Press [enter] to continue")
  #  title<-c("Columns in repeated measures", "Number of measured variables", "Name of measured variable",
  #           "How many repeated measures variables ?",
  #           "name of the factor", "How many levels", "level", "Name of levels for",
  #           .dico[["ask_is_long_format_correct"]])
  #}

  ifelse(type=="msg", r<-msg, r<-title)
  r<-r[number]
  return(r)}



.ez.reshape.in<-function(data, varying = NULL, v.names = NULL,
                         idvar = "id",  IV.names=NULL, IV.levels=list()) {
  resultats<-list()
  dostop<-F
  # completer les dial
  data<-choix.data(data=data, info=T, nom=T)
  if(length(data)==0) return(NULL)
  nom<-data[[1]]
  data<-data[[2]]


  if(is.null(varying) || any(!unlist(varying) %in%names(data)) ){
    varying<-list()
    n.var<-NA
    dostop<-T
    while(is.na(n.var)){
      n.var <- dlgInput(.ez.reshape.msg("title",2), 1)$res
      if(length(n.var)==0) {
        return(ez.reshape())}
      strsplit(n.var, ":")->n.var
      tail(n.var[[1]],n=1)->n.var
      as.numeric(n.var)->n.var
      if(is.na(n.var)) msgBox(.ez.reshape.msg("msg",5))
    }
    varying2<-.var.type(X= unlist(varying) , info=T, data=data, type=NULL,
                        check.prod=F, message=.ez.reshape.msg("msg",1),  multiple=T,
                        title=.ez.reshape.msg("title",1), out=NULL)
    if(is.null(varying2)) {
      return(ez.reshape())}

    varying2<-varying2$X
    if(n.var>1) {
      v.names<-c()
      for(i in 1:(n.var-1)){
        paste(.ez.reshape.msg("msg",2), i)
        varying[[i]]<-dlg_list(varying2, multiple=T, title=paste0("Variables.",i))$res
        varying2<-setdiff(varying2, unlist(varying))
        var.name <- dlgInput(paste(.ez.reshape.msg("title",3),i), paste0("Variable.", i))$res
        if(length(var.name)==0) { return(ez.reshape())}
        strsplit(var.name, ":")->var.name
        tail(var.name[[1]],n=1)->var.name
        var.name<-gsub("[^[:alnum:]]", ".", var.name)
        v.names<-c(v.names, var.name)
      }
      varying[[n.var]]<-varying2
      var.name <- dlgInput(paste(.ez.reshape.msg("title",3),n.var), paste0("Variable.", n.var))$res
      if(length(var.name)==0) { return(ez.reshape())}
      strsplit(var.name, ":")->var.name
      tail(var.name[[1]],n=1)->var.name
      var.name<-gsub("[^[:alnum:]]", ".", var.name)
      v.names<-c(v.names, var.name)

    } else {
      varying[[1]]<-  varying2
      v.names <- dlgInput(paste(.ez.reshape.msg("title",3),1), .dico[["txt_variable"]])$res
      if(length(v.names)==0) { return(ez.reshape())}
      strsplit(v.names, ":")->v.names
      v.names<-gsub("[^[:alnum:]]", ".", v.names)
      tail(v.names[[1]],n=1)->v.names
    }

  } else n.var=length(v.names)


  if(!is.null(setdiff(names(data),unlist(varying)))) idvar<-setdiff(names(data),unlist(varying))
    data$IDeasy<-paste0("p", 1:nrow(data))
    data$IDeasy<-factor(data$IDeasy)
    idvar<-c(idvar,"IDeasy")

  if(n.var==1) times=unlist(varying) else {
    x<-varying[[1]]
    y<-varying[[2]]
    times<-sapply(seq_along(x), function(i)
      paste(LCS(strsplit(x[i], '')[[1]], strsplit(y[i], '')[[1]])$LCS,
            collapse = ""))
    if(any(nchar(times)==0)) {
      concat<-paste0("str_c(varying[[1]]")
      for(i in 2:length(varying)){
        concat<-paste0(concat, ",varying[[", i, "]]")
      }
      concat<-paste0(concat, ")")
      times<-eval(parse(text= concat))
    }
  }

  if(is.null(IV.names)| (length(IV.names)>1 & is.null(IV.levels)) |
     (!is.null(IV.levels) &&  prod(sapply(IV.levels, length))!=length(varying[[1]])) ) {
    dostop<-T
    if(is.null(IV.names) & length(varying[[1]])>3) N.facteurs <- dlgInput(.ez.reshape.msg("title",4), 1)$res else {
      N.facteurs<-"1"
    }
    while(length(N.facteurs)=="0"){
      writeLines(.ez.reshape.msg("msg",3))
      dlgMessage(.ez.reshape.msg("msg",4), "yesno")$res->quitte
      if(quitte=="yes") return(NULL) else  N.facteurs <- dlgInput(.ez.reshape.msg("title",4), 1)$res }
    strsplit(N.facteurs, ":")->N.facteurs
    tail(N.facteurs[[1]],n=1)->N.facteurs
    as.numeric(N.facteurs)->N.facteurs
    if(is.na(N.facteurs)) { writeLines(.ez.reshape.msg("msg",5))
      return(NULL)}


  }
  c()->N.modalites2
  if(is.null(IV.names) || !is.character(unlist(IV.names))){
    list()->IV.names # liste pour stocker le nom des facteurs


    if(N.facteurs==1){
      dlgInput(.ez.reshape.msg("title",5), "Variable.1")$res->IV.names[[1]]
      if(length(IV.names [[1]])==0) return(ez.reshape(data=data, varying=varying))
      strsplit(IV.names [[1]], ":")->IV.names[[1]]
      tail(IV.names [[1]][[1]],n=1)->IV.names[[1]]
      IV.names[[1]]<-gsub("[^[:alnum:]]", ".", IV.names[[1]])
    } else {
      c()->N.modalites2 # nombre de modalités sur chaque facteur
      while(prod(N.modalites2)!=length(varying[[1]])){
        # liste pour stocker le nom des modalités

        writeLines(paste(.ez.reshape.msg("msg",6), length(varying[[1]]),.ez.reshape.msg("msg",7) ))
        writeLines(.ez.reshape.msg("msg",8))
        for(i in 1:N.facteurs) {
          dlgInput(paste(.ez.reshape.msg("title",5),i), paste(.dico[["txt_variable"]],i, sep="."))$res->IV.names[[i]]
          if(length(IV.names[[i]])==0) return(ez.reshape(data=data, varying=varying))
          strsplit(IV.names[[i]], ":")->IV.names[[i]]
          tail(IV.names[[i]][[1]],n=1)->IV.names[[i]]
           IV.names[[i]]<-gsub("[^[:alnum:]]", ".", IV.names[[i]])
          N.modalites <- dlgInput(paste(.ez.reshape.msg("title",6), IV.names[[i]]), 2)$res
          if(length(N.modalites)==0) return(ez.reshape(data=data, varying=varying))
          strsplit(N.modalites, ":")->N.modalites
          tail(N.modalites[[1]],n=1)->N.modalites
          as.numeric(N.modalites)->N.modalites
          if(is.na(N.modalites)) writeLines(.ez.reshape.msg("msg",5))
          c(N.modalites2,N.modalites)->N.modalites2
          dlgForm(setNames(as.list(paste(.ez.reshape.msg("title",7), 1:N.modalites2[i])),
                           paste(.ez.reshape.msg("title",7), 1:N.modalites2[i])),
                  paste(.ez.reshape.msg("title",8), IV.names[[i]]) )$res->IV.levels[[i]]
        }

      }
    }
  }

  resultats$nom<-nom
  resultats$data<-data
  resultats$varying<-varying
  resultats$v.names<-v.names
  resultats$idvar<-idvar
  resultats$IV.names<-IV.names
  if(length(IV.names)>1) resultats$IV.levels<-IV.levels else resultats$IV.levels<-NULL
  resultats$N.modalites2<-N.modalites2
  resultats$times<-times
  resultats$dostop<-dostop
  return(resultats)
}


.ez.reshape.out<-function(data, varying = NULL, v.names = NULL,
                          idvar = "id", times, IV.names=NULL,IV.levels=NULL,N.modalites2=NULL )    {
    longdata<-reshape(data, direction="long",idvar= idvar, varying=varying, v.names=v.names, times = times,
                      new.row.names =paste0("p", 1:(nrow(data)*length(varying[[1]])) ))
  rownames(longdata)<-c()
  if(is.null(N.modalites2)) {
    for(i in 1:length(IV.levels)){
      N.modalites2<-c(N.modalites2, length(IV.levels[[i]]))
    }
  }

  if(length(IV.names)==1) {
    colnames(longdata)[which(colnames(longdata)=="time")]<-IV.names[[1]]
    longdata[,unlist(IV.names)]<-factor(longdata[,unlist(IV.names)])
  }else {
    for(i in 1:length(IV.names)){
      if(i==length(IV.names)){a<-1} else {
        a<-prod(N.modalites2[(i+1):length(IV.names)])
      }
      gl(n=N.modalites2[[i]], k=length(data[,1])*a, length=length(data[,1])*prod(N.modalites2),
         labels=IV.levels[[i]])->longdata$variable1
      names(longdata)<-c(names(longdata[1:(length(longdata)-1)]),IV.names[[i]])
    }
    longdata[,unlist(IV.names)]<-lapply(longdata[, unlist(IV.names)], factor)
  }


  return(longdata)
}















# Function from qualV written by K. Gerald van den Boogaart

LCS <- function (a, b) {
  m <- length(a)
  n <- length(b)
  if (m == 0 || n == 0) stop (.dico[["txt_vector_length_zero"]])

  # creates a table
  M <- matrix(nrow = m + 1, ncol = n + 1)
  M[, 1] <- 0
  M[1, ] <- 0

  # fills the table
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      if (a[i - 1] == b[j - 1]) { M[i, j] <- M[i - 1, j - 1] + 1 }
      else { M[i, j] <- max(c(M[i, j - 1], M[i - 1, j])) }
    }
  }

  # length of the longest common subsequence
  LLCS <- M[m + 1, n + 1]

  # determines one possible longest common subsequence
  # by means of "trace-back" by the table filled out
  i <- m + 1; j <- n + 1
  LCS <- va <- vb <- NULL
  while (i > 1 & j > 1) {
    if (M[i, j] == M[i - 1, j - 1] + 1 & a[i - 1] == b[j - 1]) {
      LCS <- c(a[i - 1], LCS)
      va <- c(i - 1, va); vb <- c(j - 1, vb)
      i <- i - 1; j <- j - 1
    }
    else {
      if (M[i - 1, j] > M[i, j - 1]) { i <- i - 1 }
      else { j <- j - 1 }
    }
  }

  # calculates the quality similarity index
  QSI <- round(LLCS / max(m, n), digits = 2)
  invisible(list(a = a, b = b, LLCS = LLCS, LCS = LCS, QSI = QSI , va = va, vb = vb))
}
