maths <-
  function(info=TRUE){
    options (warn=-1) 
    packages<-c("svDialogs")
    #faire l analyse par groupe # regler le probleme des noms
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)}
    list()->Results
    
    choice.data(nom=TRUE)->data1
    if(length(data1)==0) {return(preprocess())}
    data1[[1]]->nom1
    data1[[2]]->data
    if(info=="TRUE") writeLines("Veuillez  choisir l'operation mathematique que vous desirez realiser ")
    dlgList(c("additions","multiplication", "division", "soustraction","average of columns", "exponent or root", 
              "logarithm", "exponential","absolute value.,"complex model"), preselect="additions", multiple = FALSE, title="Which operation do you want?")$res->choice
    if(length(choice)==0) return(preprocess())
    
    variable<-function(multiple=TRUE){
      X<-dlgList(c(names(data), "to cancel"), multiple = multiple, title="Variable (s)")$res
      if(any(sapply(data[,X], class)=="factor")) {writeLines("at least one of the variables is not numeric")
        writeLines(str(data))
        return(maths())}
      return(X)}
    
    value<-function(info=TRUE, out=NULL){
      # info : logique pour determiner les informations relatives aux parametres doivent s'afficher dans la console
      # out : value renvoyee si value non numerique ou annulation
      if(info) writeLines("Please specify the value to perform your math operation.")
      msg<-"no"
      while(msg=="no" ){
        value1 <- dlgInput("What value do you want for your math operation?", out)$res 
        if(length(value1)!=0){
          strsplit(value1, ":")->value1
          if(class(value1)=="list") {  tail(value1[[1]],n=1)->value1}
          if(grepl("/",value1)) apply(sapply(strsplit(value1, split = "/"), as.numeric), 2, function(x) x[1] / x[2])->value1
          if(value1=="e") value1<-exp(1)
          as.numeric(value1)->value1
          msg<-"yes"} else return(out) 
        if(is.na(value1) ) { dlgMessage("the value you entered is not numeric. Do you want to cancel this scan?", "yesno")$res->msg
          if(msg=="yes") return(out)}
        
      }
      return(value1)
    }
    nom<-function(data,info, nom1){
      if(info=="TRUE") writeLines("What name do you want to give to the new variable? ")
      variable<-dlgInput("Name of the new variable?","new.variable")$res
      if(length(variable)==0) variable<-"new.variable"
      strsplit(variable, ":")->variable
      tail(variable[[1]],n=1)->variable
      if(grepl("[^[:alnum:]]", variable)) {
      writeLines("Unauthorized characters were used for the name. These characters have been replaced by periods")
      gsub("[^[:alnum:]]", ".", variable)->variable
}
      
      
      names(data)<-c(names(data)[1:(length(data)-1)], variable)
      assign(nom1, data, envir=.GlobalEnv)
      Results<-paste("The variable", variable, "has been added to", nom1)
      return(Results)}
    
    if(choice=="additions") {
      if(info=="TRUE") writeLines("Si vous selectionnez les deux options en meme temps, la value specifiee sera ajoutee a l'ensemble des colonnes choisies 
                                  et ensuite les colonnes choisies seront additionnees. Pour additionner une value specifique au total,
                                  veuillez choisir l'option addition of columns uniquement.")
      dlgList(c("addition of columns","addition of a specific value"), preselect="addition of columns", multiple = TRUE, title="Which operation do you want?")$res->choice2
      if(length(choice2)==0) return(maths())
      if(any(choice2== "addition of a specific value")){
        variable()->X
        if(length(X)==0|| any(X=="to cancel")) return(maths())
        value(info=info)->value1
        if(is.null(value1)) return(maths())
        data.frame(data, data[,X]+value1)->data
        if(value1>0)      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "plus", value1, sep=".") else names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "less", abs(value1), sep=".")
        assign(nom1, data, envir=.GlobalEnv)
        paste(value1, "has been added to the variable", X)->Results
      }
      
      if(any(choice2== "addition of columns")) {
        if(info=="TRUE") writeLines("Please select the variables to add.")
        variable()->X
        if(length(X)==0|| any(X=="to cancel")) return(maths())
        X->X1
        X2<-X1[1]
        X1[-1]->X1
        while(length(X1)!=0){paste(X2,"+",X1[1])->X2
          X1[-1]->X1}
        rowSums(data[,X])->data$new.variable
        if(info=="TRUE") writeLines("You can still add a specific value to the total. Leave 0 if you don't want to add anything")    
        value(info=info, out=0)->value1
        if(value1!=0) {data$new.variable+value1->data$new.variable
          paste(X2, "+", value1)->X2}
        writeLines(paste("you have performed the following operation:", X2))
        writeLines("do you still want to add a value to the total?")
        nom(data=data, info=info,nom1=nom1)->Results
      }
    }
    
    if(choice=="multiplication"){
      if(info=="TRUE") writeLines("Si vous selectionnez les deux options en meme temps, la value specifiee sera multipliee a l'ensemble des colonnes choisies 
                                  et ensuite les colonnes choisies seront multipliees Between elles. Pour multiplier une value specifique au total,
                                  veuillez choisir l'option multipication de colonnes uniquement.")
      dlgList(c("multiplication of columns","multiplication of a specific value"), preselect="multiplication of columns", multiple = TRUE, title="Which operation do you want?")$res->choice2
      if(length(choice2)==0) return(maths())
      if(any(choice2== "multiplication of a specific value")){
        if(info=="TRUE") writeLines("Please select the variables to be multiplied. ")
        variable()->X
        if(length(X)==0|| any(X=="to cancel")) return(maths())
        value(info=info, out=NULL)->value1
        if(is.null(value1)) return(maths())
        data.frame(data, data[,X]*value1)->data
        names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "multiplied by", value1, sep=".")
        assign(nom1, data, envir=.GlobalEnv)
        paste(value1, "a multiplies the -variable-s", X)->Results
      }
      
      if(any(choice2== "multiplication of columns")) {
        variable()->X
        if(length(X)==0|| any(X=="to cancel")) return(maths())
        
        X->X1
        X2<-X1[1]
        X1[-1]->X1
        while(length(X1)!=0){paste(X2,"*",X1[1])->X2
          X1[-1]->X1}
        1*data[,X[1]]->nouvelle
        for(i in 1:(length(X)-1)) nouvelle*data[,X[i+1]]->nouvelle
        data.frame(data, nouvelle)->data
        
        if(info=="TRUE") writeLines("You can still multiply the total by a specific value. Leave 1 if you no longer want to multiply by a new value")    
        value(info=info, out=1)->value1
        if(value1!=1) {data$nouvelle*value1->data$nouvelle
          paste(X2, "*", value1)->X2}
        writeLines(paste("you have performed the following operation:", X2))
        nom(data=data, info=info,nom1=nom1)->Results
      }
    }
    if(choice=="division"){
      if(info=="TRUE") writeLines("Is the numerator a variable or a value? ")
      numer<-dlgList(c("value", "variable"), multiple = FALSE, title="Numerator")$res
      if(length(numer)==0) return(maths())
      if(numer=="value") value(info=info, out=1)->X else{
        if(info=="TRUE") writeLines("Please select the variable in the numerator ")
        variable(multiple=FALSE)->X
        if(length(X)==0|| any(X=="to cancel")) return(maths())
        data[,X]->X
      }
      
      if(info=="TRUE") writeLines("Is the denominator a variable or a value? ")
      denom<-dlgList(c("value", "variable"), multiple = FALSE, title="Denominator")$res
      if(length(denom)==0) return(maths())
      if(denom=="value") value(info=info, out=1)->Y else{
        if(info=="TRUE") writeLines("Please select the variable in the denominator ")
        variable(multiple=FALSE)->Y
        if(length(X)==0|| any(X=="to cancel")) return(maths())
        data[,Y]->Y
        if(any(Y)==0) writeLines("At least one of the denominator values is 0. The return value in this case is infinite - inf")
      }
      X/Y->data$new.variable
      nom(data=data, info=info,nom1=nom1)->Results
    }
    
    if(choice=="soustraction") {
      if(info=="TRUE") writeLines("Veuillez selectionner les values situees a gauche du symbole *less*. Si plusieurs variables sont selectionnees, 
                                  les regles du calcul matriciel sont appliques.")
      if(info=="TRUE") writeLines("Les values positives sont-elles une/des variable(s) ou une value ? ")
      numer<-dlgList(c("value", "variable"), multiple = FALSE, title="Positive values")$res
      if(length(numer)==0) return(maths())
      if(numer=="value") value(info=info, out=0)->X else{
        if(info=="TRUE") writeLines("Veuillez selectionner la -les- variable(s) a gauche du symbole *less*")
        variable(multiple=TRUE)->X
        if(length(X)==0|| any(X=="to cancel")) return(maths())
        data[,X]->X1
        data.frame(X1)->X1
      }
      
      if(info=="TRUE") writeLines("Les values a droite du symbole *less* sont-elles une/des variable(s) ou une value  ? ")
      denom<-dlgList(c("value", "variable"), multiple = FALSE, title="Negative values")$res
      if(length(denom)==0) return(maths())
      if(denom=="value") value(info=info, out=0)->Y else{
        if(info=="TRUE") writeLines("Veuillez selectionner la -les- variable(s) a droite du symbole *less*.")
        Y<-NULL
        while(is.null(Y)){
          variable(multiple=TRUE)->Y
          if(length(Y)==0|| any(Y=="to cancel")) return(maths())
          data[,Y]->Y1
          data.frame(Y1)->Y1 
          if(length(X1)!=1 & length(Y1)!=1 & length(X1)!=length(Y1)) {
            writeLines("Il ne doit y avoir qu'une colonne ou le nombre de colonnes a droite du symbole *less* doit etre egal 
                       au nombre de colonnes a gauche du symbole *less*")
            Y<-NULL} else Y<-Y
        }
        }
      X1-Y1->new.var
      names(new.var)<-paste0(X, ".less.", Y)
      data<-data.frame(data, new.var)
      assign(nom1, data, envir=.GlobalEnv)
      #nom(data=data, info=info,nom1=nom1)->Results
      Results<-"The mathematical operation took place correctly."
      }
    
    if(choice=="average of columns")  {
      if(info=="TRUE") writeLines("Please select the variables to be averaged ")
      X<-variable()
      if(length(X)==0|| any(X=="to cancel")) return(maths())
      rowMeans(data[,X])->data$new.variable
      nom(data=data, info=info,nom1=nom1)->Results
    }
    if(choice== "exponent or root"){
      if(info=="TRUE") writeLines("Please select the variables to which the exponent applies ")
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X=="to cancel")) return(maths())
      if(info=="TRUE") writeLines("Veuillez preciser la value de l'exhibitor. 
                                  NOTE : Pour les racines, l'exhibitor est l'inverse la value. Par exemple, La racine carree vaut 1/2, la racine cubique 1/3... ")
      value(info=info)->Y
      if(class(Y)!="numeric") {writeLines("the entered value is not numeric")
        return(maths())}
      data.frame(data, data[,X]^Y)->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "exhibitor", Y, sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste("the variable", X, " was raised to power", Y)->Results
      
    }
    if(choice== "logarithm"){
      if(info=="TRUE") writeLines("Please select the variables to be logarithmd ")
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X=="to cancel")) return(maths())
      if(info=="TRUE") writeLines("Please specify the base of the logarithm, to get e, type e")
      value(info=info)->Y
      if(class(Y)!="numeric") {writeLines("the entered value is not numeric")
        return(maths())}
      if(Y<0) {writeLines("it is not possible to calculate logarithms for a base is negative. NA is fired")
        return(maths()) }
      data.frame(data, log(data[,X], base=Y))->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("log.", X,  sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste("the base logarithm", Y, " has been applied to the variable", X)->Results
    }
    if(choice== "exponential"){
      if(info=="TRUE") writeLines("Please select the variables used for the exponential ")
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X=="to cancel")) return(maths())
      data.frame(data, exp(data[,X]))->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("exp.", X,  sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste("the exponential has been applied to the variable", X)->Results
    }
    if(choice== "absolute value.){
      if(info=="TRUE") writeLines("Please select the variables to be the absolute value ")
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X=="to cancel")) return(maths())
      data.frame(data, abs(data[,X]))->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("absolute value.", X,  sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste("the absolute value has been applied to the variable", X)->Results
    }
    if(choice== "complex model"){
      writeLines("L'expression doit etre correcte. Vous pouvez utiliser directement le nom des variables
                 les operateurs sont +,-,*,/,^,(,). Une expression correcte serait :")
      print(paste(names(data)[1],"^2+5"), quote=FALSE)
      print(names(data))
      value1 <- dlgInput("Please specify the model to be produced")$res 
      if(length(value1)==0) return(maths())
      strsplit(value1, ":")->value1
      tail(value1[[1]],n=1)->value1
      try(eval(parse(text=value1), envir=data), silent=TRUE)->nouvelle
      if(class(nouvelle)=="try-error") {writeLines("The model cannot be evaluated. It must contain an error")
        return(maths())} else nouvelle->data$nouvelle
      
      nom(data=data,info=info, nom1=nom1)->Results
      
    }
    View(data)
    return(Results)
    }
