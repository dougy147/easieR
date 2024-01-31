maths <-
  function(info=TRUE){
    options (warn=-1) 
    packages<-c("svDialogs")
    #faire l analyse par groupe # regler le probleme des noms
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)}
    list()->Resultats
    
    choix.data(nom=TRUE)->data1
    if(length(data1)==0) {return(preprocess())}
    data1[[1]]->nom1
    data1[[2]]->data
    if(info=="TRUE") writeLines("Veuillez  choisir l'operation mathematique que vous desirez realiser ")
    dlgList(c(TXT_additions,TXT_multiplication, "division", TXT_substraction,TXT_col_mean, TXT_exponant_or_root, 
              TXT_logarithm, TXT_exponential,TXT_absolute_value,TXT_complex_model), preselect=TXT_additions, multiple = FALSE, title=ASK_which_operation)$res->choix
    if(length(choix)==0) return(preprocess())
    
    variable<-function(multiple=TRUE){
      X<-dlgList(c(names(data), TXT_cancel), multiple = multiple, title=TXT_variables)$res
      if(any(sapply(data[,X], class)=="factor")) {writeLines(INFO_at_least_one_var_is_not_num)
        writeLines(str(data))
        return(maths())}
      return(X)}
    
    valeur<-function(info=TRUE, out=NULL){
      # info : logique pour determiner les informations relatives aux parametres doivent s'afficher dans la console
      # out : valeur renvoyee si valeur non numerique ou annulation
      if(info) writeLines(ASK_value_for_operation)
      msg<-"no"
      while(msg=="no" ){
        valeur1 <- dlgInput(ASK_which_value_for_operation, out)$res 
        if(length(valeur1)!=0){
          strsplit(valeur1, ":")->valeur1
          if(class(valeur1)=="list") {  tail(valeur1[[1]],n=1)->valeur1}
          if(grepl("/",valeur1)) apply(sapply(strsplit(valeur1, split = "/"), as.numeric), 2, function(x) x[1] / x[2])->valeur1
          if(valeur1=="e") valeur1<-exp(1)
          as.numeric(valeur1)->valeur1
          msg<-"yes"} else return(out) 
        if(is.na(valeur1) ) { dlgMessage(ASK_cancel_entered_value_not_num, "yesno")$res->msg
          if(msg=="yes") return(out)}
        
      }
      return(valeur1)
    }
    nom<-function(data,info, nom1){
      if(info=="TRUE") writeLines(ASK_new_variable_name)
      variable<-dlgInput(ASK_variable_name,"nouvelle.variable")$res
      if(length(variable)==0) variable<-"nouvelle.variable"
      strsplit(variable, ":")->variable
      tail(variable[[1]],n=1)->variable
      if(grepl("[^[:alnum:]]", variable)) {
      writeLines(INFO_unauthorized_char_replaced)
      gsub("[^[:alnum:]]", ".", variable)->variable
}
      
      
      names(data)<-c(names(data)[1:(length(data)-1)], variable)
      assign(nom1, data, envir=.GlobalEnv)
      Resultats<-paste(INFO_the_variable_upper, variable, INFO_has_been_added_to, nom1)
      return(Resultats)}
    
    if(choix==TXT_additions) {
      if(info=="TRUE") writeLines("Si vous selectionnez les deux options en meme temps, la valeur specifiee sera ajoutee a l'ensemble des colonnes choisies 
                                  et ensuite les colonnes choisies seront additionnees. Pour additionner une valeur specifique au total,
                                  veuillez choisir l'option addition de colonnes uniquement.")
      dlgList(c(TXT_add_of_cols,TXT_add_of_specific_value), preselect=TXT_add_of_cols, multiple = TRUE, title=ASK_which_operation)$res->choix2
      if(length(choix2)==0) return(maths())
      if(any(choix2== TXT_add_of_specific_value)){
        variable()->X
        if(length(X)==0|| any(X==TXT_cancel)) return(maths())
        valeur(info=info)->valeur1
        if(is.null(valeur1)) return(maths())
        data.frame(data, data[,X]+valeur1)->data
        if(valeur1>0)      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, TXT_plus, valeur1, sep=".") else names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, TXT_minus, abs(valeur1), sep=".")
        assign(nom1, data, envir=.GlobalEnv)
        paste(valeur1, INFO_has_been_added_to_variable, X)->Resultats
      }
      
      if(any(choix2== TXT_add_of_cols)) {
        if(info=="TRUE") writeLines(ASK_variables_to_add)
        variable()->X
        if(length(X)==0|| any(X==TXT_cancel)) return(maths())
        X->X1
        X2<-X1[1]
        X1[-1]->X1
        while(length(X1)!=0){paste(X2,"+",X1[1])->X2
          X1[-1]->X1}
        rowSums(data[,X])->data$nouvelle_variable
        if(info=="TRUE") writeLines(INFO_you_can_still_add)    
        valeur(info=info, out=0)->valeur1
        if(valeur1!=0) {data$nouvelle_variable+valeur1->data$nouvelle_variable
          paste(X2, "+", valeur1)->X2}
        writeLines(paste(INFO_you_did_this_operation, X2))
        writeLines(ASK_add_value_to_total)
        nom(data=data, info=info,nom1=nom1)->Resultats
      }
    }
    
    if(choix==TXT_multiplication){
      if(info=="TRUE") writeLines("Si vous selectionnez les deux options en meme temps, la valeur specifiee sera multipliee a l'ensemble des colonnes choisies 
                                  et ensuite les colonnes choisies seront multipliees entre elles. Pour multiplier une valeur specifique au total,
                                  veuillez choisir l'option multipication de colonnes uniquement.")
      dlgList(c(TXT_cols_multiplication,TXT_specific_val_multiplication), preselect=TXT_cols_multiplication, multiple = TRUE, title=ASK_which_operation)$res->choix2
      if(length(choix2)==0) return(maths())
      if(any(choix2== TXT_specific_val_multiplication)){
        if(info=="TRUE") writeLines(ASK_variables_to_multiply)
        variable()->X
        if(length(X)==0|| any(X==TXT_cancel)) return(maths())
        valeur(info=info, out=NULL)->valeur1
        if(is.null(valeur1)) return(maths())
        data.frame(data, data[,X]*valeur1)->data
        names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "multiplie.par", valeur1, sep=".")
        assign(nom1, data, envir=.GlobalEnv)
        paste(valeur1, INFO_has_multiplied_variables, X)->Resultats
      }
      
      if(any(choix2== TXT_cols_multiplication)) {
        variable()->X
        if(length(X)==0|| any(X==TXT_cancel)) return(maths())
        
        X->X1
        X2<-X1[1]
        X1[-1]->X1
        while(length(X1)!=0){paste(X2,"*",X1[1])->X2
          X1[-1]->X1}
        1*data[,X[1]]->nouvelle
        for(i in 1:(length(X)-1)) nouvelle*data[,X[i+1]]->nouvelle
        data.frame(data, nouvelle)->data
        
        if(info=="TRUE") writeLines(INFO_you_can_still_multiply)    
        valeur(info=info, out=1)->valeur1
        if(valeur1!=1) {data$nouvelle*valeur1->data$nouvelle
          paste(X2, "*", valeur1)->X2}
        writeLines(paste(INFO_you_did_this_operation, X2))
        nom(data=data, info=info,nom1=nom1)->Resultats
      }
    }
    if(choix=="division"){
      if(info=="TRUE") writeLines(ASK_numerator_variable_or_value)
      numer<-dlgList(c(TXT_value, TXT_variable), multiple = FALSE, title=TXT_numerator)$res
      if(length(numer)==0) return(maths())
      if(numer==TXT_value) valeur(info=info, out=1)->X else{
        if(info=="TRUE") writeLines(ASK_numerator_variable)
        variable(multiple=FALSE)->X
        if(length(X)==0|| any(X==TXT_cancel)) return(maths())
        data[,X]->X
      }
      
      if(info=="TRUE") writeLines(ASK_denominator_variable_or_value)
      denom<-dlgList(c(TXT_value, TXT_variable), multiple = FALSE, title=TXT_denominator)$res
      if(length(denom)==0) return(maths())
      if(denom==TXT_value) valeur(info=info, out=1)->Y else{
        if(info=="TRUE") writeLines(ASK_denominator_variable)
        variable(multiple=FALSE)->Y
        if(length(X)==0|| any(X==TXT_cancel)) return(maths())
        data[,Y]->Y
        if(any(Y)==0) writeLines(INFO_at_least_one_denom_is_zero)
      }
      X/Y->data$nouvelle_variable
      nom(data=data, info=info,nom1=nom1)->Resultats
    }
    
    if(choix==TXT_substraction) {
      if(info=="TRUE") writeLines("Veuillez selectionner les valeurs situees a gauche du symbole *moins*. Si plusieurs variables sont selectionnees, 
                                  les regles du calcul matriciel sont appliques.")
      if(info=="TRUE") writeLines(ASK_positive_val_variable_or_value)
      numer<-dlgList(c(TXT_value, TXT_variable), multiple = FALSE, title=TXT_positive_values)$res
      if(length(numer)==0) return(maths())
      if(numer==TXT_value) valeur(info=info, out=0)->X else{
        if(info=="TRUE") writeLines("Veuillez selectionner la -les- variable(s) a gauche du symbole *moins*")
        variable(multiple=TRUE)->X
        if(length(X)==0|| any(X==TXT_cancel)) return(maths())
        data[,X]->X1
        data.frame(X1)->X1
      }
      
      if(info=="TRUE") writeLines("Les valeurs a droite du symbole *moins* sont-elles une/des variable(s) ou une valeur  ? ")
      denom<-dlgList(c(TXT_value, TXT_variable), multiple = FALSE, title=TXT_negative_values)$res
      if(length(denom)==0) return(maths())
      if(denom==TXT_value) valeur(info=info, out=0)->Y else{
        if(info=="TRUE") writeLines("Veuillez selectionner la -les- variable(s) a droite du symbole *moins*.")
        Y<-NULL
        while(is.null(Y)){
          variable(multiple=TRUE)->Y
          if(length(Y)==0|| any(Y==TXT_cancel)) return(maths())
          data[,Y]->Y1
          data.frame(Y1)->Y1 
          if(length(X1)!=1 & length(Y1)!=1 & length(X1)!=length(Y1)) {
            writeLines("Il ne doit y avoir qu'une colonne ou le nombre de colonnes a droite du symbole *moins* doit etre egal 
                       au nombre de colonnes a gauche du symbole *moins*")
            Y<-NULL} else Y<-Y
        }
        }
      X1-Y1->new.var
      names(new.var)<-paste0(X, ".moins.", Y)
      data<-data.frame(data, new.var)
      assign(nom1, data, envir=.GlobalEnv)
      #nom(data=data, info=info,nom1=nom1)->Resultats
      Resultats<-INFO_operation_succesful
      }
    
    if(choix==TXT_col_mean)  {
      if(info=="TRUE") writeLines(ASK_variables_to_mean)
      X<-variable()
      if(length(X)==0|| any(X==TXT_cancel)) return(maths())
      rowMeans(data[,X])->data$nouvelle_variable
      nom(data=data, info=info,nom1=nom1)->Resultats
    }
    if(choix== TXT_exponant_or_root){
      if(info=="TRUE") writeLines(ASK_variables_to_exp)
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X==TXT_cancel)) return(maths())
      if(info=="TRUE") writeLines("Veuillez preciser la valeur de l'exposant. 
                                  NOTE : Pour les racines, l'exposant est l'inverse la valeur. Par exemple, La racine carree vaut 1/2, la racine cubique 1/3... ")
      valeur(info=info)->Y
      if(class(Y)!="numeric") {writeLines(INFO_entered_value_not_num)
        return(maths())}
      data.frame(data, data[,X]^Y)->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, TXT_exponant, Y, sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste(INFO_the_variable_lower, X, INFO_has_been_put_to_the_power_of, Y)->Resultats
      
    }
    if(choix== TXT_logarithm){
      if(info=="TRUE") writeLines(ASK_variables_to_log)
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X==TXT_cancel)) return(maths())
      if(info=="TRUE") writeLines(ASK_log_base)
      valeur(info=info)->Y
      if(class(Y)!="numeric") {writeLines(INFO_entered_value_not_num)
        return(maths())}
      if(Y<0) {writeLines(INFO_neg_log_impossible)
        return(maths()) }
      data.frame(data, log(data[,X], base=Y))->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("log.", X,  sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste(INFO_log_with_base, Y, INFO_has_been_applied_to_variable, X)->Resultats
    }
    if(choix== TXT_exponential){
      if(info=="TRUE") writeLines(ASK_variables_used_for_exponential)
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X==TXT_cancel)) return(maths())
      data.frame(data, exp(data[,X]))->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("exp.", X,  sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste(INFO_exponential_has_been_applied_to_var, X)->Resultats
    }
    if(choix== TXT_absolute_value){
      if(info=="TRUE") writeLines(ASK_variables_to_abs)
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X==TXT_cancel)) return(maths())
      data.frame(data, abs(data[,X]))->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("valeur.absolue.", X,  sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste(INFO_abs_val_applied_to_var, X)->Resultats
    }
    if(choix== TXT_complex_model){
      writeLines("L'expression doit etre correcte. Vous pouvez utiliser directement le nom des variables
                 les operateurs sont +,-,*,/,^,(,). Une expression correcte serait :")
      print(paste(names(data)[1],"^2+5"), quote=FALSE)
      print(names(data))
      valeur1 <- dlgInput(ASK_model)$res 
      if(length(valeur1)==0) return(maths())
      strsplit(valeur1, ":")->valeur1
      tail(valeur1[[1]],n=1)->valeur1
      try(eval(parse(text=valeur1), envir=data), silent=TRUE)->nouvelle
      if(class(nouvelle)=="try-error") {writeLines(INFO_model_contains_error)
        return(maths())} else nouvelle->data$nouvelle
      
      nom(data=data,info=info, nom1=nom1)->Resultats
      
    }
    View(data)
    return(Resultats)
    }
