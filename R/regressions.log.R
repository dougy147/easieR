regressions.log <-
  function(data=NULL, model=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=T, select.m="none", step=NULL, group=NULL, scale=T, dial=T, info=T,
           sauvegarde=F,proba=F, html=T){
    
    logisticPseudoR2s <- function(LogModel) {
      dev <- LogModel$deviance
      nullDev <- LogModel$null.deviance
      modelN <-  length(LogModel$fitted.values)
      R.l <-  1 -  dev / nullDev
      R.cs <- 1- exp ( -(nullDev - dev) / modelN)
      R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
      return(c(round(R.l, 3),round(R.cs, 3),round(R.n, 3)))
    } 
    reg.log.in<-function(data=NULL, model=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=T, select.m="none", step=NULL, group=NULL, scale=T, dial=T, info=T,
                         sauvegarde=F,proba=F){
      
      options (warn=-1) 
      Results<-list()
      if(is.null(data) | is.null(model))  {dial<-TRUE}else dial<-F 
      data<-choice.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL) 
      nom<-data[[1]]
      data<-data[[2]]  
      
      
      if(dial && is.null(model)){
        if(info) writeLines("Veuillez choisir le(s) type(s) de relations Between les variables. Les effets additifs prennent la forme de
                            y=X1+X2 tandis que les effets d'interaction prennent la forme de Y=X1+X2+X1:X2")
        dlgList(c("Additive effects", "Interaction effects", "Specify the model"), preselect="Regressions", multiple = TRUE, title="What kind of regression?")$res->link
        if(length(link)==0) return(NULL)} else link<-"none"
      
      if(length(Y)>1){
        msgBox("There can only be one dependent variable.")
        Y<-NULL }
      if(any(link %in% c("Additive effects", "Interaction effects"))){
        msg3<-"Please choose the dependent variable."
        Y<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=FALSE, title="Dependent variable", out=NULL)
        if(is.null(Y)) {
          reg.log.in()->Results
          return(Results)}
        data<-Y$data
        Y<-Y$X
        
        if(length(unique(data[,Y]))!=2) {
          msg1<-paste("Your veriable dependent has", length(unique(data[,Y])), "terms and conditions. It is incompatible with a logistic regression. She must be dichotomous" )
          msgBox(msg1)
          if(class(data[,Y]) %in%c("numeric","integer")){
            dlgMessage("voulez-vous convertir the variable dependante en une variable dichotomique,  ?","yesno")$res->conv
            
            if(conv=="no") return(reg.log.in())  else{
              if(info) writeLines("Please specify the criterion on which you want to dichotomize your variable. You can use the median or choose a specific threshold.")
              dlgList(c("Median", "Threshold"), preselect="Median", multiple = FALSE, title="What coding criteria do you want?")$res->codage
              if(length(codage)==0) return(reg.log.in())
              if(codage=="Median") data[,Y]<-ifelse(data[,Y]>median(data[,Y]),1, 0)
              View(data)
              readline()
              if(codage=="Threshold") {
                seuil<-NA
                while(is.na(seuil)){
                  seuil<-dlgInput("Please specify the separation value", median(data[,Y]))$res 
                  if(length(seuil)==0) return(reg.log.in())
                  strsplit(seuil, ":")->seuil
                  tail(seuil[[1]],n=1)->seuil
                  as.numeric(seuil)->seuil
                  if(is.na(seuil) || seuil>max(data[,Y]) || seuil<min(data[,Y])) {msgBox("The value must be numeric and between the minimum and the maximum of the dependent variable.")
                    Y<-NA}
                }
                data[,Y]<-ifelse(data[,Y]>seuil,1, 0)
                
              } # seuil
            }
          }
          if(class(data[,Y]) %in%c("factor","character")){
            dlgMessage("Do you want to make groupings between the modalities?","yesno")$res->reg
            if(reg=="no") return(reg.log.in()) else {
              if(info) writeLines("Veuillez specifier la/les modality(s) qui serviront pour la ligne de base (e.g. 0). Les autres modalitys seront regroupes dans la categorie 1.")
              reg<- dlgList(levels(data[,Y]), preselect=NULL, multiple = TRUE, title="Modalities to be regrouped")$res
              setdiff(levels(data[,Y]),reg)->reste
              data[,Y]<-ifelse(data[,Y]%in%reg, 0,1) 
              data[,Y]<-factor(data[,Y])
            }
          }      
        }
        
        
        if(any(link=="Additive effects") || !is.null(X_a)| any(X_a %in% names(data)==F)) {
          msg3<-"Please choose the dependent variable."
          X_a<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title="Additive model variables", out=Y)
          if(is.null(X_a)) {
            reg.log.in()->Results
            return(Results)}
          data<-X_a$data
          X_a<-X_a$X
          
        }else X_a<-NULL 
        
        if(any(link=="Interaction effects") || !is.null(X_i) & (length(X_i)<2 | any(X_i %in% names(data)==F))) {
          msg3<-"Please choose the predictors to enter in the interaction model. It is necessary to have at least two variables"
          X_i<-c()
          while(length(X_i)<2){
            X_i<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title="Interactive model variables", out=c(X_a,Y))
            if(is.null(X_i)) {
              reg.log.in()->Results
              return(Results)}
            data<-X_i$data
            X_i<-X_i$X
          }
        }else X_i<-NULL
        
        
        
        paste0(Y," ~ ")->model
        if(!is.null(X_a ))  {
          X_a.mod<-X_a[1]
          if(length(X_a)>1) for(i in 2 : length(X_a)) paste0(X_a.mod, "+", X_a[i])-> X_a.mod
        } else X_a.mod<-NULL
        
        if(!is.null(X_i)){
          X_i.mod<-X_i[1]
          if(length(X_i)>1) for(i in 2 : length(X_i)) paste0(X_i.mod, "*", X_i[i])-> X_i.mod
        } else X_i.mod<-NULL
        
        if(!is.null(X_a.mod) & !is.null(X_i.mod)) {
          paste0(model, X_a.mod, "+", X_i.mod)->model
        } else paste0(model, X_a.mod, X_i.mod)->model
        
      }
      
      
      
      if(any(link=="Specify the model")) model<-fix(model)
      variables<-terms(as.formula(model))
      variables<-as.character( attributes(variables)$variables)[-1]
      pred<-attributes(terms(as.formula(model)))$term.labels
      if(any(ftable(data[,which(lapply(sapply(data,unique),length)<5)])<3)) {
        msgBox("There are insufficient observations (<3) for certain combinations of levels of the variables of the model")
        return(ftable(data[,which(lapply(sapply(data,unique),length)<5)]))
      }
      if(dial){
        if(length(pred>1)){
          pred.ord<-c()
          while(length(pred)!=0){
            if(info)  writeLines("L'ordre d'Betweene des variables est important pour le calcul du maximum likelihood. Veuillez
                                 preciser l'ordre d'Betweene des variables") 
            V1<-dlgList(pred, multiple = FALSE,title="Which variable at this stage")$res
            c(pred.ord,V1)->pred.ord
            setdiff(pred,V1)->pred}
        }else pred.ord<-pred
        
        paste0(Y," ~ ", pred.ord[1])->model
        if(length(pred.ord)>1) for(i in 2 : length(pred.ord)) paste0(model, "+", pred.ord[i])-> model
        model<-as.formula(model)}
      
      model.test<-try(model.matrix(model, data), silent=T)
      if(class(model.test)=="try-error") {
        msgBox("The model specified is incorrect. Check your variables and your model")
        return(reg.log.in())
      }
      
      
      data[complete.cases(data[,variables]),]->data
      options<-.ez.options(options=c("outlier"), n.boot=NULL,param=F, non.param=F, robust=F, Bayes=F, msg.options1=NULL, msg.options2=NULL, info=info, dial=dial, 
                           choice=NULL, backup =sauvegarde, outlier=outlier, rscale=NULL)
      if(is.null(options)) return(reg.log.in())
      
      reg.options<- .regressions.options(data=data, model=model, CV=FALSE, inf=inf, select.m=select.m, method=NULL, criteria=NULL, step=step, group=group, scale=scale, dial=dial,info=info)
      if(is.null(reg.options)) return(reg.log.in())
      
      if(dial){
        if(info) writeLines('voulez-vous integrer les probabilities a votre base de donnees ?')
        dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title="Probabilities?")$res->proba
        
      }
      
      Results$proba<-proba
      Results$data<-data
      Results$nom<-nom
      Results$model<-model
      Results$options<-options
      Results$reg.options<-reg.options
      return(Results)   
      
    }
    
    reg.log.out<-function(data=NULL, model=NULL,  select.m="none", step=NULL, scale=T, nom=NULL,proba=F){
      
      Results<-list()
      variables<-terms(as.formula(model))
      variables<-as.character( attributes(variables)$variables)[-1]
      pred<-attributes(terms(as.formula(model)))$term.labels
      Results$"Descriptive statistics"<-.stat.desc.out(X=variables, groupes=NULL, data=data, tr=.1, type=3, plot=T)
      
      if(scale==T || scale=="Center") {Results$info<-"In accordance with the recommendations of Schielzeth 2010, the data were previously centered"
      fun<-function(X){X-mean(X)}
      variables[-1]->pred2
      sapply(X=data[, names(which(sapply(data[,pred2],class)!="factor"))], fun)->data[,names(which(sapply(data[,pred2],class)!="factor"))]
      }
      if(class(data[,variables[1]])=="character") factor(data[,variables[1]])->data[,variables[1]]
      
      if(!is.null(step)){
        
        as.formula(paste0(variables[1]," ~ ",step[[1]][1]))->model.H
        list()->model.H1
        list()->formule.H1
        for(i in 1:length(step)){
          
          for(j in 1:length(step[[i]])){update(model.H, as.formula(paste0(".~. + ",step[[i]][j])))->model.H}
          formule.H1[[i]]<-model.H
          glm(model.H, data=data, na.action=na.exclude , family="binomial")->lm.H
          lm.H->model.H1[[i]]}
        
        hier<-paste0("anova(model.H1[[1]],model.H1[[2]]")
        if(length(model.H1)>2){
          for(i in 3: length(model.H1)){
            hier<-paste0(hier, ",model.H1[[", i, "]]")
          }
        }
        hier<-paste0(hier,")")
        hier<-eval(parse(text=hier))
        
        attributes(hier)$heading[1]<-"Hierarchical models deviance analysis table"
        round(1-pchisq(hier$Deviance,hier$Df,lower.tail=F),4)->hier$p-value
        names(hier)<-c("dof.resid", "Deviance.resid","dof.effect", "Deviance", "p-value")
        Results$"Hierarchical analysis of models "<-hier
      }
      
      
      
      
      
      mod<-list()
      model1<-as.formula(paste0(variables[1], "~", pred[1]))
      glm(model1, data=data, family="binomial")->glm.r1
      mod <- list() 
      glm.r1->mod[[1]] 
      if(length(pred)>1) {
        for(i in 2:length(pred)){update(glm.r1, as.formula(paste0(".~.+",pred[i])))->glm.r1
          glm.r1->mod[[i]]}
      }
      
      anova(mod[[length(mod)]])->Amelioration_du_MV
      
      summary(mod[[length(mod)]])->resultats
      as(resultats$call,"character")->texte
      paste("the model tested is" , texte[2])->Results$"Model tested"
      
      cbind(rms::vif(mod[[length(mod)]]), 1/rms::vif(mod[[length(mod)]]))->MC
      dimnames(MC)[[2]]<-c("Variance inflation factor", "Tolerance")
      round(MC,4)->Results$"Multicolinearity test"
      
      sum(Amelioration_du_MV$Df[2:length(Amelioration_du_MV$Df)])->dof
      Amelioration_du_MV$`Resid. Dev`[1]-Amelioration_du_MV$`Resid. Dev`[length(Amelioration_du_MV$`Resid. Dev`)]->chi.carre.model
      round(1-pchisq(chi.carre.model,dof),4)->p-value
      logisticPseudoR2s(mod[[length(mod)]])->Pseudo.R.carre
      data.frame(chi.carre.model, dof, p-value,Pseudo.R.carre[1],Pseudo.R.carre[2],Pseudo.R.carre[3])->mod.glob
      names(mod.glob)<-c("chi.2.model", "dof", "p-value","Hosmer and Lemeshow R ^ 2","Cox and Snell R^2","Nagelkerke R^2")
      mod.glob->Results$"Significance of the global model"
      
      
      Amelioration_du_MV$chi.deux.prob<-1-pchisq(Amelioration_du_MV$Deviance, Amelioration_du_MV$Df)
      round(Amelioration_du_MV,4)->Amelioration_du_MV
      names(Amelioration_du_MV)<-c("predictor dof", "MV","dof.residueels","Residual MV","p-value")
      Results$"Improved likelihood for each variable"<-data.frame(Amelioration_du_MV)
      
      data.frame(resultats$coefficients)->table
      (table$z.value)^2->table$Wald.statistic
      exp(table$Estimate)->table$Odd.Ratio
      round(table,4)->table
      names(table)<-c("b","Standard.error","value.Z","p.Wald", "Wald","Odd.ratio")
      cbind(table, round(exp(confint(mod[[length(mod)]])),4))->table
      table$interpretation<-ifelse(table$Odd.ratio>=1,paste(table$Odd.ratio, "times more"), paste(round(1/table$Odd.ratio,4), "times less"))
      table->Results$"Table of coefficients"
      
      R_sq<-NULL
      for(i in 1:length(mod)){logisticPseudoR2s(mod[[i]])->R_squared
        rbind(R_sq, R_squared)->R_sq}
      diff(R_sq,lag=1)->R_sq[2.]
      dimnames(R_sq)[[1]]<-pred
      dimnames(R_sq)[[2]]<-c("Hosmer and Lemeshow R ^ 2","Cox and Snell R^2","Nagelkerke R^2")
      R_sq->Results$"Delta of pseudo R square"
      
      if(proba=="TRUE")	{ 
        round(fitted(mod[[length(mod)]]),4)->data$"Predicted probabilities"
        head(data)
        print(nom)
        assign(x=nom, value=data, envir=.GlobalEnv)}
      
      if(select.m!="none"){
        select.m<-switch(select.m,"Forward - step by step ascending"="forward", "Backward - step by step descending"="backward", "Bidirectional"="both",
                         "forward"="forward", "bidirectional"="both","backward"="backward" )
        glm(model, data=data, family="binomial")->glm.r1
        
        steps<-stepAIC(glm.r1, direction=select.m) 
        Results$"Selection method - Akaike information criteria"<-steps$anova
        model<-as.formula(attributes(steps$anova)$heading[5])
      }
      
      return(Results)
      
      
      
    }
    
    
    c("boot","car","psych", "mlogit","svDialogs","rms","MASS")->packages
    if(class(data)=="data.frame") deparse(substitute(data))->data 
    options (warn=-1) 
    .e <- environment()
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    Results<-list() 
    reg.in.output<-reg.log.in(data=data, model=model, Y=Y, X_a=X_a, X_i=X_i, outlier=outlier, inf=inf, 
                              select.m=select.m,  step=step, group=group,  scale=scale, info=info, backup =sauvegarde, proba=proba)
    if(is.null(reg.in.output)) return(choice.reg())
    data<-reg.in.output$data
    nom<-reg.in.output$nom
    model<-reg.in.output$model
    outlier<-reg.in.output$options$desires
    sauvegarde<-reg.in.output$options$sauvegarde
    scale<-reg.in.output$reg.options$scale
    inf<-reg.in.output$reg.options$inf
    step<-reg.in.output$reg.options$step
    select.m<-reg.in.output$reg.options$select.m
    group<-reg.in.output$reg.options$group
    proba<-reg.in.output$proba
    
    if(!is.null(reg.in.output$reg.options$CV) && reg.in.output$reg.options$CV==TRUE) print("Cross validation is not yet available.")
    
    if(any(outlier==  "Complete data")){
      Results$"Complete data"<-  reg.log.out(data=data, model=model,  select.m=select.m, step=step, scale=scale, proba=proba, nom=nom)
      if(!is.null(group))   {  
        R1<-list()
        G<-data[,group]
        if(length(group)>1) G<-as.list(G)
        G<-split(data, G)
        for(i in 1:length(G)){
          resg<-  try(reg.log.out(data=G[[i]], model=model,  select.m=select.m, step=step, scale=scale,proba=proba), silent=T)
          if(class(resg)=="try-error")   R1[[length(R1)+1]]<-"The number of observations is insufficient to carry out the analyzes for this group" else R1[[length(R1)+1]]<-resg
          names(R1)[length(R1)]<-names(G)[i]
        }
        Results$"Complete data"$"Group analysis"<-R1
      } 
      
    } 
    if(any(outlier=="Identification of influential values")|any(outlier=="Data without influencing value")|inf==T){
      
      lm.r1<-glm(model, data, na.action=na.exclude ,family="binomial")
      as.character(attributes(terms(model))$variables)->variables
      variables[2:length(variables)]->variables
      plot(lm.r1, which = 5)
      if(inf) {
        influence.measures(lm.r1)->mesure_influence
        data<-data.frame(data, round(mesure_influence$infmat,3))
        rstandard(lm.r1)->data$res.stand
        rstudent(lm.r1)->data$res.student # idem avec le residue studentise
        data$res.student.p<-2*pt(abs(data$res.student), df=lm.r1$df.residueal, lower.tail=F)
        data$res.student.p.Bonf<-p.adjust(data$res.student.p,"bonferroni")
        data$est.inf<-" "
        data[which(apply(mesure_influence$is.inf, 1, any)),"est.inf"]<-"*"
        
        data[order(data$res.student.p.Bonf), ]->data
        writeLines("Observations marked with an asterisk are considered influential at least on one criterion.")
        View(data)
        suppression<-"yes"
        outliers<-data.frame()
        cleaned<-data
        while(suppression=="yes"){
          
          cat ("Appuyez [Betweene] pour continuer")
          line <- readline()
          sup<-NA
          while(is.na(sup)){
            sup <- dlgInput("What observation do you want to get from the analyzes? 0 = none", 0)$res
            if(length(sup)==0) return(regressions())
            strsplit(sup, ":")->sup
            tail(sup[[1]],n=1)->sup
            as.numeric(sup)->sup
            if(is.na(sup)) msgBox("You must enter the observation number")  
          }
          if(sup==0) suppression<-"no" else {
            rbind(outliers, cleaned[sup,])->outliers
            cleaned[-sup,]->cleaned
          }
          
        }
        if(length(outliers)!=0) outliers<-outliers[,variables]
        assign(nom, data, envir=.GlobalEnv)
      } else {
        4/length(data[,1])->seuil_cook # fixe le seuil pour les values aberrantes 
        cooks.distance(lm.r1)->data$cook.d  
        data[which(data$cook.d<= seuil_cook), ]->cleaned 
        data[which(data$cook.d>= seuil_cook), ]->outliers
        cbind(outliers[,variables],outliers$cook.d)->outliers
        Results$"information"$"les values influentes sont identifiees sur la base de 4/n"
      }
      cleaned->>cleaned   
      
      if(any(outlier== "Identification of influential values")){
        length(data[,1])-length(cleaned[,1])->N_retire # identifier le nombre d observations retirees sur la base de la distance de cook
        paste(N_retire/length(data[,1])*100,"%")->Pourcentage_retire # fournit le pourcentage retire
        data.frame("N.retirees"=N_retire, "Percentage.obs.removed"=Pourcentage_retire)->Results$"Summary of the number of observations considered to be influential"
        if(length(outliers)!=0) Results$"Identification of influential values"$"Observations considered influential"<-outliers
        
      }
      if(any(outlier== "Data without influencing value")) {
        if(N_retire!=0 | all(outlier!="Complete data")){
          so<- try(reg.log.out(data=cleaned,model=model,  select.m=select.m, step=step, scale=scale,proba=proba, nom=paste0(nom,".cleaned")),silent=T)
          if(class(so)=="try-error") Results$"Data without influencing value"<-"The removal of influential values leads to too few numbers in certain modalities to carry out the analysis." else{
            Results$"Data without influencing value"<-so 
            
            if(!is.null(group))   {  
              R1<-list()
              G<-cleaned[,group]
              if(length(group)>1) G<-as.list(G)
              G<-split(cleaned, G)
              for(i in 1:length(G)){
                resg<- try( reg.log.out(data=G[[i]], model=model,  VC=VC, select.m=select.m, method=method, step=step, group=group,  scale=scale,proba=proba), silent=T)
                
                if(class(resg)=="try-error")   R1[[length(R1)+1]]<-"The number of observations is insufficient to carry out the analyzes for this group" else R1[[length(R1)+1]]<-resg
                names(R1)[length(R1)]<-names(G)[i]
              }
              Results$"Data without influencing value"$"Group analysis"<-R1
            } 
          } 
          
        }
      }
    }
    
    
    paste(outlier, collapse="','", sep="")->outlier
    as.character(model)->m1
    model<-paste0(m1[2],"~", m1[3])
    if(!is.null(group)) paste(group, collapse="','", sep="")->group
    if(!is.null(step)) {
      paste0("list(")->step.call
      for(i in 1:length(step)){
        if(i>1) n.step<-paste0(", step",i) else n.step<-paste0("step",i)
        paste(step[[i]], collapse="','", sep="")->var.step
        step.call<-paste0(step.call,n.step,"=c('", var.step, "')")
      }
      step.call<-paste0(step.call, ")")
    }
    Results$Call<-paste0("regressions.log(data=", nom, ",model=",  model, ",outlier=c('", outlier, "'),inf=", inf, ",select.m='", select.m,"',step=", ifelse(!is.null(step), step.call,"NULL"),
                           ",group=", ifelse(is.null(group), "NULL", paste0("c('",group,"')")),", dial = T, info = T, save =", sauvegarde,",proba=",proba ,")")
    
    
    .add.history(data=data, command=Results$Call, nom=nom)
    .add.result(Results=Results, name =paste("Regressions.logistics", Sys.time() ))  
    if(sauvegarde)   if(sauvegarde) save(Results=Results, choice="Regressions.logistics", env=.e)
    Results$"References"<-ref1(packages)
    if(html) try(ez.html(Results), silent=T)
    return(Results)
    
  }
