regressions <-
  function(data=NULL, model=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=T, CV=F, select.m="none", method="p", step=NULL, group=NULL, criteria=0.15 , scale=T, dial=T, info=T,
           sauvegarde=F, n.boot=NULL, param=NULL, rscale=0.353, html=TRUE){
    
    
    
    regressions.in<-function(data=NULL, model=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=F, CV=F, select.m="none", method="p", step=NULL, group=NULL, criteria=NULL , scale=T, dial=T, info=T,
                             sauvegarde=F, n.boot=NULL, param=NULL, rscale=0.353){
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
        if(length(link)==0) return(NULL) } else link<-"none"
      
      if(length(Y)>1){
        msgBox("There can only be one dependent variable.")
        Y<-NULL }
      if(any(link %in% c("Additive effects", "Interaction effects"))){
        msg3<-"Please choose the dependent variable."
        Y<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=FALSE, title="Dependent variable", out=NULL)
        if(is.null(Y)) {
          regressions.in()->Results
          return(Results)}
        data<-Y$data
        Y<-Y$X
        
        if(any(link=="Additive effects") || !is.null(X_a)| any(X_a %in% names(data)==F)) {
          msg3<-"Please choose the dependent variable."
          X_a<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title="Additive model variables", out=Y)
          if(is.null(X_a)) {
            regressions.in()->Results
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
              regressions.in()->Results
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
      
      if(any(link=="Specify the model")) {
        if(is.null(model)) model<-" "
        model<-fix(model)}
      model<-as.formula(model)
      variables<-terms(model)
      variables<-as.character( attributes(variables)$variables)[-1]
      
      
      model.test<-try(model.matrix(model, data), silent=T)
      if(class(model.test)=="try-error") {
        msgBox("The model specified is incorrect. Check your variables and your model")
        return(regressions.in())
      }
      
      
      data[complete.cases(data[,variables]),]->data
      msg.options1<-"The parametric test is classical regression and the robust tests are an estimate on an M estimator as well as a bootstrap."
      
      options<-.ez.options(options=c("choice","outlier"), n.boot=n.boot,param=T, non.param=F, robust=T, Bayes=T, msg.options1=msg.options1, msg.options2=msg.options2, info=info, dial=dial, 
                           choice=param,sauvegarde=sauvegarde, outlier=outlier, rscale=rscale)
      if(is.null(options)) return(regressions.in())
      
      reg.options<- .regressions.options(data=data, model=model, CV=CV, inf=inf, select.m=select.m, method=method, criteria=criteria, step=step, group=group, scale=scale, dial=dial,info=info)
      if(is.null(reg.options)) return(regressions.in())
      
      
      Results$data<-data
      Results$nom<-nom
      Results$model<-model
      Results$options<-options
      Results$reg.options<-reg.options
      return(Results)   
      
    }
    
    regressions.out<-function(dtrgeasieR=NULL, model=NULL,  VC=F, select.m="none", method=NULL, step=NULL, group=NULL, criteria=NULL , scale=T,
                              sauvegarde=F, n.boot=NULL, param=NULL, rscale=0.353){
      
      Results<-list()
      variables<-terms(as.formula(model))
      variables<-as.character( attributes(variables)$variables)[-1]
      pred<-attributes(terms(as.formula(model)))$term.labels
      Results$"Descriptive statistics"<-.stat.desc.out(X=variables, groupes=NULL, data=dtrgeasieR, tr=.1, type=3, plot=T)
      
       if(scale==T || scale=="Center") {
         Results$info<-"In accordance with the recommendations of Schielzeth 2010, the data were previously centered"
        which(!sapply(dtrgeasieR[,pred[which(pred %in% variables)]],class)%in%c("factor", "character"))->cBetween
        cBetween<-pred[cBetween]
      if(length(cBetween)==1) dtrgeasieR[,cBetween]-mean(dtrgeasieR[,cBetween],na.rm=T)->dtrgeasieR[,cBetween] else{
        sapply(X=dtrgeasieR[,cBetween], fun<-function(X){X-mean(X, na.rm=T)})->dtrgeasieR[,cBetween]
      }
      }
      
      
      
      mod<-list()
      model1<-as.formula(paste0(variables[1], "~", pred[1]))
      lm( model1,na.action=na.exclude, data=dtrgeasieR)->lm.r1
      lm.r1->mod[[1]]
      if(length(pred)>1) {
        for(i in 2:length(pred)){update(lm.r1, as.formula(paste0(".~.+",pred[i])))->lm.r1
          lm.r1->mod[[i]]}
      }
      assign("lm.r1",lm.r1, env= .GlobalEnv)
      resid(lm.r1)->dtrgeasieR$residue
      Results$"Normality tests"<-.normalite(data=dtrgeasieR, X="residuee", Y=NULL)
      if(length(variables)>1)  {
        cont<-variables[which(!sapply(dtrgeasieR[,variables],class)%in%c("factor","character"))]
        Results$"Multivariate normalcy"<-.normalite(data=dtrgeasieR, X=cont, Y=NULL)
        ols_plot_resid_fit(lm.r1)
        IVF<-ols_coll_diag(lm.r1) # calcul du factor.d inflation de la variance 
        IVF[[1]]<-data.frame(IVF[[1]])
        names(IVF)<-c("Multicolinearity test", "Index of eigenvalues")
        names(IVF$`Multicolinearity test`)<-c("variables", "Tolerance", "IVF")
        Results$"Multicolinearitis tests"<-IVF$`Multicolinearity test`
        if(IVF$`Multicolinearity test`$Tolerance==0) {
          msgBox("Multicolinearity is too important. The model is unstable")
          return(Results)
        }
        
        Results$"Graph testing the linearity between the predictors and the dependent variable"<-ols_plot_comp_plus_resid(lm.r1)
        Results$"Index of eigenvalues"<-IVF$`Index of eigenvalues`
        dwt(lm.r1, simulate=TRUE, method= "normal", reps=500)->DWT.results
        Results$"Durbin-Watson test - autocorrelations"<-round(data.frame("Autocorrelation"=DWT.results[[1]],
                                                                               "D-W statistic"=DWT.results[[2]],"p-value"=DWT.results[[3]]),4)
        
        var.err<-ols_test_breusch_pagan(lm.r1, rhs=T)
        
        Results$"Verification of the non-constancy of the error variance (Breusch-Pagan test)"<-data.frame(chi=var.err$bp,
                                                                                                                 dof=length(var.err$preds), p-value=var.err$p) 
        
        try(ceresPlots(lm.r1, main="Ceres graph testing linearity"), silent=T)
      }
      if(select.m!="none"){
        dtrgeasieR<<-dtrgeasieR
        if(method %in% c("F", "F value", "p", "probability value")){
          select.m<-switch(select.m,"Forward - step by step ascending"="Forward", "Backward - step by step descending"="Backward", "Bidirectional"="Both",
                           "forward"="Forward", "bidirectional"="Stepwise","backward"="Both" )
          
          if(select.m=="Forward") t<-capture.output({  ols.out <- ols_step_forward_p(lm.r1,penter = criteria, details=F)})
          if(select.m=="Backward") t<-capture.output({  ols.out <- ols_step_backward_p(lm.r1, prem=criteria, details=F)})
          if(select.m=="Both") t<-capture.output({  ols.out <- ols_step_both_p(lm.r1,pent=criteria, details=F)})
          predname<-if(!is.null(ols.out$predictors)) rep(TRUE, length(ols.out$predictors)) else rep(FALSE,length(ols.out[[1]]) )
          methodname<-if(!is.null(ols.out$method)) rep(TRUE, length(ols.out$method)) else rep(select.m,length(ols.out[[1]]) )
          ols.frame<-data.frame(step=1:ols.out$steps,
                                predicteurs=ifelse(predname,ols.out$predictors,ols.out$removed) ,
                                mallows_cp=ols.out$ mallows_cp,
                                AIC=ols.out$aic,
                                BIC=ols.out$sbc,
                                RMSE=ols.out$rmse,
                                r.squared=ols.out$rsquare,
                                r.squared.adj=ols.out$adjr,
                                Method=ifelse(methodname==T, ols.out$method, ifelse(methodname=="Forward" , "Variable added", "variable deleted"))
          )
          Results$"Selection method"<-ols.frame 
        }
        
        if(method %in% c("AIC - Akaike Information criterion","AIC")){ 
          select.m<-switch(select.m,"Forward - step by step ascending"="Forward", "Backward - step by step descending"="Backward", "Bidirectional"="Both",
                           "forward"="Forward", "bidirectional"="Both","backward"="Backward" )
          lm.r1<-lm(model, data=dtrgeasieR)
          if(select.m=="Forward") t0<-capture.output({  ols.out <- ols_step_forward_aic(lm.r1, details=T)}) 
          if(select.m=="Backward") t0<-capture.output({  ols.out <- ols_step_backward_aic(lm.r1, details=T)})
          if(select.m=="Both")     t0<-capture.output({  ols.out <- ols_step_both_aic(lm.r1, details=T)})
          
          predname<-if(select.m!="Backward") rep(TRUE, length(ols.out$predictors)) else rep(FALSE,length(ols.out[[1]])+1 )
          methodname<-if(!is.null(ols.out$method)) rep(TRUE, length(ols.out$method)) else rep(select.m,length(ols.out[[4]]) )
          ols.frame<-data.frame(step=1:ols.out$steps,
                                predicteurs=ifelse(predname,ols.out$predictors, c("Complete model", ols.out$predictor)) ,
                                Somme.Carre=ols.out$rss,
                                AIC=ols.out$aic,
                                SC.res=ols.out$ess,
                                r.squared=ols.out$rsq,
                                r.squared.adj=ols.out$arsq,
                                Method=ifelse(methodname==T, ols.out$method, ifelse(methodname=="Forward" , "Variable added", c(" ","variable deleted")))
          )
          
          Results$"Selection method - Akaike information criteria"<-ols.frame
          
        }
        
        if(any(param=="Bayes")|any(param=="Bayesian factors")){
          BF.out<-try(regressionBF(model, data=dtrgeasieR,progress=F, rscaleCont=rscale), silent=T)
          if(class(BF.out)!="try-error") {
            try(plot(BF.out) , silent=T)
            BF.out<-extractBF(BF.out)
            BF.out<-head(BF.out[order(BF.out[,1], decreasing=T), ])
            BF.out<-BF.out[,1:2]
            Results$"Selection methods: Bayesian factors"<-BF.out
          } else Results$"Selection methods: Bayesian factors"<-"The selection methods for Bayesian factors do not apply for complex models."
        }
        rm( "dtrgeasieR", envir = .GlobalEnv)
      }
      
      if(!is.null(step)){
        
        as.formula(paste0(variables[1]," ~ ",step[[1]][1]))->model.H
        list()->model.H1
        list()->formule.H1
        for(i in 1:length(step)){
          
          for(j in 1:length(step[[i]])){update(model.H, as.formula(paste0(".~. + ",step[[i]][j])))->model.H}
          formule.H1[[i]]<-model.H
          lm(model.H, data=dtrgeasieR, na.action=na.exclude )->lm.H
          lm.H->model.H1[[i]]}
        
        if(any(param=="param")|any(param=="Parametric test")) {
          hier<-paste0("anova(model.H1[[1]],model.H1[[2]]")
          if(length(model.H1)>2){
            for(i in 3: length(model.H1)){
              hier<-paste0(hier, ",model.H1[[", i, "]]")
            }
          }
          hier<-paste0(hier,")")
          hier<-eval(parse(text=hier))
          attributes(hier)$heading[1]<-"Analysis of variance table for hierarchical models"
          names(hier)<-c("dof.resid", "SC.resid","dof.effect", "SC", "F", "p-value")
          Results$"Hierarchical analysis of models "<-hier
          
          
          
          c(summary(model.H1[[1]])$sigma, summary(model.H1[[1]])$r.squared, summary(model.H1[[1]])$fstatistic)->significativite_model # fournit les residuees, le R.two et le F
          pf(summary(model.H1[[1]])$fstatistic[1], summary(model.H1[[1]])$fstatistic[2],summary(model.H1[[1]])$fstatistic[3], lower.tail=F)->p.value #permet de savoir si le F est significatif
          c(significativite_model , p.value)->model_avec_outliers 
          
          for(i in 2:(length(model.H1))){
            c(summary(model.H1[[i]])$sigma, summary(model.H1[[i]])$r.squared, summary(model.H1[[i]])$fstatistic)->significativite_model # fournit les residuees, le R.two et le F
            pf(summary(model.H1[[i]])$fstatistic[1], summary(model.H1[[i]])$fstatistic[2],summary(model.H1[[i]])$fstatistic[3], lower.tail=F)->p-value #permet de savoir si le F est significatif
            rbind(model_avec_outliers, c(significativite_model ,p-value))->model_avec_outliers  
          }
          round(model_avec_outliers,3)->model_avec_outliers 
          c("Residual error", "R.two", "F", "DOF (1)", "DOF (2)","p-value")->dimnames(model_avec_outliers)[[2]]
          paste("step", 1:length(model_avec_outliers[,1]))->dimnames(model_avec_outliers)[[1]]
          Results$"Hierarchical models - significance of the complete model at each stage"<-model_avec_outliers
          
        }
        
        if(any(param=="Bayes")|any(param=="Bayesian factors")) {
          BF<-lmBF(formula= as.formula(formule.H1[[1]]), data=dtrgeasieR, rscaleFixed=rscale)
          BF.model<-extractBF(BF, onlybf=T)
          BF.hier<-c(NA)
          for(i in 2:length(formule.H1)){
            numBF<-lmBF(formula= as.formula(formule.H1[[i]]), data=dtrgeasieR, rscaleFixed=rscale)
            BF.model<-c(BF.model, extractBF(numBF, onlybf=T))
            denomBF<-lmBF(formula= as.formula(formule.H1[[i-1]]), data=dtrgeasieR, rscaleFixed=rscale)
            OddBF<-numBF/denomBF
            BF.hier<-c(BF.hier, extractBF(OddBF, onlybf=T))}
          
          BF.hier<-data.frame("Ratio of FBs between models"=BF.hier, "Model FB"= BF.model)
          dimnames(BF.hier)[[1]]<- unlist(as.character(formule.H1))
          Results$"Bayesian approach to hierarchical models"<-BF.hier
        }
        
      }
      # "parametric test", "non-parametric test","Robust testing - involving bootstraps", "Bayesian factors"   
      if(any(param=="param")|any(param=="Parametric test")) {
        c(summary(lm.r1)$sigma, summary(lm.r1)$r.squared, summary(lm.r1)$fstatistic)->significativite_model # fournit les residuees, le R.two et le F
        pf(summary(lm.r1)$fstatistic[1], summary(lm.r1)$fstatistic[2],summary(lm.r1)$fstatistic[3], lower.tail=F)->p.value #permet de savoir si le F est significatif
        c(significativite_model , p.value)->model.F # on combine les precedents 
        round(model.F,3)->model.F # on arrondit les nombres a la 3e decimale
        c("Residual error", "R.two", "F", "Dof (num)", "Dof (dname)","p-value")->names(model.F)# attribue le nom aux colonnes
        model.F->Results$"Estimation  du model global"
        
        
        data.frame(summary(lm.r1)$coefficients)->table # fournit le b, le t et la probability value. On le stocke dans table
        round(table[,1:4],3)->table # on arrondit les values a 3 decimales 
        
        beta<-coef(lm.r1)*sapply(data.frame(model.matrix(lm.r1)),sd) /sd(dtrgeasieR[,variables[1]])
        c("",round(beta[-1],5))->table$beta # fournit les betas qu on inclut a la table 
        names(table)<-c("b","standard.error","t","p-value","beta")
        
        r.squared<- matrix(c(0,0,0),1)
        for(i in 1:length(mod)){
          rep(summary(mod[[i]])$r.squared, (length(coef(mod[[i]]))-length(r.squared[,1])))->r.squared2
          summary(mod[[i]])$r.squared-r.squared[length(r.squared[,2]),1]->diff
          rep(diff, (length(coef(mod[[i]]))-length(r.squared[,1])))->diff
          rep(summary(mod[[i]])$adj.r.squared, (length(coef(mod[[i]]))-length(r.squared[,1])))->r.squared_adj
          
          round(cbind(r.squared2, diff, r.squared_adj), 4)->r.squared2
          rbind(r.squared,r.squared2 )->r.squared
          
        }
        
        dimnames(r.squared)<-list(ligne=NULL, c("R.two", "Delta R.two", "R.two.aj"))
        data.frame(table,r.squared)->table
        table[is.na(table)]<-""
        table->Results$"betas table"
        if(length(pred)>1){
          ols.corr<-try(ols_correlations(lm.r1), silent=T)
          if(class(ols.corr)!="try-error"){
          Results$"Contribution of variables to the model"<-ols.corr
          Results$"Graph of added variables" <-ols_plot_added_variable(lm.r1)}
        }
      }
      
      if(any(param=="Bayes")|any(param=="Bayesian factors")){
        
        lmBF(model1, data=dtrgeasieR)->BF.out
        BF.table<-extractBF(BF.out)[1:2]
        if(length(pred)>1) { for(i in 2:length(pred)){
          model1<-update(model1, as.formula(paste0(".~.+",pred[i])))
          lmBF(model1, data=dtrgeasieR)->BF.out
          BF.table<-rbind(BF.table, extractBF(BF.out)[1:2])
        }
        } 
        Results$"Bayesian factors"<-BF.table
        
      }
      
      if(any(param=="robusts"| any(param=="Robust testing - involving bootstraps"))){
        
        rlm(formula=model, data=dtrgeasieR)->model_robuste
        summary(model_robuste)->res_model_robuste
        (1-pt(abs(res_model_robuste$coefficients[,3]), (length(dtrgeasieR[,1])-1-length(pred)), lower.tail=TRUE))*2->proba
        round(cbind(res_model_robuste$coefficients, proba),3)->M_estimator
        data.frame(M_estimator)->M_estimator
        noms<-c("b (M estimator)", "SE", "t", "p-value")
        
        
        if(n.boot>100){ 
          bootReg<-function(formula, dtrgeasieR, i)
          {  d <- dtrgeasieR[i,]
          fit <- lm(formula, data = d)
          return(coef(fit))}
          bootResults<-boot(statistic=bootReg, formula= model , data=dtrgeasieR, R=n.boot) # cree le bootstrap
          intervalle<-c()
          try(for(i in 1: length(lm.r1$coefficients)){boot.ci(bootResults, type = "bca", index = i)$bca[,4:5]->IC1
            rbind(intervalle, IC1)->intervalle}, silent=T)
          if(is.null(intervalle)){
            for(i in 1: length(lm.r1$coefficients)){boot.ci(bootResults, type = "perc", index = i)$percent[,4:5]->resultats
              rbind(intervalle, resultats)->intervalle}
            noms<-c(noms, "Percentile.inf.lim", "Percentile.sup.lim")
          } else{
            noms<-c(noms, "Bca.inf.lim", "Bca.sup.lim")
          }
          data.frame(M_estimator, round(intervalle,4))->M_estimator
        }
        names(M_estimator)<-noms
        Results$"Robust statistics"<-M_estimator
      }  
      
      
      if(CV) "cross validation is encountering some issues" 
      
      return(Results) 
      
    }
    options (warn=-1) 
    .e <- environment()
    c("BayesFactor","boot","car","ggplot2","gsl", "MBESS","olsrr","nortest","psych","QuantPsyc","svDialogs")->packages
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    Results<-list() 
    try( windows(record=T), silent=T)->win
    if(class(win)=="try-error") quartz()
    if(class(data)=="data.frame") deparse(substitute(data))->data 
    reg.in.output<-regressions.in(data=data, model=model, Y=Y, X_a=X_a, X_i=X_i, outlier=outlier, inf=inf, 
                                  CV=CV, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale, info=info,
                                  sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
    if(is.null(reg.in.output)) return(choice.reg()) 
    data<-reg.in.output$data
    nom<-reg.in.output$nom
    model<-reg.in.output$model
    param<-reg.in.output$options$choice
    n.boot<-reg.in.output$options$n.boot
    if(reg.in.output$options$rscalei) rscale<-reg.in.output$options$rscale/2 else rscale<-reg.in.output$options$rscale
    outlier<-reg.in.output$options$desires
    sauvegarde<-reg.in.output$options$sauvegarde
    scale<-reg.in.output$reg.options$scale
    inf<-reg.in.output$reg.options$inf
    CV<-reg.in.output$reg.options$CV
    step<-reg.in.output$reg.options$step
    select.m<-reg.in.output$reg.options$select.m
    method<-reg.in.output$reg.options$method
    criteria<-reg.in.output$reg.options$criteria
    group<-reg.in.output$reg.options$group
    
    
    
    
    
    
    
    
    if(any(outlier==  "Complete data")){
      Results$"Complete data"<-regressions.out(dtrgeasieR=data, model=model,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                                     sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
      if(!is.null(group))   {  
        R1<-list()
        G<-data[,group]
        if(length(group)>1) G<-as.list(G)
        G<-split(data, G)
        for(i in 1:length(G)){
          resg<-regressions.out(data=G[[i]], model=model,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
          
          R1[[length(R1)+1]]<-resg
          names(R1)[length(R1)]<-names(G)[i]
        }
        Results$"Complete data"$"Group analysis"<-R1
      } 
      
    } 
    if(any(outlier=="Identification of influential values")|any(outlier=="Data without influencing value")|inf==T){
      lm.r1<-lm(model, data)
      as.character(attributes(terms(model))$variables)->variables
      variables[2:length(variables)]->variables
      plot(lm.r1, which = 5)
      if(inf) {
        influence.measures(lm.r1)->mesure_influence
        data<-data.frame(data, round(mesure_influence$infmat,3))
        data$leverage<-ols_leverage(lm.r1)
        rstandard(lm.r1)->data$res.stand
        rstudent(lm.r1)->data$res.student # idem avec le residue studentise
        data$res.student.p<-2*pt(abs(data$res.student), df=lm.r1$df.residueal, lower.tail=F)
        data$res.student.p.Bonf<-p.adjust(data$res.student.p,"bonferroni")
        data$est.inf<-" "
        data[which(apply(mesure_influence$is.inf, 1, any)),"est.inf"]<-"*"
        ols_plot_dfbetas(lm.r1)
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
            if(is.na(sup)) msgBox("You must enter the number allowing you to know which observation should be deleted.")  
          }
          if(sup==0) suppression<-"no" else {
            rbind(outliers, cleaned[which(dimnames(cleaned)[[1]]==sup),])->outliers
            cleaned[-which(dimnames(cleaned)[[1]]==sup),]->cleaned
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
      length(data[,1])-length(cleaned[,1])->N_retire # identifier le nombre d observations retirees sur la base de la distance de cook
      if(any(outlier== "Identification of influential values")){
        paste(N_retire/length(data[,1])*100,"%")->Pourcentage_retire # fournit le pourcentage retire
        data.frame("N.retire"=N_retire, "Percent.obs. withdrawn"=Pourcentage_retire)->Results$"Summary of the number of observations considered to be influential"
        if(length(outliers)!=0) Results$"Identification of influential values"$"Observations considered influential"<-outliers
        
      }
      if(any(outlier== "Data without influencing value")) {
        if(N_retire!=0 | all(outlier!="Complete data")){
          Results$"Data without influencing value"<-regressions.out(dtrgeasieR=cleaned, model=model,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                                                     sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
          
          if(!is.null(group))   {  
            R1<-list()
            G<-cleaned[,group]
            if(length(group)>1) G<-as.list(G)
            G<-split(cleaned, G)
            for(i in 1:length(G)){
              resg<-regressions.out(data=G[[i]], model=model,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                    sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
              
              R1[[length(R1)+1]]<-resg
              names(R1)[length(R1)]<-names(G)[i]
            }
            Results$"Data without influencing value"$"Group analysis"<-R1
          } 
          
          
        }
      }
    }
    
    
    paste(outlier, collapse="','", sep="")->outlier
    paste(param, collapse="','", sep="")->param
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
    Results$Call<-paste0("regressions(data=", nom, ",model=",  model, ",outlier=c('", outlier, "'),inf=", inf, ",CV=", CV,",select.m='", select.m,"',step=", ifelse(!is.null(step), step.call,"NULL"),
                           ",group=", ifelse(is.null(group), "NULL", paste0("c('",group,"')")),
                           ",criteria=", criteria, ",scale=", scale, ", dial = T, info = T, save =", sauvegarde, ",n.boot=", n.boot, ",param=c('", param, "'),rscale=", round(rscale,3), ")")
    
    
    .add.history(data=data, command=Results$Call, nom=nom)
    .add.result(Results=Results, name =paste("multiple regressions", Sys.time() ))  
    if(sauvegarde)   if(sauvegarde) save(Results=Results, choice="Multiple regressions", env=.e)
    Results$"References"<-ref1(packages)
    if(html) ez.html(Results)
    return(Results)
  }





.regressions.options<-function(data=NULL, model=NULL, CV=F, inf=F, select.m="none", method="p", criteria=NULL, step=NULL, group=NULL, scale=T, dial=T,info=T){
  # data : dataframe 
  # model : formula as it is used in lm
  # CV : logical. Should a cross validation to be performed ? 
  # inf : Logical. Should influential observations be checked ? 
  # select.m : character specifying method of selection. One among "none", "forward", "backward" and "bidirectional"
  # method : if select is different of "none", one among "AIC", "F", or "p"
  # criteria : if method is "F", specify F value to use. If method is "p", specify p value to use as cutoff criteria. 
  # step : list. Each element of the list is a vector with the effect to test at the specific step (see details)
  # group : character. Name of the factor variable definying the groups
  # scale : Logical. Should the predictor be scaled before the analysis (recommended) ? 
  
  Results<-list()
  step1<-terms(as.formula(model))
  
  step2<-as.character( attributes(step1)$variables)[-1]
  step1<-attributes(step1)$term.labels
  if(dial || !is.logical(scale)){
    if(info)   writeLines("Do you want to center the numeric variables? Center is generally advised (e.g., Schielzeth, 2010).")
    scale<-dlgList(c("Center", "No center"), multiple = FALSE, title="Center?")$res
    if(length(scale)==0) return(NULL)
    scale<-ifelse(scale=="Center",T,F) 
  }
  Results$scale<-scale
  if(dial || !is.logical(inf) || !is.logical(CV)) {
    writeLines("Voulez-vous preciser d'autres options ? Vous pouvez en selectionner plusieurs.
               Les methodes de selection permettent de selectionner le meilleur model sur la base de criteres statisticals.
               Les models hierarchiques permettent de comparer plusieurs models. 
               Les validations croisees permettent de verifier si un model n'est pas dependant des donnees. Cette option est a utiliser notamment 
               avec les methodes de selection. L'analyse par groupe permet de realiser la meme regression pour des sous-groupes.
               Les mesures d'influences sont les autres mesures habituellement utilisees pour identifier les values influentes.")
    autres.options<-c("Cross validation","Influence measurement",  "any")
    if(dim(model.matrix(model, data))[2]>2) autres.options<-c("Selection methods", "Hierarchical models", autres.options)
    if(length(step2)<length(data))  autres.options<-c("group analysis",autres.options)
    
    autres.options<- dlgList( autres.options, preselect=c("any"), multiple = TRUE, title="Other options?")$res 
    if(length(autres.options)==0) return(.regressions.options(data=data, model=model))
    # if(any(autres.options=="any")) return(Results)   
    if(any(autres.options=="Influence measurement") ) Results$inf<-T else  Results$inf<-F
    if(any(autres.options=="Cross validation") ) Results$CV<-T else Results$CV<-F
  }else{Results$inf<-inf
  Results$CV<-CV 
  autres.options<-"any"
  }
  
  
  if(any(autres.options=="group analysis") || !is.null(group)) {
    
    msg5<-"Please choose the categorical ranking factor."
    group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=T, message=msg5,  multiple=FALSE, title="Variable-s groups", out=step2)
    if(length(group)==0) { return(.regressions.options(data=data, model=model))}
    data<-group$data
    group<-group$X 
    ftable(data[,group])->groupe.check
    if(any(is.na(groupe.check)) || min(groupe.check)<(length(dimnames(model.matrix(as.formula(model), data))[[2]])+10)) {
      msgBox("At least 10 observations plus the number of variables are needed to perform the analysis. Check your data.")
      return(groupe.check)
    }
  }
  
  if(any(autres.options=="Selection methods") || select.m!="none" & length(select.m)!=1 | !select.m%in%c("none","forward", "backward", "bidirectional","Forward - step by step ascending",
                                                                                                             "Backward - step by step descending", "Bidirectional")){
    if(info) writeLines("Please choose the selection method you wish to use")
    select.m<- dlgList(c("Forward - step by step ascending","Backward - step by step descending", "Bidirectional"), 
                       preselect=NULL, multiple = FALSE, title="Choice of method")$res
    if(length(select.m)==0) return(.regressions.options(data=data, model=model))
  } 
  if(!is.null(method)){
    if(any(autres.options=="Selection methods")   || (select.m!="none" && !method%in%c("AIC", "p", "F", "F value","probability value", "AIC - Akaike Information criterion")) ){
      if(info) writeLines("What method should be applied for the selection method?")
      method<- dlgList(c("F value","probability value", "AIC - Akaike Information criterion"), 
                       preselect=c("F value"), multiple = FALSE, title="Choice of method")$res
      if(length(method)==0) return(.regressions.options(data=data, model=model)) 
    }
    
    if(select.m!="none" & (method=="F value" | method=="F")){
      if(!is.null(criteria) && (!is.numeric(criteria) || criteria<1)) {msgBox("You must specify the value of F. This value must be greater than 1")
        criteria<-NULL}
      
      if(is.null(criteria)) {
        while(is.null(criteria)){
          criteria <- dlgInput("What value of F do you want to use?", 4)$res
          if(length(criteria)==0) return(.regressions.options(data=data, model=model))
          strsplit(criteria, ":")->criteria
          tail(criteria[[1]],n=1)->criteria
          as.numeric(criteria)->criteria
          if(is.na(criteria) || criteria<1) {criteria<-NULL
          msgBox("You must specify the value of F. This value must be greater than 1")
          }
          criteria<-df(criteria, df1=1, df2=(length(data[,1])-1-length(step1)), log = FALSE)
        }
      }
    }
    
    if(select.m!="none" & (method=="probability value" | method=="p")){
      if(dial | !is.null(criteria) && (!is.numeric(criteria) || criteria<0 || criteria>1)) {msgBox("You must specify the value of the probability. This value must be between 0 and 1")
        criteria<-NULL}
      if(is.null(criteria)) {
        while(is.null(criteria)){
          criteria <- dlgInput("What value of the probability do you want to use?", 0.15)$res
          if(length(criteria)==0) return(.regressions.options(data=data, model=model))
          strsplit(criteria, ":")->criteria
          tail(criteria[[1]],n=1)->criteria
          as.numeric(criteria)->criteria
          if(is.na(criteria) || criteria>1 || criteria<0 ) {criteria<-NULL
          msgBox("You must specify the value of the probability. This value must be between 0 and 1")}
        }
      }
      
    }
  }
  if(any(autres.options=="Hierarchical models")| !is.null(step)) {
    
    if(!is.null(step) ){
      st1<-unlist(step)
      if(any(table(st1>1))) st1<-"error"
      if(any(!st1%in%step1 ))st1<-"error"
      if(st1=="error"){
        msgBox("A problem has been identified in the stages of your hierarchical regression")
        step<-NULL
      }
    }         
    if(is.null(step)){
      if(info) writeLines("Please choose the variables to use for each step")      
      step<-list()
      step[[1]]<- dlgList(step1, preselect=NULL, multiple = TRUE, title="Variable (s) of this step")$res
      if(length(step[[1]])==0) return(.regressions.options(data=data, model=model))
      setdiff(step1,step[[1]])->step1
      
      while(length(step1!=0)){
        step[[length(step)+1]]<-dlgList(step1, multiple = TRUE,title="Variable (s) of this step")$res
        if(length(step[[length(step)]])==0) return(.regressions.options(data=data, model=model))
        setdiff(step1,step[[length(step) ]])->step1
      } 
    }
  } 
  
  Results$step<-step
  Results$select.m<-select.m
  Results$method<-method
  Results$criteria<-criteria
  Results$group<-group 
  return(Results) 
}
