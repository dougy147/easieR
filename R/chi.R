chi <-
  function(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=TRUE, n.boot=NULL, priorConcentration =1,  
           SampleType=NULL,fixedMargin=NULL, choix2=c("non-parametric test","Robust testing - involving bootstraps", "Bayesian factors") ,rscale=2^0.5/2, html=T){
    # X = character or vector.  First set of variables
    # Y = character or vector. Second set of variables
    # Effectifs = character. Name of weighting variable. Must be positive integer
    # p = vector of probabilities. Must be equal to 1. The lenght must be equel to number of levels of X
    # choix = character. One among "Adjustment", "Independence", or "McNemar test"
    # data = name of the dataframe 
    # B = number of bootstrap fro computing p.values by Monte-Carlo simulation
    # priorConcentration : prior concentration paramter, set to 1 by default (see ?contingencyTableBF)
    # SampleType : the sampling plan (see details)
    # fixedMargin : for the independent multinomial sampling plan, which margin is fixed ("rows" or "cols")
    # rscale : prior scale. A number of preset values can be given as strings
    chi.in<-function(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL){
      if(!is.null(choix)) dial<-F else dial<-T
      if(is.null(choix) || (choix %in%c("Adjustment", "Independence", "McNemar test")==FALSE)){
        if(info) writeLines("Please specify the type of square footage you wish to achieve.")
        choix<- dlgList(c("Adjustment", "Independence", "McNemar test"), preselect="Independence", multiple = FALSE, title="Chi-square type")$res
        if(length(choix)==0) return(NULL)
      }
      
      choix.data(data=data, info=info, nom=T)->data
      if(length(data)==0) return(NULL)
      data[[1]]->nom
      data[[2]]->data
      msg3<-"Please choose the first set of categorical factor (s)"
      if(choix=="Independence") multiple<-T else multiple<-F
      X<-.var.type(X=X, info=info, data=data, type="factor", check.prod=F, message=msg3,  multiple=multiple, title="Variable-s", out=NULL)
      if(is.null(X)) {
        chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
        return(Resultats)}
      X$data->data
      X$X->X
      
      if(choix!="Adjustment"){
        msg4<-"Please choose the second set of categorical factor (s)"
        Y<-.var.type(X=Y, info=info, data=data, type="factor", check.prod=F, message=msg4,  multiple=multiple, title="Variable-s", out=NULL)
        if(is.null(Y)) {
          chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
          return(Resultats)}
        Y$data->data
        Y$X->Y
        if(choix=="McNemar test" & any(sapply(data[,c(X,Y)],nlevels)!=2)) {
          msgBox("McNemar's test involves a 2x2 array. The dimensions of your table are different.")
          print(table(data[,X], data[,Y], dnn=c(X,Y)))
          chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
          return(Resultats)
        }
      }
      
      if(dial){       
        if(info==T) writeLines("Should the analysis be weighted by an effective variable?")
        Effectifs<-dlgList(c("Yes", "non"), multiple = F, preselect="non", title="Specify workforce?")$res
        if(length(Effectifs)==0) {
          chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
          return(Resultats)}
        if(Effectifs=="non") Effectifs<-NULL}
      
      if(!is.null(Effectifs)){
        msg5<-"Please choose the variable (s) defining the workforce"
        .var.type(X=Effectifs, info=T, data=data, type="integer", message=msg5,multiple=F, title="Specify the actual numbers?", out=c(X, Y))->Effectifs
        if(is.null(Effectifs)) {
          chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
          return(Resultats)}
        Effectifs$X->Effectifs
      }
      
      # check variable
      if(!is.null(Effectifs)) sum(data[,Effectifs])->tot else length(data[,1])->tot
      if(choix!="Adjustment") {
        expand.grid(X, Y)->comb
        comb[which(as.vector(comb[,1])!=as.vector(comb[,2])),]->comb
        if(any(apply(comb, 1, function(x) prod(sapply(data[,x],nlevels)))>tot)){
          which(apply(comb, 1, function(x) prod(sapply(data[,x],nlevels)))>tot)->trop
          for(i in length(trop):1){
            msg6<-paste0("Les effectifs sont insuffisants pour le nombre de combinaisons entre la variable ", comb[trop[i],1], " et la variable ", comb[trop[i],2], ". This analysis will not be performed.")
            msgBox(msg6)
            comb[ -which(dimnames(comb)[[1]]==names(trop)[i]),]->comb
          }
          if(length(comb[,1])==0) {
            msgBox("The variables you have chosen to perform your analysis do not allow you to perform any analysis. Please redefine your analysis")
            return(NULL)
          } 
        }
      }
      
      if(choix=="Adjustment") {
        if(dial==F & is.null(p)) rep(1/nlevels(data[,X]),times=nlevels(data[,X]))->p
        if(sum(p)!=1 | any(p)>1 | any(p)<0) p<-NULL
        
        while(is.null(p)){
          if(info==T) writeLines("Please enter the probabilities corresponding to each category of the variable.")
          dlgForm(setNames(as.list(rep(1/nlevels(data[,X]),times=nlevels(data[,X]))), levels(data[,X])), "Probability vector. Warning: do not enter fractions")$res->niveaux
          stack(niveaux)[,1]->p
          if(sum(p)!=1 ||length(p)!=nlevels(data[,X]) | any(p)>1 | any(p)<0){
            if( dlgMessage("La somme des probabilites est differente de 1 ou le nombre de probabilites ne correspond pas au nombre de modalites de la variable.
                           Veuillez entrer un vecteur de probabilites valide","okcancel")$res=="cancel") {
              chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
              return(Resultats)} else return(NULL)
        } 
        }
        }
      if(choix=="McNemar test") robust<-F else robust<-T
      if(choix=="Adjustment") Bayes<-F else Bayes<-T 
      msg.options<-"In this case, the non-parametric test is the classic chi-square test."
      .ez.options(options="choice", n.boot=n.boot,param=F, non.param=T, robust=robust, Bayes=Bayes, msg.options1=NULL, msg.options2=msg.options, info=T, dial=dial, choix=choix2)->Options
      if(is.null(Options)){  chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
        return(Resultats)}
      if(dial==T || any(SampleType %in% c("poisson", "jointMulti","hypergeom", "indepMulti"))==F || SampleType=="indepMulti" & any(fixedMargin %in% c("rows","cols"))==F){
        
        if(any(Options$choix=="Bayesian factors") && choix== "Independence" ){
          if(info==T) {
            writeLines("Quel type d'echantillonnage  avez-vous realise pour votre analyse ?") 
            cat("Si l'effectif total est non fixe, on fait l'hypothese que les observations surviennent en respectant une loi de poisson.
                La repartition sur les niveaux d'un facteur surviennent avec une probabilite fixe. La distribution est une distribution poisson")
            print(matrix(c(100,50,200,100), nrow=2, ncol=2, dimnames=list(c("A.1", "A.2"), c("B.1", "B.2")) ))
            
            writeLines("L'option *Effectif total fixe* doit etre choisi si on fait l'hypohese nulle que la repartition dans chacune des cellules du tableau est fixee.
                       La distribution est une distribution multinomiale jointe")
            print(matrix(c(100,100,100,100), nrow=2, ncol=2, dimnames=list(c("A.1", "A.2"), c("B.1", "B.2")) ))
            
            writeLines("L'option Effectif total fixe pour les lignes* doit etre choisi si les effectifs pour chaque ligne est identique, 
                       comme lorsqu'on veut s'assurer d'un appariement entre groupes. La distribution est une distribution multinomiale independante")
            print(matrix(c(15,40,55, 85,60,145, 100,100,200), nrow=3, ncol=3, dimnames=list(c("A.1", "A.2", "total"), c("B.1", "B.2", "total")) ))
            writeLines("L'option Effectif total fixe pour les colonnes* est identique a la precedente pour les colonnes")
            writeLines("L'option *Effectif total fixe pour les lignes et les colonnes* lorsque les totaux pour les lignes et les colonnes sont fixes.La distribution est hypergeometrique")
            print(matrix(c(15,85,100, 85,15,100, 100,100,200), nrow=3, ncol=3, dimnames=list(c("A.1", "A.2", "total"), c("B.1", "B.2", "total")) ))
          } 
          SampleType<-c()
          FM<-c()
          for(i in 1:length(comb[,1])){
            
            if(nlevels(data[,as.character(comb[i,1])])==2 && nlevels(data[,as.character(comb[i,2])])==2) possible<- c("fish - Total number not fixed", "jointMulti - Total fixed workforce", 
                                                                                                                      paste("indepMulti - Total fixed headcount for lines - variable", comb[i,1]), 
                                                                                                                      paste("indepMulti - Fixed count for columns - variable", comb[i,2]), 
                                                                                                                      "hypergeom -  Effectif total fixe pour les lignes et les colonnes") else {
                                                                                                                        possible<- c("fish - Total number not fixed", "jointMulti - Total fixed workforce", 
                                                                                                                                     paste("indepMulti - Total fixed headcount for lines - variable", comb[i,1]), 
                                                                                                                                     paste("indepMulti - Fixed count for columns - variable", comb[i,2]))
                                                                                                                      }
            SampleType1<-dlgList(possible, preselect="Total workforce not fixed", multiple = FALSE, title=paste("Pan experimental between", comb[i,1], "et",comb[i,2], "?"))$res
            if(length(SampleType1)==0) {chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
              return(Resultats)}
            ifelse(SampleType1 == paste("indepMulti - Total fixed headcount for lines - variable", comb[i,1]), fixedMargin<-"rows",
                   ifelse(SampleType1 == paste("indepMulti - Fixed count for columns - variable", comb[i,2]), fixedMargin<-"cols", fixedMargin<-0))
            FM<-c(FM,fixedMargin )  
            ST<- switch(SampleType1, "fish - Total number not fixed"= "poisson",
                        "jointMulti - Total fixed workforce"="jointMulti",
                        "hypergeom -  Effectif total fixe pour les lignes et les colonnes"= "hypergeom")
            if(SampleType1==paste("indepMulti - Total fixed headcount for lines - variable", comb[i,1])) ST<-"indepMulti"
            if(SampleType1==paste("indepMulti - Fixed count for columns - variable", comb[i,2])) ST<-"indepMulti"
            SampleType<-c(SampleType, ST)
          }  
          
        }
      }
      
      list()->Resultats
      Resultats$analyse<-choix
      Resultats$data<-data
      Resultats$nom.data<-nom
      if(choix=="Adjustment") Resultats$Variables<-X else Resultats$Variables<-comb
      Resultats$Effectifs<-Effectifs
      Resultats$p<-p
      Resultats$choix<-Options$choix
      Resultats$n.boot<-Options$n.boot
      Resultats$SampleType<-SampleType
      Resultats$fixedMargin<-FM
      return(Resultats)
      } 
    Cramer<-function(chi.r){
      x<-chi.r$statistic
      n<-sum(chi.r$expected)
      dims<-dim(chi.r$expected)
      V<-round((x/((min(dims)-1)*n))^0.5,3)
      V.sq<-round(V^2,3)
      resultats<-data.frame("V"=V, "V.carre"=V.sq)
      return(resultats)}
    chi.out<-function(data=NULL, X=NULL, Y=NULL, p=NULL, choix=NULL, Effectifs=NULL, n.boot=NULL, SampleType=NULL,
                      fixedMargin=NULL, choix2=NULL, rscale=2^0.5/2,priorConcentration=1){
      Resultats<-list()
      if(choix=="Adjustment"){
        if(!is.null(Effectifs)){
          tapply(data[,Effectifs], data[,X],sum,na.rm=TRUE)->tab
          rbind(tab,p, p*sum(data[,Effectifs]))->Distribution} else {
            table(data[,X])->tab
            rbind(tab, p, sum(tab)*p)->Distribution}
        dimnames(Distribution)[[1]]<-c("Observations", "probabilities","Expected")
        Resultats$"Summary table"<-Distribution 
        chi<-chisq.test(tab, p=p, B=n.boot)
        Resultats$"chi.two adjustment"<-data.frame(chi.deux=round(chi$statistic,3), ddl=chi$parameter) 
        if(any(choix2== "Non-parametric test")) Resultats$"chi.two adjustment"$valeur.p<-round(chi$p.value,4)
        if(!is.null(n.boot) && n.boot>1){
          Resultats$"chi.two adjustment"$"Estimated value of p by Monte Carlo simulation"<-round(chisq.test(tab, B=n.boot, simulate.p.value=T, correct=F)$p.value,4)}
        
      }
      if((choix!="Adjustment")){
        if (is.null(Effectifs)) tab<-table(data[,X],data[ ,Y], dnn=c(X, Y))else {
          tab<-tapply(data[,Effectifs],list(data[,X],data[,Y]),sum,na.rm=TRUE) 
          tab[is.na(tab)] <- 0
          as.table(tab)->tab
          names(attributes(tab)$dimnames)<-c(X,Y)
        }
        # graphique   
        spineplot(tab, col=topo.colors(nlevels(data[,Y])))
        table.margins(tab)->Resultats$"Workforce Observations"
        if(choix=="Independence"){
          mon.chi<-chisq.test(tab, B=n.boot, correct=F)
          mon.chi$expected->Resultats$"Expected workforce"
          if(any(choix2 %in% c("Non-parametric test","Robust testing - involving bootstraps")))    {
            SY<-data.frame( "chi.deux"=round(mon.chi$statistic,4), 
                            "dof"=mon.chi$parameter, Cramer(mon.chi))
            if(any(choix2=="Non-parametric test")) SY$valeur.p<-round(mon.chi$p.value,4) 
            try(fisher.test(tab),silent=T)->fisher
            if(class(fisher)!="try-error") SY$Fisher.Exact.Test=round(fisher$p.value,4)
            if(all(dim(tab)==2)){
              mon.chi<-chisq.test(tab, B=n.boot, correct=T)
              AY<-data.frame("chi.deux"=round(mon.chi$statistic,4),"dof"=mon.chi$parameter,   Cramer(mon.chi),valeur.p=round(mon.chi$p.value,4) ,Fisher.Exact.Test="" )
              if(any(choix2=="Non-parametric test")) AY$valeur.p<-round(mon.chi$p.value,4)
              SY<-rbind(SY, AY)
              dimnames(SY)[[1]]<-c("Without Yates correction", "With Yates correction") 
            } else dimnames(SY)[[1]][1]<-c("Without Yates correction")
            if(!is.null(n.boot) && n.boot>1){
              SY$"P-value by Monte Carlo simulation"<-chisq.test(tab, B=n.boot, simulate.p.value=T, correct=F)$p.value
            } 
            Resultats$"Main analysis"<-SY
            # Rapport de vraisemblance 
            RV<-2* sum(mon.chi$observed[which(mon.chi$observed!=0)] * 
                         log(mon.chi$observed[which(mon.chi$observed!=0)]/mon.chi$expected[which(mon.chi$observed!=0)],base=exp(1)))
            PRV<-pchisq(RV, mon.chi$parameter, ncp = 0, lower.tail = F, log.p = FALSE)
            p<-mon.chi$observed/sum(mon.chi$observed)
            q<-mon.chi$expected/sum(mon.chi$expected)
            RVES<-(-1/(log(min(q[which(p!=0)]), base=exp(1)))) *sum(p *log(p[which(p!=0)]/q[which(p!=0)], base=exp(1))) # ES from JOHNSTON et al. 2006
            RV<-data.frame("chi.carre"=RV, "dof"=mon.chi$parameter, "p-value"=round(PRV,4), "Size.effect"=round(RVES,4))
            Resultats$"Likelihood ratio (G test)"<-RV
          }
          # facteur bayesien
          if(any(choix2=="Bayesian factors")) {
            if(!is.null(fixedMargin) && fixedMargin==0) fixedMargin<-NULL
            bf<-contingencyTableBF(tab, sampleType = SampleType, fixedMargin = fixedMargin, priorConcentration=priorConcentration)
            bf<-ifelse(extractBF(bf, onlybf=T)>1000, ">1000", ifelse(extractBF(bf, onlybf=T)<.001, "<0.001",round(extractBF(bf, onlybf=T),4)))
            bf<-data.frame("Bayesian factor"=c(bf, ifelse(class(bf)=="character", "<0.001", round(1/bf,4)),SampleType))
            dimnames(bf)[[1]]<-c("In favor of the alternative hypothesis", "In favor of the null hypothesis", "Type")
            Resultats$"Bayesian factor"<-bf 
          }
          
          # Odd ratio 
          as.matrix(tab)->tab
          if(all(dim(tab)>2) |any(mon.chi$observed==0)) {
            "We cannot calculate ORs for arrays larger than 2x3 or arrays containing 0s"->Resultats$"Odd ratio"
          }else{
            if(length(tab[1,])>2) tab<-apply(tab,1, rev)
            Resultats$"Odd ratio"<- oddsratio.wald(x=tab,conf.level = 0.95,rev = c("neither"),correction = FALSE,verbose = FALSE)$measure
          }
          if(any(choix2 %in% c("Non-parametric test","Robust testing - involving bootstraps")))      {
            if(is.null(SY$"P-value by Monte Carlo simulation")) p<-SY$valeur.p else p<-SY$"P-value by Monte Carlo simulation"
            if(p<0.05)  {
              round(mon.chi$residuals,3)->Resultats$"Residues"
              round((mon.chi$observed-mon.chi$expected)/(mon.chi$expected^0.5),3)->Resultats$"Standardized residues"
              round(mon.chi$stdres,3)->Resultats$"Adjusted standardized residues"
              p.adjust(2*(1-pnorm(abs(Resultats$"Adjusted standardized residues"))), method="holm")->valeur.p
              matrix(valeur.p, nrow=nrow(tab))->valeur.p
              dimnames(tab)->dimnames(valeur.p)
              round(valeur.p,4)->Resultats$"Significance of residues - probability corrected by applying Holm's method"
            }
          }
          round(table.margins(prop.table(mon.chi$observed))*100,1)->Resultats$"Total percentage"
          round(sweep(addmargins(mon.chi$observed, 1, list(list(All = sum, N = function(x) sum(x)^2/100))), 2,apply(mon.chi$observed, 2, sum)/100, "/"), 1)->Resultats$"Percentage by column"
          round(sweep(addmargins(mon.chi$observed, 2, list(list(All = sum, N = function(x) sum(x)^2/100))), 1,apply(mon.chi$observed, 1, sum)/100, "/"), 1)->Resultats$"Percentage by line"
          
        }
        if(choix=="McNemar test"){
          if(any(choix2== "Non-parametric test"))    {
            MCN<-mcnemar.test(tab, correct=F)
            MCN<-data.frame("chi.deux"=round(MCN$statistic,3), "dof"=MCN$parameter, "p-value"= round(MCN$p.value,4))
            MCN2<-mcnemar.test(tab, correct=T)
            MCN2<-data.frame("chi.deux"=round(MCN2$statistic,3), "dof"=MCN2$parameter, "p-value"= round(MCN2$p.value,4))
            MCN<-rbind(MCN, MCN2)
            dimnames(MCN)[[1]]<-c("McNemar test without continuity correction", "McNemar test with continuity correction" )
            MCN->Resultats$"McNemar test with Yates correction" # test de McNemar    
          }
          if(any(choix2=="Bayesian factors")) {
            bf<-proportionBF(y=tab[1,2], tab[1,2]+tab[2,1], p=0.5,rscale=rscale)
            erreur<-bf@numerator[[1]]@analysis$properror
            erreur<-ifelse(erreur<.0001, "<0.0001", erreur)
            bf<-ifelse(extractBF(bf, onlybf=T)>1000, ">1000", ifelse(extractBF(bf, onlybf=T)<.001, "<0.001",extractBF(bf, onlybf=T)))
            samples =proportionBF(y = tab[1,2], N = tab[1,2]+tab[2,1], p = .5, posterior = TRUE, iterations = 10000)
            plot(samples[,"p"])
            bf<-data.frame("Bayesian factor"=c(bf, ifelse(class(bf)=="character", "<0.001", round(1/bf,4)), erreur, rscale))
            dimnames(bf)[[1]]<-c("In favor of the alternative hypothesis", "In favor of the null hypothesis", "error", "rscale")
            Resultats$"Bayesian factors"<-bf
          }
          
          if( all(dimnames(tab)[[1]]==dimnames(tab)[[2]])) Resultats$Avertissement<-"Les cellules utilisees pour le calcul du McNemar  sont celles de la 1e ligne 2e colonne et de la 2e ligne 1e colonne" else
            Resultats$Avertissement<-"McNemar test: the modalities are not the same for the McNemar test. Is this a factor capable of repeated?"}
        
      }
      return(Resultats) 
    }
    
    c("svDialogs", "epitools", "BayesFactor", "ggplot2")->packages
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    .e <- environment()
    Resultats<-list()
    
    
    if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data 
    
    chi.in(X=X, Y=Y, Effectifs=Effectifs,p=p, choix=choix, data=data, info=info, n.boot=n.boot, SampleType=SampleType, FM=fixedMargin, choix2=choix2)->chi.options
    if(is.null(chi.options)) return(analyse())
    if(chi.options!="Adjustment"){
      try( windows(record=T), silent=T)->win
      if(class(win)=="try-error") quartz()
    }
    
    
    if(class(chi.options$Variables)=="data.frame") {
      X<- chi.options$Variables[,1]
      Y<- chi.options$Variables[,2]
    } else {X<-chi.options$Variables
    Y<-NULL}
    
    if(length(X)>1) Resultats$"Alpha warning"<-paste("you multiply the error of the 1st kind. The risk of making a mistake of the first kind is", 100*(1-0.95^length(X)), "%", sep=" ")  
    for(i in 1:length(X)) {
      as.character(X[i])->Xi
      as.character(Y[i])->Yi
      chi.results<-chi.out(data=chi.options$data, X=Xi, Y=Yi,p=chi.options$p, choix=chi.options$analyse, 
                           Effectifs =chi.options$Effectifs, n.boot=chi.options$n.boot, choix2=chi.options$choix,
                           SampleType=chi.options$SampleType[i],  fixedMargin=chi.options$fixedMargin[i], rscale=rscale, priorConcentration =priorConcentration)
      Resultats[[i]]<-chi.results
      if(chi.options$analyse=="Adjustment") nom<-paste("chi-square adjustment on the variable", X, sep =" ")
      if(chi.options$analyse=="Independence") nom<-paste("Results of the match of two between the variable", Xi,
                                                         "and the variable", Yi,sep=" ")
      if(chi.options$analyse=="McNemar test") nom<-paste("McNemar test results between variable", Xi,
                                                            "and the variable", Yi,sep=" ")
      names(Resultats)[i]<-nom
    } 
    
    paste(unique(X), collapse="','", sep="")->X
    if(!is.null(Y)) paste(unique(Y), collapse="','", sep="")->Y
    paste(chi.options$choix, collapse="','", sep="")->choix2
    paste(chi.options$p, collapse=",", sep="")->p
    if(!is.null(chi.options$SampleType)) paste(chi.options$SampleType, collapse="','", sep="")->SampleType
    paste(chi.options$fixedMargin, collapse="','", sep="")->FM
    paste0("chi(X=c('", X,ifelse(!is.null(Y), paste0("'),Y=c('", Y, "')"), "'), Y=NULL"), 
           ifelse(is.null(chi.options$Effectifs),",Effectifs=NULL", paste0(",Effectifs='", chi.options$Effectifs, "'")),
           ifelse(!is.null(Y), ", p=NULL", paste0(", p=c(", p,")")), 
           ", choix='", chi.options$analyse, "',data=", chi.options$nom.data, ",info=", info, ",n.boot=", ifelse(is.null(chi.options$n.boot), "NULL",chi.options$n.boot) , 
           ",priorConcentration =" ,priorConcentration, ",SampleType=", ifelse(is.null(chi.options$SampleType), 'NULL', paste0("c('",SampleType,"')")), 
           ",fixedMargin=", ifelse(is.null(chi.options$fixedMargin), 'NULL', paste0("c('",FM,"')")), ",choix2=c('",choix2,
           "'),rscale=", round(rscale,3), ")")->Resultats$Call
    .add.history(data=chi.options$data, command=Resultats$Call, nom=chi.options$nom)
    .add.result(Resultats=Resultats, name =paste(chi.options$analyse, Sys.time() ))
    
    
    ref1(packages)->Resultats$"References"
    ### Obtenir les Resultats
    if(html) try(ez.html(Resultats))
    return(Resultats) 
    }
