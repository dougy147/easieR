test.t <-
  function(X=NULL, Y=NULL, group=NULL, choice=NULL,
           sauvegarde=F, outlier=c("Complete data",  "Identification of influential values","Data without influencing value"),  z=NULL, data=NULL,
           alternative="two.sided", mu=NULL, formula=NULL, n.boot=NULL, 
           param=c("parametric test", "non-parametric test","Robust testing - involving bootstraps", 
                   "Bayesian factors"), info=TRUE, rscale=0.707, html=T){
    # X : Character specifying the dependant variable in dataframe. 
    # Y : character specifying either a two levels factor in dataframe or a numeric variable if paired is TRUE
    # group : Factor vector allowing to decompose analysis by group in one sample t test
    # choice : Character. One among c("Comparison to a standard", "Two matched samples","Two independent samples")
    # sauvegarde : logical. Should the results be saved ? 
    # outlier : character. One or several possibilities among c("Complete data",   "Identification of influential values", "Data without influencing value")
    # z : if NULL and the identification/exclusion of outlier is desired, outlier are identified on Grubbs' test. If z is numeric, outliers are identified on abs(z)
    # data : data on which analysis has to be performed. 
    # alternative : one among c("greater", "lower", "two.sided"). Two sided is default. 
    # formula : a formula of the form dependant.variable~independant.variable
    # n.boot : number of bootstrap. Must be a positive value
    # param : character vector with one or several choices among c("parametric test", "non-parametric test","Robust testing - involving bootstraps", "Bayesian factors")
    # info : logical. If dialog box are used, Should information be printed in the console
    # rscale : if "Bayesian factors is choosen in "param", rscale is the prior scale. See t.testBF for more information
    
    #### 5 fonctions qui seront appelees pour realiser l'analyse
    test.t.in<-function(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                        formula=NULL,n.boot=NULL, rscale=NULL, mu=NULL){
      
      Results<-list()
      if(!is.null(choice)) dial<-F else dial<-T
      if(is.null(choice) || (choice %in%c("Comparison to a standard", "Two matched samples","Two independent samples")==FALSE)){
        if(info) writeLines("Please specify the type of t test you wish to perform.")
        choice<-dlgList(c("Comparison to a standard", "Two matched samples",
                         "Two independent samples"), preselect=NULL, multiple = FALSE, title="Choice of t-test")$res
        if(length(choice)==0) return(NULL)
      }
      data<-choice.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL)
      nom<-data[[1]]
      data<-data[[2]]
      if(is.null(Y) || class(data[,Y]) == "factor") format<-"long" else format<-"wide"
      
      if(is.null(formula)){
        if(choice=="Two matched samples"){
          if(dial){
            if(info==TRUE){
              time1<-1:3
              time2<-4:6
              data.frame("time1"=time1,"time2"=time2)->wide
              data.frame(c(rep("time1",3),rep("time2", 3)), 1:6)->long
              names(long)<-c("moment","mesure")
              writeLines("this is the wide format")
              print(wide)
              writeLines("this is the long format")
              print(long)}
            format<-dlgList(c("wide", "long"), preselect="wide", multiple = FALSE, title="What is the format of your data?")$res
            if(length(format)==0) {
              Results<-test.t.in(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                   formula=NULL,n.boot=NULL, rscale=NULL)
              return(Results)
            }
          }}  
        if(format=="wide") {
          msg3<-"Please choose time 1."
          msg4<-"Please choose time 2."
          title1<-"time 1"
          title2<-"time 2"
        } else{
          msg3<-"Please choose the dependent variable."
          msg4<-"Please choose the independent variable."
          title1<-"Dependent variable"
          title2<-"Independent variable"
          
        }
        
        if(choice=="Two matched samples") {multiple<-F 
        if(length(X)>1){
          msgBox("There can be only one dependent variable for the student's t's for matched samples")
          X<-NULL }}else multiple<-T
          X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=multiple, title=title1, out=NULL)
          if(is.null(X)) {
            test.t.in(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                      formula=NULL,n.boot=NULL, rscale=NULL)->Results
            return(Results)}
          data<-X$data
          X1<-X$X
          
          if(choice!="Comparison to a standard"){
            if(choice=="Two matched samples" && format=="wide") type<-"numeric" else type<-"factor"
            Y<-.var.type(X=Y, info=info, data=data, type=type, check.prod=F, message=msg4,  multiple=FALSE, title=title2, out=X1)
            if(is.null(Y)) {
              test.t.in(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                        formula=NULL,n.boot=NULL, rscale=NULL)->Results
              return(Results)}
            data<-Y$data
            Y<-Y$X 
            if(class(data[,Y])=="factor" && nlevels(data[,Y])!=2) {
              msgBox("You must use a categorical independent variable with 2 modalities")
              test.t.in(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                        formula=NULL,n.boot=NULL, rscale=NULL)->Results
              return(Results)
            }
          } 
      } else {
        X1<-as.character(formula[2])
        Y<-as.character(formula[3])
      }
      
      
      
      
      
      if(choice=="Two matched samples"){
        if(format=="wide"){
          if(dial){
            if(info==TRUE)writeLines("Please give a name to the independent variable. Giving an explicit name to the independent variable will make reading the results more readable")
            nomVI <- dlgInput("What is the name of the independent variable?", "Moment")$res
            if(length(nomVI)==0) {
              Results<-test.t.in(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                   formula=NULL,n.boot=NULL, rscale=NULL)
              return(Results)
            }
            strsplit(nomVI, ":")->nomVI
            tail(nomVI[[1]],n=1)->nomVI
            if(info==TRUE) writeLines("Please give a name to the dependent variable. Giving an explicit name to the dependent variable will make reading the results more readable")
            nomVD <- dlgInput("What is the name of the dependent variable?", "Result")$res
            if(length(nomVD)==0) {
              Results<-test.t.in(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                   formula=NULL,n.boot=NULL, rscale=NULL)
              return(Results)
            }
          } else {
            nomVD<-"Result"
            nomVI<-"Moment"
          }
          strsplit(nomVD, ":")->nomVD
          tail(nomVD[[1]],n=1)->nomVD
          data[complete.cases(data[,c(X1, Y)]),]->data
          data$IDeasy<-paste0("p", 1:length(data[,X1]))
          melt(data=data, measure.vars=c(X1,Y) , variable.name=nomVI, value.name=nomVD)->data
          assign(x=paste0(nom,".format.long"), value=data, envir=.GlobalEnv)
          X1<-nomVD
          Y<-nomVI
        }
        if(format=="long") {
          if( length(unique(table(data[,Y])))!=1) {
            msgBox("The number of occurrences for each modality of your independent variable is not the same. Please choose a participant ID") 
            msg4<-"Please choose the variable identifying the participants"
            ID<-.var.type(X=NULL, info=info, data=data, type=type, check.prod=F, message=msg4,  multiple=multiple, title="Variable *Identifiant*", out=c(X1,Y))
            if(is.null(ID)) {
              test.t.in(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                        formula=NULL,n.boot=NULL, rscale=NULL)->Results
              return(Results)}
            ID<-ID$X 
            ID.fail<-names(which(table(data[,ID])!=2))
            data<-data[which(data[,ID]!=ID.fail),]
            data<-data[order(data[,c(Y,ID)]), ]
          } else {
            data[order(data[,Y]),]->data
            data$IDeasy<-rep(paste0("p", 1:(length(data[,X1])/2)), 2) 
          }
        }
        
      }
      
      if(choice=="Comparison to a standard"){
        writeLines("Please specify the value of the standard")
        if(class(mu) !="numeric") mu<-NA
        while(is.na(mu)){
          mu <- dlgInput("What is the value of the standard?", 0)$res
          if(length(mu)==0) { test.t.in(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                        formula=NULL,n.boot=NULL, rscale=NULL)->Results
            return(Results)}
          strsplit(mu, ":")->mu
          tail(mu[[1]],n=1)->mu
          as.numeric(mu)->mu
          if(is.na(mu)) msgBox("The standard must be a numeric value.")  
        }
        if(dial){
          
          
          if(info==TRUE) writeLines("Une analyse bilaterale teste l'existence d'une difference. Le choice superieur teste si la mediumne est strictement superieure
                                    \n Le choice inferieur teste l'existence d'une difference strictement inferieure")
          dlgList(c("Bilateral", "Superior", "Inferior"), preselect=NULL, multiple = FALSE, title="Comparison of means")$res->alternative
          if(length(alternative)==0) {
            test.t.in(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                      formula=NULL,n.boot=NULL, rscale=NULL)->Results
            return(Results)
          } else car::recode(alternative, "'Bilateral' = 'two.sided'; 'Superior' = 'greater'; 'Inferior' = 'less'")->alternative
          
          if(info==TRUE) writeLines("Si vous souhaitez realiser l'analyse pour differents sous-echantillons en fonction d'un critere categoriel (i.e; realiser une analyse par groupe)
                                    \n choisissez Yes. Dans ce cas, l'analyse est realisee sur l'echantillon complet et sur les sous-echantillons.
                                    \n Si vous desirez l'analyse pour l'echantillon complet uniquement, chosissez non.
                                    \n l'analyse par groupe ne s'appliquent pas aux statisticals robusts.")
          dlgList(c("Yes", "non"), preselect="non", multiple = FALSE, title="Group analysis?")$res->par.groupe
          if(length(par.groupe)==0) {
            test.t.in(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                      formula=NULL,n.boot=NULL, rscale=NULL)->Results
            return(Results)
          } 
          msg5<-"Please choose the categorical ranking factor."
          if(par.groupe=="Yes"){group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=F, message=msg5,  multiple=FALSE, title="Variable-s", out=X1)
          if(length(group)==0) { test.t.in(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                           formula=NULL,n.boot=NULL, rscale=NULL)->Results
            return(Results)}
          data<-group$data
          group<-group$X 
          }
        }
      }
      msg.options1<-"The parametric test is the classic t test"
      msg.options2<- "The non-parametric test is the Wilcoxon (or Mann-Whitney) test"
      
      options<-.ez.options(options=c("choice","outlier"), n.boot=n.boot,param=T, non.param=T, robust=T, Bayes=T, msg.options1=msg.options1, msg.options2=msg.options2, info=info, dial=dial, 
                           choice=param,sauvegarde=sauvegarde, outlier=outlier, rscale=rscale)
      if(is.null(options)){
        test.t.in(X=NULL, Y=NULL, data=NULL, choice=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                  formula=NULL,n.boot=NULL, rscale=NULL)->Results
        return(Results)
      }
      Results$choice<-choice
      Results$nom<-ifelse(format=="wide", paste0(nom,".format.long"), nom)
      Results$data<-data
      Results$X<-X1
      if(exists("Y")) Results$Y<-Y
      if(exists("mu")) Results$mu<-mu
      if(exists("alternative")) Results$alternative<-alternative
      if(exists("group")) Results$group<-group
      Results$options<-options
      return(Results)
      }
    
    norme<-function(X, mu, data, param=c("param", "non param", "robusts"), group=NULL, alternative="two.sided", n.boot=NULL, rscale=0.707){
      if(class(data)!="data.frame") {data<-data.frame(data)
                                     names(data)[1]<-X}
      Results<-list()
      .e <- environment()
      Results$"descriptive statistics"<-.stat.desc.out(X=X, groupes=NULL, data=data, tr=.1, type=3, plot=F)
      cutoff <- data.frame(x = c(-Inf, Inf), y = mu, cutoff = factor(mu) )
      p2<- ggplot(data)
      p2<-p2+ eval(parse(text=paste0("aes(x=factor(0), y=", X,")"))) + geom_violin()
      p2<-p2+geom_line(aes( x, y, linetype = cutoff ), cutoff)
      p2<-p2+ labs( x=" ")
      p2<-p2 + stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9))
      p2<-p2 + geom_dotplot(binaxis='y', stackdir='center', dotsize=1/4)
      p2<-p2 + theme(legend.position="none")
      p2<-p2+theme(plot.title = element_text(size = 12))+ggtitle("Mean and standard deviation")
      # print(p2)
      Results$"descriptive statistics"$Graphique<-p2
      
      if(!is.null(group)) {Results$"descriptive statistics by group"<-.stat.desc.out(X=X, groupes=group, data=data, tr=.1, type=3, plot=T) }
      if(any(param=="param") | any(param=="Parametric test")){
        Results$"Normality tests"<-.normalite(data=data, X=X, Y=NULL)
        t.test(data[,X], mu = mu, paired = FALSE, conf.level = 0.95, alternative=alternative)->ttest
        ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R.carre
        cohensD(data[,X], mu=mu)->dc
        data.frame("t test"=round(ttest$statistic,3), "dof"=ttest$parameter, "p-value"=round(ttest$p.value,4), "Lower CI limit"=ttest$conf.int[[1]], "Lim.up.CI"=ttest$conf.int[[2]], 
                   "R.carre"=round(R.carre,4), "Dr. Cohen"=round(dc,3))->ttest
        dimnames(ttest)[1]<-" "
        ttest->Results$"Student's test - comparison to a norm"
        if(!is.null(group)){
          data<-data[complete.cases(data[,group]),]
          func <- function(data, moy=mu){ 
            t.test(data, mu = moy)->ttest
            ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R.carre
            cohensD(data[,1], mu=moy)->dc
            return(data.frame(test.t=round(ttest$statistic,3), 
                              dof=ttest$parameter, 
                              p-value=round(ttest$p.value,4), 
                              IC.inf=ttest$conf.int[[1]], 
                              IC.sup=ttest$conf.int[[2]], 
                              "R.carre"=round(R.carre,4), 
                              D.Cohen=round(dc,3)))}
          data.frame(data[,X])->Y
          
          ddply(.data=Y, .(data[,group]), func)->t.groupes
          t.groupes->Results$"Student's t per group"}}
      
      if(any(param=="Bayes") | any(param=="Bayesian factors") ){
        if(all(param!="param") & all(param!="Parametric test")) Results$"Normality tests"<-.normalite(data=data, X=X, Y=NULL)
        
        BF<-ttestBF(x = data[,X], mu=mu , paired=FALSE, rscale=rscale)
        BF<-extractBF(BF, onlybf=F)
        BF<-data.frame("Bayesian factor"=c(round(BF$bf,5), round((1/BF$bf),5)), "Error"=round(c( BF$error, BF$error),5))
        dimnames(BF)[[1]]<-c("In favor of the alternative hypothesis", "In favor of the null hypothesis")
        Results$"Bayesian factors"<-BF
        if(!is.null(group)){
          func <- function(data, moy=mu, scale=rscale){ 
            ttestBF(data, mu = moy, rscale=scale)->BF
            BF<-extractBF(BF, onlybf=F)
            return(data.frame("Bayesian factor"=round(BF$bf,5), "Error"=round(BF$error,5)))
          }
          BFgroup<-tapply(X=data[,X], data[,group], func,scale=rscale, moy=mu)
          BFgroup<-matrix(unlist(BFgroup), ncol=2, byrow=T)
          dimnames(BFgroup)<-list(levels(data[,group]), c("FB", "error"))
          BFgroup->Results$"Bayesian factor by group"
        }
        samples<-ttestBF(x = data[,X], mu=mu , paired=FALSE, rscale=rscale, posterior=T, iterations = ifelse(is.null(n.boot), 1000, n.boot))
        plot(samples[,"mu"])
        
        
        bfs<-c()
        for (i in 5:length(data[,X])) {
          bfm <- ttestBF(x = data[,X][1:i], mu=mu,paired=FALSE, rscale=0.707)
          bfl <- ttestBF(x = data[,X][1:i], mu=mu,paired=FALSE, rscale=1)
          bful <- ttestBF(x = data[,X][1:i], mu=mu,paired=FALSE, rscale=1.41)
          bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
        }
        
        SBF<-data.frame("n"=rep(5:length(data[,X]), each=3 ),"BF"= bfs, 
                        "rscale"=factor(rep(c("medium", "wide", "ultra wide"), length.out= 3*(length(data[,X])-4) )))
        names(SBF)<-c("n", "BF", "rscale")
        reorder( c("medium", "wide", "ultra wide"),levels(SBF$rscale))->levels(SBF$rscale)
        Results$"Sequential Bayesian factors"<-.plotSBF(SBF)
        
        ##### Debut du graphique  Bayes Factor Robustness Check     
        
        # what is the t-value for the data?
        tVal <-  t.test(data[,X], mu = mu, paired = FALSE, conf.level = 0.95, alternative=alternative)$statistic
        # how many points in the prior should be explored?
        nPoints <- 1000
        # what Cauchy rates should be explored?
        cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
        # what effect sizes should be plotted?
        effSize <- seq(from = -2, to = 2, length.out = 1000)
        
        # get the Bayes factor for each prior value
        bayesFactors <- sapply(cauchyRates, function(x) 
          exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = x)[['bf']]))
        
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 0.707)[['bf']])->r1
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1)[['bf']])->r2
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1.41)[['bf']])->r3
        plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
        # do the Bayes factor plot
        plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48", 
             ylim = c(0, max(bayesFactors)), xaxt = "n", 
             xlab = "Cauchy Prior Width (r)", ylab = "Bayes Factor (10)")
        abline(h = 0, lwd = 1)
        abline(h = 6, col = "black", lty = 2, lwd = 2)
        axis(1, at = seq(0, 1.5, 0.25))
        # add the BF at the default Cauchy point
        points(0.707, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
        points(1, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
        points(2^0.5, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
        # add legend
        legend(x="topright", legend = c("r = 0.707 - medium", "r = 1 - wide ", "r = 1.41 - ultrawide"),
               pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
               col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")
        
      }
      
      if(any(param=="non param")| any(param=="Non-parametric test")){
        
        wilcox.test(x= data[,X], y = NULL, alternative = alternative, mu = mu, paired = FALSE, exact = T,  
                    conf.int = TRUE, conf.level = 0.95)
        WT<-wilcox.test(data[,X],y=NULL, mu=mu, alternative, conf.int=T, conf.level=0.95)
        if(alternative!="two.sided")  abs(qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
        r<-z/(length(data[,X]))^0.5
        Results$Wilcoxon<- data.frame("Wilcoxon W"=WT$statistic, "p-value"=round(WT$p.value,4), "z"=round(z,4), "r"=round(r,4),
                                        "lower CI limit"=WT$conf.int[1],"sup.ci limit"=WT$conf.int[2])
        
        if(!is.null(group)){
          func <- function(data,Y=X, moy=mu, alt=alternative){
            WT<-wilcox.test(data[,Y],mu=moy, alternative=alt)
            if(alt!="two.sided") abs( qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
            r<-z/(length(data[,X]))^0.5
            return(data.frame(Wilcoxon.W=WT$statistic, p-value=round(WT$p.value,4), z=round(z,4), r=round(r,4)))
          }
          
          ddply(.data=data, .(data[, group]), func)->Wilcox.groupes
          Wilcox.groupes->Results$"Wilcoxon by group"
        }
      }
      
      if(any(param=="robusts"| any(param=="Robust testing - involving bootstraps"))){
        try( round(unlist(WRS::trimci(data[,X],tr=.2,alpha=.05, null.value=mu)),4), silent=T)->m.tr
        if(m.tr!="try-error"){
          names(m.tr)<-c("lower CI limit","sup.ci limit", "M.tronquee","test.t", "se","p-value","n")
          m.tr->Results$'Test sur la mediumne tronquee a 0.2' 
          data[,X]->x
          try(WRS::trimcibt(x, tr=.2,alpha=.05,nboot=n.boot,plotit=T,op=3)$ci, silent=T)->trimci
          try(WRS::mestci(x,alpha=.05,nboot=n.boot,bend=1.28,os=F),silent=T)->M.estimator
          try(WRS:: momci(x,alpha=.05,nboot=n.boot),silent=T)->MoM
          IC.robusts<-data.frame()
          if(class(trimci)!="try-error") {IC.robusts<-rbind(IC.robusts,trimci)
          dimnames(IC.robusts)[[1]][1]<-"bootstrap-t method"}
          if(class(M.estimator)!="try-error") {IC.robusts<-rbind(IC.robusts,M.estimator$ci)
          dimnames(IC.robusts)[[1]][length(IC.robusts[,1])]<-"M-estimator"}
          if(class(MoM)!="try-error") {IC.robusts<-rbind(IC.robusts,MoM$ci)
          dimnames(IC.robusts)[[1]][length(IC.robusts[,1])]<-"M-estimator modifiess"}
          if(all(dim(IC.robusts)!=0)) names(IC.robusts )<-c("lower CI limit", "sup.ci limit")
          Results$Robustes<-IC.robusts
          c("The bootstrap-t method is a bootstrap suitable for calculating the truncated mean", 
            " This index is suitable in most situations. The modified M-estimator should be preferred for N <20",
            "The truncation on the M-estimator adapts according to the characteristics of the sample.")->Results$infos
        } else Results$Robustes<-"Robust statistics could not be obtained. Verify that the WRS packages are correctly installed"
      }
   
      return(Results)
    }
    apparies<-function(X, Y, data=NULL, param=c("param", "non param", "robusts"),alternative="two.sided", n.boot=NULL, rscale=0.707){
      Results<-list()
      .e <- environment()
      Results$"descriptive statistics"<-.stat.desc.out(X=X, groupes=Y, data=data, tr=.1, type=3, plot=T)
      wide<-data.frame("t1"=data[which(data[,Y]==levels(data[,Y])[1]), X], "t2"=data[which(data[,Y]==levels(data[,Y])[2]), X])
      if(any(param=="param") | any(param=="Parametric test")){
        wide$diff<--wide$t2-wide$t1
        Results$"Normality tests"<-.normalite(data=wide, X="diff", Y=NULL)
        t.test(data[,X]~data[,Y], paired = TRUE, conf.level = 0.95, alternative=alternative)->ttest
        ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R.carre
        cohensD(x= wide[,1], y=wide[,2], method="paired")->dc
        data.frame("t test"= round(ttest$statistic,3), "dof"= ttest$parameter, "p-value"= round(ttest$p.value,4), "Lower CI limit"= ttest$conf.int[[1]], 
                   "Lim.up.CI"=ttest$conf.int[[2]], "R.carre"=round(R.carre,4), "D de Cohen"=round(dc,3))->ttest
        dimnames(ttest)[1]<-" "
        ttest->Results$"Student's test - comparison of two matched samples"}
      if(any(param=="param") | any(param=="Parametric test", any(param=="Bayes") | any(param=="Bayesian factors"))) {
        # realisation du graphique
        X1<-which(names(data)==X)
        nonaj<-ggplot(data) 
        nonaj<- nonaj+eval(parse(text=paste0("aes(x=", Y, ", y=", X,")")))
        # aes(x=data[,Y], y=data[,X1]))+labs(x=Y, y=X)+
        nonaj<- nonaj+ stat_summary(fun.y=mean, geom="bar",fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
        nonaj<-nonaj+theme(plot.title = element_text(size = 12))+ggtitle("Unadjusted data")
        # realisation du graphique ajuste propose par Loftus et Masson 1994 (pour plus d informations voir l article)
        Results$"Mean and standard deviation for unadjusted data"<-nonaj
        wide$meanD2<-(wide[ ,1]+wide[ ,2])/2
        mean(wide$meanD2)->GMean
        GMean-wide$meanD2->wide$adj
        wide$adjM1<-wide[ ,1]+wide$adj
        wide$adjM2<-wide[ ,2]+wide$adj
        data[,paste0(X, ".ajustee")]<-c(wide$adjM1,wide$adjM2)
        
        aj<-ggplot(data)
        aj<-aj+eval(parse(text=paste0("aes(x=", Y, ", y=", names(data)[length(data)],")")))
        aj<-aj+labs(x=Y, y=X)+stat_summary(fun.y=mean, geom="bar", 
                                           fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
        aj<-aj+theme(plot.title = element_text(size = 12))+ggtitle("Adjusted data (Loftus "Adjusted data (Loftus Donnees ajustees (Loftus & Masson, 1994) Masson, 1994)" Masson, 1994)")
        Results$"Mean and standard deviation for adjusted data"<-aj
        .multiplot(nonaj,aj, cols=2 )
      }
      
      if(any(param=="Bayes") | any(param=="Bayesian factors") ){
        if(all(param!="param") & all(param!="Parametric test")) Results$"Normality tests"<-.normalite(data=data, X=X, Y=Y)
        BF<-ttestBF(x=data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], y=data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X] , paired=TRUE, rscale=rscale)
        BF<-extractBF(BF, onlybf=F)
        BF<-data.frame("Bayesian factor"=c(round(BF$bf,5), round((1/BF$bf),5)), "Error"=round(c( BF$error, BF$error),5))
        dimnames(BF)[[1]]<-c("In favor of the alternative hypothesis", "In favor of the null hypothesis")
        Results$"Bayesian factors"<-BF
        
        samples<-ttestBF(x=data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], y=data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X] , paired=TRUE, rscale=rscale, posterior=T, iterations = ifelse(is.null(n.boot), 1000, n.boot))
        plot(samples[,1:4])
        
        
        bfs<-c()
        for (i in 5:(length(data[,X])/2)) {
          bfm <- ttestBF(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X][1:i], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X][1:i] , paired=TRUE, rscale=0.707)
          bfl <- ttestBF(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X][1:i], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X][1:i] , paired=TRUE,  rscale=1)
          bful <- ttestBF(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X][1:i], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X][1:i] , paired=TRUE,  rscale=1.41)
          bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
        }
        
        SBF<-data.frame("n"=rep(5:(length(data[,X])/2), each=3 ),"BF"= bfs, 
                        "rscale"=factor(rep(c("medium", "wide", "ultra wide"), length.out= 3*((length(data[,X])/2)-4) )))
        names(SBF)<-c("n", "BF", "rscale")
        reorder( c("medium", "wide", "ultra wide"),levels(SBF$rscale))->levels(SBF$rscale)
        Results$"Sequential Bayesian factors"<-.plotSBF(SBF)
        
        ##### Debut du graphique  Bayes Factor Robustness Check     
        
        # what is the t-value for the data?
        tVal <-  t.test(data[,X]~data[,Y], paired = TRUE, conf.level = 0.95, alternative=alternative)$statistic
        # how many points in the prior should be explored?
        nPoints <- 1000
        # what Cauchy rates should be explored?
        cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
        # what effect sizes should be plotted?
        effSize <- seq(from = -2, to = 2, length.out = 1000)
        
        # get the Bayes factor for each prior value
        bayesFactors <- sapply(cauchyRates, function(x) 
          exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = x)[['bf']]))
        
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 0.707)[['bf']])->r1
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1)[['bf']])->r2
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1.41)[['bf']])->r3
        plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
        # do the Bayes factor plot
        plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48", 
             ylim = c(0, max(bayesFactors)), xaxt = "n", 
             xlab = "Cauchy Prior Width (r)", ylab = "Bayes Factor (10)")
        abline(h = 0, lwd = 1)
        abline(h = 6, col = "black", lty = 2, lwd = 2)
        axis(1, at = seq(0, 1.5, 0.25))
        # add the BF at the default Cauchy point
        points(0.707, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
        points(1, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
        points(2^0.5, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
        # add legend
        legend(x="topright", legend = c("r = 0.707 - medium", "r = 1 - wide ", "r = 1.41 - ultrawide"),
               pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
               col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")
        
      }
      if(any(param=="non param")| any(param=="Non-parametric test")) {
        WT<-wilcox.test(as.formula(paste0(X, "~",Y)), paired=T,data=data, alternative=alternative, conf.int=T, conf.level=0.95)
        if(alternative!="two.sided")  abs(qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
        r<-z/(length(data[,X]))^0.5
        Results$Wilcoxon<- data.frame("Wilcoxon W"=WT$statistic, "p-value"=round(WT$p.value,4), "z"=round(z,4), "r"=round(r,4),
                                        "lower CI limit"=WT$conf.int[1],"sup.ci limit"=WT$conf.int[2])
      }
      
      if(any(param=="robusts"| any(param=="Robust testing - involving bootstraps")) ){
        try(WRS::yuend(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X], tr=.2),silent=T)->moy.tr
        if(class(moy.tr)!="try-error"){
          round(unlist(moy.tr),3)->moy.tr
          names(moy.tr)<-c("CI Inf","CI Sup", "p-value", "Average1", "Average2", "Difference","se", "Stat", "n", "dof") 
          if(n.boot>99){
            WRS::ydbt(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X], tr=0.2, nboot=n.boot)->moy.tr.bt
            moy.tr->Results$Robustes$"Comparison based on truncated means"
            round(unlist(moy.tr.bt),4)->Results$Robustes$"bootstrap studentise sur les truncated averages"
            if(length(data[,1])>20) {
              try({WRS::bootdpci(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X], 
                                                   nboot=n.boot, BA=T)$output[,2:6]->Mest
                names(Mest)<-c("statistical", "p-value", "p.crit", "CI inf", "CI sup")
              Mest->Results$Robustes$"BCa type bootstrap on the M-estimator"}
                , silent=T)
              }}} else Results$Robustes<-"Robust statistics could not be realized"
      }
      
      
      
      
      
      return(Results)                                                                               
    }  
    indpdts<-function(X, Y, data, param=c("param", "non param","robusts"),alternative="two.sided", n.boot=NULL, rscale=0.707){
      Results<-list()
      .e <- environment()
      Results$"descriptive statistics"<-.stat.desc.out(X=X, groupes=Y, data=data, tr=.1, type=3, plot=T)
      as.formula(paste0(X," ~ ",Y))->model
      if(any(param=="param") | any(param=="Parametric test")){
        Results$"Normality tests"<-.normalite(data=data, X=X, Y=Y)
        car::leveneTest(data[ ,X], data[ ,Y])->Levene # test de Levene pour homogeneite des variances
        round(unlist(Levene)[c(1,2,3,5)],3)->Levene
        names(Levene)<-c("dof1","dof2","F","p-value")
        Levene->Results$"Levene test verifying the homogeneity of variances"
        t.test(model, data=data, alternative=alternative,  var.equal=TRUE, conf.level=0.95)->student
        round(student$statistic^2/(student$statistic^2+student$parameter),3)->R.two
        d_cohen<-round(cohensD(model , data=data, method = "pooled"),3)
        data.frame(student[9], round(student$statistic,3), student$parameter, round(student$p.value,3), round(student$conf.int[1],4),
                   round(student$conf.int[2],4),  R.two, d_cohen)->student
        t.test(model, data=data, alternative=alternative,  var.equal=FALSE, conf.level=0.95)->corrige
        corrige$statistic^2/(corrige$statistic^2+corrige$parameter)->R.two.corr
        d_cohen.corr<-cohensD(model , data=data, method = "unequal")
        data.frame(corrige[9], round(corrige$statistic,3), round(corrige$parameter,3), round(corrige$p.value,3), round(corrige$conf.int[1],4),
                   round(corrige$conf.int[2],4),  R.two, d_cohen)->corrige
        names(student)<-c("model", "test t", "dof", "p-value", "lower CI limit", "sup.ci limit","R.carre","d de Cohen")
        names(corrige)<- c("model", "test t", "dof", "p-value", "lower CI limit", "sup.ci limit","R.carre","d de Cohen")
        student<-rbind(student, corrige)
        dimnames(student)[[1]]<-c("without Welch correction","with Welch correction")
        student->Results$"t of student for independent samples"
        p<-ggplot(data)
        p<-p+eval(parse(text=paste0("aes(x=", Y, ", y=", X,")")))
        p<-p+  stat_summary(fun.y=mean, geom="bar",fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
        Results$"Graphical Representation - Mean and Standard Deviation"<-p
        
      }
      if(any(param=="Bayes") | any(param=="Bayesian factors") ){
        if(all(param!="param") & all(param!="Parametric test")) Results$"Normality tests"<-.normalite(data=data, X=X, Y=Y)
        BF<-ttestBF(formula=model,data=data, paired=FALSE, rscale=rscale)
        BF<-extractBF(BF, onlybf=F)
        BF<-data.frame("Bayesian factor"=c(round(BF$bf,5), round((1/BF$bf),5)), "Error"=round(c( BF$error, BF$error),5))
        dimnames(BF)[[1]]<-c("In favor of the alternative hypothesis", "In favor of the null hypothesis")
        Results$"Bayesian factors"<-BF
        
        samples<-ttestBF(formula=model,data=data, paired=FALSE, rscale=rscale, posterior=T, iterations = ifelse(is.null(n.boot), 1000, n.boot))
        plot(samples[,1:4])
        
        
        bfs<-c()
        tab<-table(data[,Y])
        data1<-data.frame(X=c(data[which(data[,Y]==levels(data[,Y])[1] ),X], data[which(data[,Y]==levels(data[,Y])[2] ),X]), id=c(1:tab[1],1:tab[2]), 
                          Y=c(rep(levels(data[,Y])[1], tab[1]), rep(levels(data[,Y])[2], tab[2])))
        data1<-data1[order(data1$id),]
        for (i in 5:length(data[,X])) {
          bfm <- ttestBF(formula=X~Y,data=data1[1:i,], paired=FALSE, rscale=0.707)
          bfl <- ttestBF(formula=X~Y,data=data1[1:i,] , paired=FALSE,  rscale=1)
          bful <- ttestBF(formula=X~Y,data=data1[1:i,] , paired=FALSE,  rscale=1.41)
          bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
        }
        
        SBF<-data.frame("n"=rep(5:(length(data[,X])), each=3 ),"BF"= bfs, 
                        "rscale"=factor(rep(c("medium", "wide", "ultra wide"), length.out= 3*(length(data[,X])-4) )))
        names(SBF)<-c("n", "BF", "rscale")
        reorder( c("medium", "wide", "ultra wide"),levels(SBF$rscale))->levels(SBF$rscale)
        Results$"Sequential Bayesian factors"<-.plotSBF(SBF)
        
        ##### Debut du graphique  Bayes Factor Robustness Check     
        
        # what is the t-value for the data?
        tVal <-  t.test(formula=model, data=data, paired = FALSE, conf.level = 0.95, alternative=alternative)$statistic
        # how many points in the prior should be explored?
        nPoints <- 1000
        # what Cauchy rates should be explored?
        cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
        # what effect sizes should be plotted?
        effSize <- seq(from = -2, to = 2, length.out = 1000)
        
        # get the Bayes factor for each prior value
        
        bayesFactors <- sapply(cauchyRates, function(x) 
          exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = x)[['bf']]))
        
        exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = 0.707)[['bf']])->r1
        exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = 1)[['bf']])->r2
        exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = 1.41)[['bf']])->r3
        plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
        # do the Bayes factor plot
        plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48", 
             ylim = c(0, max(bayesFactors)), xaxt = "n", 
             xlab = "Cauchy Prior Width (r)", ylab = "Bayes Factor (10)")
        abline(h = 0, lwd = 1)
        abline(h = 6, col = "black", lty = 2, lwd = 2)
        axis(1, at = seq(0, 1.5, 0.25))
        # add the BF at the default Cauchy point
        points(0.707, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
        points(1, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
        points(2^0.5, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
        # add legend
        legend(x="topright", legend = c("r = 0.707 - medium", "r = 1 - wide ", "r = 1.41 - ultrawide"),
               pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
               col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")
        
      }
      if(any(param=="non param")| any(param=="Non-parametric test")) {
        WT<-wilcox.test(model, paired=F,data=data, alternative=alternative, conf.int=T, conf.level=0.95)
        if(alternative!="two.sided")  abs(qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
        r<-z/(length(data[,X]))^0.5
        Results$"Mann-Whitney test - Wilcoxon"<- data.frame("Wilcoxon W"=WT$statistic, "p-value"=round(WT$p.value,4), "z"=round(z,4), "r"=round(r,4),
                                                                 "lower CI limit"=WT$conf.int[1],"sup.ci limit"=WT$conf.int[2])
      }
      
      if(any(param=="robusts"| any(param=="Robust testing - involving bootstraps")) ){
        data[which(data[,Y]==levels(data[,Y])[1]),]->g1 # on cree une base de Donnees avec le groupe 1 uniquement (sans value aberrantes)
        data[which(data[,Y]==levels(data[,Y])[2]),]->g2 # on cree une base de Donnees avec le groupe 2 uniquement (sans value aberrantes)
        try(WRS::yuen(g1[,X],g2[,X]), silent=T)->yuen.model### fournit la probabilite associee a des truncated averages.Par defaut, la troncature est de 0.20
        if(class(yuen.model)!="try-error"){
          round(unlist(yuen.model),4)->yuen.model
          cbind(yuen.model[1:2], yuen.model[3:4])->yuen.desc
          dimnames(yuen.desc)[[1]]<-levels(data[,Y])
          dimnames(yuen.desc)[[2]]<-c("n", "truncated averages")
          yuen.desc->Results$Robustes$"descriptive statistics"
          
          yuen.model[c(5,6,8,9,10,11,12,7)]->yuen.model
          names(yuen.model)<-c("lower CI limit", "sup.ci limit", 
                                "Difference","Err-type","Stat", "Threshold", "dof","p-value")
          yuen.model->Results$Robustes$"Analysis on truncated means"
          if(n.boot>99){
            WRS::yuenbt(g1[,X],g2[,X], nboot=n.boot, side=T)->yuen.bt.model ### fournit la probabilite associee a des truncated averages apres un bootstrap.
            round(unlist(yuen.bt.model)[1:4],4)->yuen.bt.model
            names(yuen.bt.model)<-c("lower CI limit", "sup.ci limit", "Stat", "p-value")
            yuen.bt.model->Results$Robustes$"Bootstrap using the t method on truncated means"
            WRS::pb2gen(g1[,X],g2[,X], nboot=n.boot)->pb2gen.model### calcule le bootstrap sur le M-estimator et fournit l intervalle de confiance. 
            round(unlist(pb2gen.model)[1:6],4)->pb2gen.model
            names(pb2gen.model)<-c("M.estimaror.G1", "M.estimator.G2", "diff", "lower CI limit", "sup.ci limit", "p-value")
            pb2gen.model->Results$Robustes$"Bootstrap percentile on M-estimator"
            Results$Robustes$Informations<-c("the bootstrap percentile method should be preferred for small samples",
                                               "For wider samples, boostrap using the t method should be preferred.") 
          }
          
          WRS::ks(g1[,X],g2[,X],w=F,sig=T)->KS
          round(unlist(KS),4)->KS
          names(KS)<-c("KS", "Critical threshold","p-value")
          KS->Results$Robustes$"Kolmogorov-Smirnov test comparing two distributions"
        }else Results$"Robust statistics"<-"Robust statistics could not be realized. Check the installation of the WRS package"
        
        
      }
      
      return(Results)
    }
    data_summary <- function(x) {
      m <- mean(x)
      ymin <- m-sd(x)
      ymax <- m+sd(x)
      return(c(y=m,ymin=ymin,ymax=ymax))
    }
    #### 5 fonctions qui seront appelees pour realiser l'analyse
    options (warn=-1) 
    # chargement des packages
    packages<-c("BayesFactor", "svDialogs", "outliers", "nortest","psych", "lsr","ggplot2", "reshape2", "car", "plyr")
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    try(library("WRS"),silent=T)
    .e <- environment()
    Results<-list()
    try( windows(record=T), silent=T)->win
    if(class(win)=="try-error") quartz()
    if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data 
    test.t.options<-test.t.in(X=X, Y=Y, data=data, choice=choice, param=param, outlier=outlier, sauvegarde=sauvegarde, info=info, group=group,alternative=alternative, 
                              formula=formula,n.boot=n.boot, rscale=rscale, mu=mu) 
    if(is.null(test.t.options)) return(analyse())
    
    choice<-test.t.options$choice
    X<-test.t.options$X
    Y<-test.t.options$Y
    mu<-test.t.options$mu
    group<-test.t.options$group
    data<-test.t.options$data
    alternative<-test.t.options$alternative
    group<-test.t.options$group
    param<-test.t.options$options$choice
    rscale<-test.t.options$options$rscale
    n.boot<-test.t.options$options$n.boot
    sauvegarde<-test.t.options$options$sauvegarde
    outlier<-test.t.options$options$desires
    
    for(i in 1 : length(X)) {
      
      
      if(choice=="Two matched samples"){
        diffs<-data[which(is.na(data[,X])), "IDeasy"]
        if(length(diffs)==0) data->data1 else data[which(data$IDeasy!=diffs), ]->data1 
      } else  {
        data1<-data[complete.cases(data[,c(Y,X[i])]),]
      }
      
      
      
      
      X1<-X[i]
      R1<-list()
      if(any(outlier==  "Complete data")){
        switch(choice,  "Comparison to a standard"=  R1$"Complete data"<-norme(X=X1, mu=mu, data=data1, param=param, group=group, alternative=alternative, n.boot=n.boot, rscale=rscale), 
               "Two matched samples"=R1$"Complete data"<-apparies(X=X1, Y=Y, data=data1, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale),
               "Two independent samples"= R1$"Complete data"<-indpdts(X=X1, Y=Y, data=data1, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale))
      }

      if(any(outlier=="Identification of influential values")|any(outlier=="Data without influencing value")){
        if(choice=="Comparison to a standard") {
          if(class(data1)!="data.frame"){data1<-data.frame(data1)
                                       names(data1)[1]<-X1}
          data1$residue<-data1[,X1]
                                             }else data1$residue<-unlist(tapply(data1[,X1], data1[,Y], scale, center=T, scale=F))
        critere<-ifelse(is.null(z), "Grubbs", "z")
        values.influentes(X="residuee", critere=critere,z=z, data=data1)->influentes
      }
      if(any(outlier== "Identification of influential values")){influentes->R1$"Influential values"}
      if(any(outlier== "Data without influencing value")) {
        if(length(influentes$"influential observations")!=0 | all(outlier!="Complete data")){
          
          if(choice=="Two matched samples"){
            setdiff(data$IDeasy,influentes$"influential observations"$IDeasy)->diffs
            data[which(data$IDeasy%in%diffs), ]->cleaned
          } else  get("cleaned", envir=.GlobalEnv)->cleaned
          
          ### Regler le souci pour les echantillons apparies
          switch(choice,  "Comparison to a standard"=  R1$"Data without influencing value"<-norme(X=X1, mu=mu, data=cleaned, param=param, group=group, alternative=alternative, n.boot=n.boot, rscale=rscale), 
                 "Two matched samples"=R1$"Data without influencing value"<-apparies(X=X1, Y=Y, data=cleaned, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale),
                 "Two independent samples"= R1$"Data without influencing value"<-indpdts(X=X1, Y=Y, data=cleaned, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale))
        }
      }
      Results[[i]]<-R1
    }
    
    names(Results)<-paste("Analysis on the variable", X)
    
    paste(unique(X), collapse="','", sep="")->X
    paste(outlier,  collapse="','", sep="")->outlier
    paste(param,  collapse="','", sep="")->param
    Results$Call<-paste0("test.t(X=c('", X,
                           "'), Y=", ifelse(!is.null(Y),paste0("'",Y,"'"), "NULL"), 
                           ",group=", ifelse(!is.null(group),paste0("'",group,"'"), "NULL"), 
                           ", choice='", choice, 
                           "', sauvegarde = ", sauvegarde, ",outlier=c('", outlier, "'),z=", ifelse(!is.null(z),z, "NULL"),
                           ", data=", test.t.options$nom, ",alternative='", alternative, "', mu=", ifelse(!is.null(mu),mu, "NULL"),
                           ",formula =NULL, n.boot=", ifelse(is.null(n.boot), "NULL", n.boot), ",param=c('", param, "'),info=T, rscale=", rscale, ")"
    )
    .add.history(data=data, command=Results$Call, nom=test.t.options$nom)
    .add.result(Results=Results, name =paste(choice, Sys.time() ))
    
    if(sauvegarde){save(Results=Results ,choice =choice, env=.e)}
    
    ref1(packages)->Results$"References"
    if(html) ez.html(Results)
    ### Obtenir les Results
    return(Results) 
    
    }
