corr.complet <-
  function(X=NULL, Y=NULL, Z=NULL,data=NULL,  group=NULL, param=c("test parametrique", "non-parametric test","Robust testing - involving bootstraps", "Facteurs bayesiens"), 
           save=F, outlier=c("complete", "id", "removed"),  z=NULL, info=T, n.boot=NULL, rscale=0.353, html=T){options (warn=-1) 
    
    
    corr.complet.in<-function(X=NULL, Y=NULL,Z=NULL, data=NULL, group=NULL, param=NULL, outlier=NULL, save=NULL, info=T,n.boot=NULL, rscale=0.707){
      
      Resultats<-list()
      if(!is.null(X) & !is.null(data) & !is.null(Y)) {dial<-F 
      if(is.null(Z)) choix<-"Correlations" else choix<-"Partial and semi-partial correlations"
      }  else {dial<-T
      choix<-NULL}
      
      if(is.null(choix) ){
        if(info) writeLines("Please specify the type of correlation you wish to achieve.")
        choix<-dlgList(c("Correlations", "Partial and semi-partial correlations"), preselect="Correlations", multiple = FALSE, title="Simple or partial correlations?")$res
        if(length(choix)==0) return(NULL)
      }
      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL)
      nom<-data[[1]]
      data<-data[[2]]
      
      
      msg3<-"Please choose the variable in abscissa"
      msg4<-"Please choose the variable in ordinate"
      
      X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title="Variable-s in abscissa", out=NULL)
      if(is.null(X)) {
        corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.707)->Resultats
        return(Resultats)}
      data<-X$data
      X1<-X$X
      
      Y<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg4,  multiple=T, title="Variable-s in ordinate", out=X1)
      if(is.null(Y)) {
        corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.707)->Resultats
        return(Resultats)}
      data<-Y$data
      Y<-Y$X 
      if(choix=="Partial and semi-partial correlations"){
        msg6<-"Please specify the variable (s) to control" 
        Z<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg6,  multiple=T, title="Variable-s to control", out=c(X1,Y))
        if(is.null(Z)) {
          corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                          n.boot=NULL, rscale=0.707)->Resultats
          return(Resultats)}
        data<-Z$data
        Z<-Z$X 
      }
      
      
      if(dial){
        
        if(info==TRUE) writeLines("Si vous souhaitez realiser l'analyse pour differents sous-echantillons en fonction d'un critere categoriel (i.e; realiser une analyse par groupe)
                                  \n choisissez oui. Dans ce cas, l'analyse est realisee sur l'echantillon complet et sur les sous-echantillons.
                                  \n Si vous desirez l'analyse pour l'echantillon complet uniquement, chosissez non.
                                  \n l'analyse par groupe ne s'appliquent pas aux statistiques robustes.")
        dlgList(c("oui", "non"), preselect="non", multiple = FALSE, title="Analyse par groupe?")$res->par.groupe
        if(length(par.groupe)==0) {
          corr.complet.in(X=NULL, Y=NULL, data=NULL,param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                          n.boot=NULL, rscale=0.707)->Resultats
          return(Resultats)
        } 
        msg5<-"Please choose the categorical ranking factor."
        if(par.groupe=="oui"){group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=F, message=msg5,  multiple=TRUE, title="Variable-s", out=c(X1,Y,Z)) 
        if(length(group)==0) {  corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                                                n.boot=NULL, rscale=0.707)->Resultats
          return(Resultats)}
        data<-group$data
        group<-group$X 
        if(any(ftable(data[,group])<3)){
          msgBox("Some combinations of modalities have less than 3 observations. You must have at least 3 observations for each combination")
          corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                          n.boot=NULL, rscale=0.707)->Resultats
          return(Resultats)
        }
        }
      }
      
      msg.options1<-"The parametric test is the Bravais-Pearson correlation"
      msg.options2<- "The nonparametric test corresponds to Spearman's rho and Kendall's tau"
      
      options<-.ez.options(options=c("choix","outlier"), n.boot=n.boot,param=T, non.param=T, robust=T, Bayes=T, msg.options1=msg.options1, msg.options2=msg.options2, info=info, dial=dial, 
                           choix=param,sauvegarde=save, outlier=outlier, rscale=rscale)
      if(is.null(options)){
        corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.707)->Resultats
        return(Resultats)
      }
      Resultats$choix<-choix
      Resultats$nom<- nom
      Resultats$data<-data
      Resultats$X<-X1
      Resultats$Y<-Y
      if(exists("Z")) Resultats$Z<-Z
      if(exists("group")) Resultats$group<-group
      Resultats$options<-options
      return(Resultats)
    }
    corr.complet.out<-function(X=NULL, Y=NULL, Z=NULL, data=NULL, choix=NULL, group=NULL, param=NULL,n.boot=NULL, rscale=0.353) {
      boot_BP<-function(data,i)cor(data[ , X1][i], data[ , Y1][i], use="complete.obs", method="pearson")
      boot_Spearman<-function(data,i)cor(data[ ,X1][i], data[ , Y1][i], use="complete.obs", method="spearman")
      boot_BPSP<-function(data,i)cor(data[ , X][i], data[ , Y1][i], use="complete.obs", method="pearson")
      boot_SpearmanSP<-function(data,i)cor(data[ ,X][i], data[ , Y1][i], use="complete.obs", method="spearman")
      list()->Resultats
      Resultats$"descriptive statistics"<-.stat.desc.out(X=c(X,Y,Z), groupes=NULL, data=data, tr=.1, type=3, plot=T)
      if(!is.null(group)) {Resultats$"descriptive statistics by group"<-.stat.desc.out(X=c(X,Y,Z), groupes=group, data=data, tr=.1, type=3, plot=T) }
      
      
      if(choix== "Correlations") {
        title<-"Bravais-Pearson correlation"
        title2<-"Rho de Spearman"
        X1<-X
        Y1<-Y} else {
          title<-"Partial Bravais-Pearson correlation"
          title2<-"Spearman Partial Rho"
          modele1<-as.formula(paste0(X,"~",Z[1]))
          modele2<-as.formula(paste0(Y,"~", Z[1]))
          if(length(Z)>1) for(i in 2:length(Z)){
            modele1<-update(modele1, as.formula(paste0(".~.+",Z[i])))
            modele2<-update(modele2, as.formula(paste0(".~.+",Z[i])))
          }
          lm.r1<-lm(modele1, data)
          lm.r2<-lm(modele2, data)
          data$residus1<-lm.r1$residuals
          data$residus2<-lm.r2$residuals
          X1<-"residus1"
          Y1<-"residus2"
        }
      modele<-as.formula(paste0(X1,"~",Y1))
      lm.r<-lm(modele,na.action=na.exclude,data=data)
      resid(lm.r)->data$residus # recuperation du residu sur le modele lineaire
      
      if(any(param=="Bayes") | any(param=="Facteurs bayesiens") | any(param=="param") | any(param=="Test parametrique"))  {
        Resultats$"Tests de normalite"<-.normalite(data=data, X="residus", Y=NULL)
        graphiques<-list()
        p<-ggplot(data)
        p<-p+ eval(parse(text=paste0("aes(x=", X,", y=", Y,")"))) + geom_point() 
        p<-p+ geom_smooth(method=lm)
        p<-p+theme(plot.title = element_text(size = 12))+ggtitle(title)
        p<-p+theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))
        graphiques[[1]]<-p
        #print(p) 
        if(!is.null(group)){
          p1<-p+eval(parse(text=paste0("aes(color=",group[1] ,")")))
          if(length(group)>1) {p1<-p1+eval(parse(text=paste0("aes(shape=",group[2],")"))) } 
          if(length(group)>2){
            for(i in 3:length(group)){
              if(i==3) paste0(".~", group[3])->panneau
              if(i==4) paste0(group[4],"~", group[3])->panneau
              if(i>3 & i%%2!=0) paste0(panneau, "+", group[i])->panneau
              if(i>4 & i%%2==0) paste0(group[i], "+", panneau)->panneau
            }
            p1<-p1+ facet_grid(as.formula(panneau), labeller=label_both)     
          }
          graphiques[[2]]<-p1
        }
        Resultats$"Test parametrique"$Graphiques<-graphiques
      }
      if(any(param=="param") | any(param=="Test parametrique")){
        
        if(choix!="Correlations") {
          cor.part<-rbind( pcor.test(data[,X], data[ ,Y], data[ , Z], method = "pearson")[1:3],
                           spcor.test(data[,X], data[ ,Y], data[ ,Z], method = "pearson")[1:3])
          cor.part$estimate^2->cor.part$r.carre
          round(cor.part, 4)->cor.part
          cor.part$ddl<-(pcor.test(data[,X], data[ ,Y], data[ , Z], method = "pearson")$n-2-length(Z))
          dimnames(cor.part)<-list(c("Bravais Pearson partial correlation","Bravais Pearson semi-partial correlation"), c("Correlation", "valeur.p", "test.t", "r.carre","ddl"))
          Resultats$"Correlation partielle/semi-partielle de Bravais Pearson"<-cor.part
          
        } else {
          BP<-cor.test(data[, X1], data[ ,Y1], method = "pearson")
          Resultats$"Test parametrique"$"Bravais Pearson correlation"<-round(data.frame("r"=BP$estimate,"r.deux"=BP$estimate^2, "IC lim inf"=BP$conf.int[1],"IC lim sup"=BP$conf.int[2], "t"=BP$statistic, 
                                                                                           "ddl"=BP$parameter, "valeur.p"=BP$p.value),4)
        } 
        
        
        if(!is.null(group)){  
          if(choix=="Correlations") {
            corr.g<-function(X2){ 
              return(data.frame(BP.r= cor.test(X2[, X1], X2[ ,Y1], method = "pearson")$estimate,
                                BP.ddl= cor.test(X2[, X1], X2[ ,Y1], method = "pearson")$parameter,
                                BP.t= cor.test(X2[, X1], X2[ ,Y1], method = "pearson")$statistic,
                                BP.p= cor.test(X2[, X1], X2[ ,Y1], method = "pearson")$p.value))}} else {
                                  corr.g<-function(X2){ return(data.frame(BP.r= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "pearson")$estimate,
                                                                          BP.ddl= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z])$n-2-length(Z),
                                                                          BP.t= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "pearson")$statistic,
                                                                          BP.p= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "pearson")$p.value))}   
                                }
          
          BPgroup<-by(data=data, INDICES=data[,group], FUN=corr.g)
          BPgroup<-round(matrix(unlist(BPgroup), ncol=4, byrow=T), 4) 
          if(length(group)==1) {gr.l<-expand.grid(levels(data[,group])) 
          names(gr.l)<-group}else { gr.l<-sapply(data[,group],levels)
          gr.l<-data.frame(gr.l)
          gr.l<-expand.grid(gr.l)
          } 
          
          
          dimnames(BPgroup)[[2]]<- c("BP.r", "BP.ddl", "BP.t", "BP.p")
          BPgroup<-data.frame(gr.l,BPgroup )
          if(choix!="Correlations") Resultats$"Test parametrique"$"Partial Bravais-Pearson correlation by group"<-BPgroup else Resultats$"Bravais-Pearson correlation by group"<-BPgroup
          
        }
      }
      if(any(param=="non param")| any(param=="Non-parametric test")){
        
        
        graphiques<-list()
        p<-ggplot(data)
        p<-p+ eval(parse(text=paste0("aes(x=rank(data$", X,"), y=rank(data$", Y,"))"))) + geom_point() 
        p<-p+ labs(x =X, y=Y)
        p<-p+ geom_smooth(method=lm)
        p<-p+theme(plot.title = element_text(size = 12))+ggtitle(title2)
        p<-p+theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))
        graphiques[[1]]<-p
        #print(p) 
        if(!is.null(group)){
          p1<-p+eval(parse(text=paste0("aes(color=",group[1] ,")")))
          if(length(group)>1) {p1<-p1+eval(parse(text=paste0("aes(shape=",group[2],")"))) } 
          if(length(group)>2){
            for(i in 3:length(group)){
              if(i==3) paste0(".~", group[3])->panneau
              if(i==4) paste0(group[4],"~", group[3])->panneau
              if(i>3 & i%%2!=0) paste0(panneau, "+", group[i])->panneau
              if(i>4 & i%%2==0) paste0(group[i], "+", panneau)->panneau
            }
            p1<-p1+ facet_grid(as.formula(panneau), labeller=label_both)     
          }
          graphiques[[2]]<-p1
        }
        Resultats$"Non-parametric test"$Graphiques<-graphiques
        
        if(choix!="Correlations") {
          spear<-rbind( pcor.test(data[,X], data[ ,Y], data[ , Z], method = "spearman")[1:3],spcor.test(data[,X], data[ ,Y], data[ ,Z], method = "spearman")[1:3])
          tau<-rbind(pcor.test(data[,X], data[ ,Y], data[ , Z], method = "kendall")[1:3],spcor.test(data[,X], data[ ,Y], data[ , Z], method = "kendall")[1:3])       
          spear<-round(spear,4)
          tau<-round(tau,4)
          spear$estimate^2->spear$r.carre
          round(spear, 4)->cor.part
          dimnames(spear)<-list(c("Spearman Partial Rho","Spearman's semi-partial rho"), c("rho", "valeur.p", "t", "r.carre"))
          Resultats$"Non-parametric test"$"Rho partiel/semi partiel de Spearman"<-spear
          tau<-round(tau,4)
          dimnames(tau)<-list(c("Kendall's partial tau","Kendall's semi-partial tau"), c("tau", "valeur.p", "z"))
          Resultats$"Non-parametric test"$"Tau partiel/semi-partiel de Kendall"<-tau
        } else { Spear<-cor.test(data[,X1], data[ ,Y1], method = "spearman", exact=T, continuity=T)
        cor.test(data[,X1], data[ ,Y1], method = "kendall")->Kendall 
        Resultats$"Non-parametric test"$"Rho de Spearman"<-round(data.frame("rho"=Spear$estimate,"rho.deux"=Spear$estimate^2,"S"=Spear$statistic,"valeur.p"=Spear$p.value),4)
        round(data.frame("tau"=Kendall$estimate,"z"=Kendall$statistic,"valeur.p"=Kendall$p.value),4)->Resultats$"Non-parametric test"$"Tau de Kendall"}
        
        
        if(!is.null(group)){
          if(choix=="Correlations") {corr.g<-function(X2){ return(data.frame(Sp.r= cor.test(X2[, X1], X2[ ,Y1], method = "spearman")$estimate,
                                                                             Sp.p= cor.test(X2[, X1], X2[ ,Y1], method = "spearman")$p.value,
                                                                             Kendall.r= cor.test(X2[, X1], X2[ ,Y1], method = "kendall")$estimate,
                                                                             Kendall.p= cor.test(X2[, X1], X2[ ,Y1], method = "kendall")$p.value))}
          } else {
            corr.g<-function(X2){ return(data.frame(Spearman.r= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "spearman")$estimate,
                                                    Spearman.ddl= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z],method="spearman")$n-2-length(Z),
                                                    Spearman.t= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "spearman")$estimate,
                                                    Spearman.p= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "spearman")$p.value))
            }}   
          
          BPgroup<-by(data=data, INDICES=data[,group], FUN=corr.g)
          BPgroup<-round(matrix(unlist(BPgroup), ncol=4, byrow=T),4)
          if(length(group)==1) {gr.l<-expand.grid(levels(data[,group]))
          names(gr.l)<-group}else{ gr.l<-sapply(data[,group],levels)
          gr.l<-data.frame(gr.l)
          gr.l<-expand.grid(gr.l)
          } 
          if(choix!="Correlations"){
            dimnames(BPgroup)[[2]]<- c("Spearman.rho", "Spearman.ddl", "Spearman.t", "Spearman.p")
            BPgroup<-data.frame(gr.l,BPgroup )
            Resultats$"Non-parametric test"$"Partial Spearman correlation by group"<-BPgroup 
          } else {dimnames(BPgroup)[[2]]<- c( "Spearman.r", "Spearman.p", "Tau.Kendall.r", "Tau.Kendall.p")
          BPgroup<-data.frame(gr.l,BPgroup )
          Resultats$"Non-parametric test"$"Correlation de Spearman/Kendall par groupe"<-BPgroup}
        }
      }
      
      if(any(param=="robust"| any(param=="Robust testing - involving bootstraps"))){
        boot_BP_results<-boot(data, boot_BP, n.boot)
        if(!is.null(Resultats$"Test parametrique"$"Bravais Pearson correlation")) {
          try(Resultats$"Test parametrique"$"Bravais Pearson correlation"$"Bca lim inf"<-round( boot.ci(boot_BP_results)$bca[,4],4), silent=T)
          try(Resultats$"Test parametrique"$"Bravais Pearson correlation"$"Bca lim sup"<-round( boot.ci(boot_BP_results)$bca[,5],4),silent=T)
        } else if(!is.null(Resultats$"Test parametrique"$"Correlation partielle/semi-partielle de Bravais Pearson")) {
          boot_BPSP_results<-boot(data, boot_BPSP, n.boot)  
          try(Resultats$"Test parametrique"$"Correlation partielle/semi-partielle de Bravais Pearson"$"Bca lim inf"<-round( c(boot.ci(boot_BP_results)$bca[,4], boot.ci(boot_BPSP_results)$bca[,4]),4),silent=T)
          try(Resultats$"Test parametrique"$"Correlation partielle/semi-partielle de Bravais Pearson"$"Bca lim sup"<-round( c(boot.ci(boot_BP_results)$bca[,5], boot.ci(boot_BPSP_results)$bca[,5]) ,4), silent=T)
        } else try(Resultats$"Analyses robustes"$"Bravais Pearson correlation bootstrap"<-round(data.frame("Bca.lim.inf"= boot.ci(boot_BP_results)$bca[,4], " Bca.lim.sup"=boot.ci(boot_BP_results)$bca[,5] ), 4),silent=T)
        
        if(any(param=="non param")| any(param=="Non-parametric test")) {
          boot_Spearman_results<-boot(data, boot_Spearman, n.boot)
          if(!is.null(Resultats$"Non-parametric test"$"Rho de Spearman")) {
            try(Resultats$"Non-parametric test"$"Rho de Spearman"$"Bca lim inf"<-round( boot.ci(boot_Spearman_results)$bca[,4],4), silent=T)
            try(Resultats$"Non-parametric test"$"Rho de Spearman"$"Bca lim sup"<-round( boot.ci(boot_Spearman_results)$bca[,5],4), silent=T)
          } else{
            boot_SpearmanSP_results<-boot(data, boot_SpearmanSP, n.boot)
            
            try(Resultats$"Non-parametric test"$"Rho partiel/semi partiel de Spearman"$"Bca lim inf"<-round(c( boot.ci(boot_Spearman_results)$bca[,4], boot.ci(boot_SpearmanSP_results)$bca[,4]),4), silent=T)
            try(Resultats$"Non-parametric test"$"Rho partiel/semi partiel de Spearman"$"Bca lim sup"<-round(c( boot.ci(boot_Spearman_results)$bca[,5], boot.ci(boot_SpearmanSP_results)$bca[,5]),4), silent=T)
          } 
          
        }
      }
      
      
      if(any(param=="Bayes") | any(param=="Facteurs bayesiens") ){
        
        BF<-regressionBF(modele, data=data, rscaleCont=rscale )
        sample<-posterior(BF, iterations = ifelse(is.null(n.boot), 1000, n.boot))
        BF<-extractBF(BF, onlybf=F)
        BF<-data.frame("Facteur bayesien"=c(ifelse(BF$bf>10000,">10000", round(BF$bf,5)), 
                                            ifelse(1/BF$bf>10000, ">10000", round((1/BF$bf),5))), "Erreur"=round(c( BF$error, BF$error),5))
        
        dimnames(BF)[[1]]<-c("In favor of the alternative hypothesis", "In favor of the null hypothesis")
        # what is the t-value for the data?
        r2Val <-cor.test(data[,X1],data[,Y1])$estimate
        BF$r<-r2Val
        r2Val<-r2Val^2
        BF$r.carre<-r2Val
        Resultats$"Facteurs bayesiens"$"Bayesian factors for the Bravais-Pearson correlation"<-BF
        
        if(any(param=="non param")| any(param=="Non-parametric test")) {
          data2<-sapply(data[,c(X,Y,Z)], rank, ties.method="average", na.last="keep")
          data2<-data.frame(data2)
          if(choix!="Correlations"){
            lm.r1<-lm(modele1, data2)
            lm.r2<-lm(modele2, data2)
            data2$residus1<-lm.r1$residuals
            data2$residus2<-lm.r2$residuals
          }
          
          BFS<-regressionBF(modele, data=data2, rscaleCont=rscale )
          BFS<-extractBF(BFS, onlybf=F)
          BFS<-data.frame("Facteur bayesien"=c(ifelse(BFS$bf>10000,">10000", round(BFS$bf,5)), 
                                               ifelse(1/BFS$bf>10000, ">10000", round((1/BFS$bf),5))), "Erreur"=round(c( BFS$error, BF$error),5))
          dimnames(BFS)[[1]]<-c("In favor of the alternative hypothesis", "In favor of the null hypothesis")
          Resultats$"Facteurs bayesiens"$"Bayesian factors for the Spearman correlation"<-BFS
          
        }
        
        if(!is.null(group)){
          
          corr.g<-function(X2){  BF<-regressionBF(modele, X2, rscaleCont=rscale ,progress=F)
          BF<-extractBF(BF, onlybf=F)
          return(data.frame("Facteur bayesien"=round(BF$bf,5), "Erreur"=round(BF$error,5)))}
          
          BPgroup<-by(data=data, INDICES=data[,group], FUN=corr.g)
          BPgroup<-round(matrix(unlist(BPgroup), ncol=2, byrow=T), 4) 
          dimnames(BPgroup)[[2]]<- c("FB", "erreur")
          if(length(group)==1) {gr.l<-expand.grid(levels(data[,group])) 
          names(gr.l)<-group}else gr.l<-expand.grid(sapply(data[,group],levels))
          BPgroup<-data.frame(gr.l,BPgroup )
          
          
          
          if( any(param=="non param")| any(param=="Non-parametric test")){
            BFgroupS<-by(data=data2, INDICES=data[,group], FUN=corr.g)
            BFgroupS<-matrix(unlist(BFgroupS), ncol=2, byrow=T)
            BPgroup<-cbind(BPgroup, BFgroupS)
            names(BPgroup)<-c(group, "FB.BP","Erreur.BP", "FB.Spearman", "Erreur.Spearman")
          }  
          BPgroup->Resultats$"Facteurs bayesiens"$"Bayesian factor by group"
        }
        
        plot(sample)
        bfs<-c()
        for (i in 5:length(data[,X1])) {
          bfm <- regressionBF(modele, data=data[1:i,],progress=F, rscaleCont=0.353)
          bfl <- regressionBF(modele, data=data[1:i,], progress=F, rscaleCont=0.5)
          bful <- regressionBF(modele,data=data[1:i,], progress=F, rscaleCont=0.707)
          bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
        }
        
        SBF<-data.frame("n"=rep(5:length(data[,X]), each=3 ),"BF"= bfs, 
                        "rscale"=as.factor(rep(c("moyen - 0.353", "large - 0.5", "ultra large - 0.707"), length.out= 3*(length(data[,X])-4) )))
        SBF$rscale<-relevel(SBF$rscale, ref=2)
        Resultats$"Sequential Bayesian factors"<-.plotSBF(SBF)
        
        ##### Debut du graphique  Bayes Factor Robustness Check     
        
        
        # how many points in the prior should be explored?
        nPoints <- 1000
        # what Cauchy rates should be explored?
        cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
        # what effect sizes should be plotted?
        effSize <- seq(from = -2, to = 2, length.out = 1000)
        
        # get the Bayes factor for each prior value
        bayesFactors <- sapply(cauchyRates, function(x) exp(linearReg.R2stat(N=length(data[,1]), p=1, R2=r2Val, rscale = x, simple = T)))
        
        exp(linearReg.R2stat(N=length(data[,1]), p=1, R2=r2Val, rscale = 0.353, simple = T))->r1
        exp(linearReg.R2stat(N=length(data[,1]), p=1, R2=r2Val, rscale = 0.5, simple = T))->r2
        exp(linearReg.R2stat(N=length(data[,1]), p=1, R2=r2Val, rscale = 0.707, simple = T))->r3
        plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
        # do the Bayes factor plot
        if(max(bayesFactors)>10^40) bayesFactors[which(bayesFactors>10^40)]<-10^40
        if(r1>10^40) r1<-10^40
        if(r2>10^40) r2<-10^40
        if(r3>10^40) r3<-10^40
        seq(min(bayesFactors),  max(bayesFactors), length.out = 5)->axe2
        format(axe2, scientific=T)->axe2b
        par(mar = c(4, 10, 0.5, 0.5), mgp = c(8, 1, 0))
        plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48", ylim= c(min(bayesFactors), max(bayesFactors)),
             yaxt = "n"    , xaxt = "n",  xlab = "Cauchy Prior Width (r)" , ylab = "Bayes Factor (10)")
        axis(2, labels=axe2b, at=axe2, las=2)
        abline(h = 0, lwd = 1)
        abline(h = 6, col = "black", lty = 2, lwd = 2)
        axis(1, at = seq(0, 1.5, 0.25))
        
        
        
        # add the BF at the default Cauchy point
        points(2^0.5/4, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
        points(0.5, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
        points(2^0.5/2, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
        # add legend
        legend(x="topright", legend = c("r = 0.353 - medium", "r = 0.5 - wide ", "r = 0.707 - ultrawide"),
               pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
               col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")
        
        
      }
      
      return(Resultats)
    }
    
    
    # package supprime "plyr",
    packages<-c("BayesFactor", "boot", "ggplot2", "ppcor","outliers","psych",  "svDialogs")
    
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    .e <- environment()
    Resultats<-list()
    try( windows(record=T), silent=T)->win
    if(class(win)=="try-error") quartz()
    if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data  
    
    corr.options<-corr.complet.in(X=X, Y=Y,Z=Z, data=data, group=group, param=param, outlier=outlier, save=save, info=T, n.boot=n.boot, rscale=rscale)
    if(is.null(corr.options)) return(analyse())
    choix<-corr.options$choix
    X<-corr.options$X
    Y<-corr.options$Y
    Z<-corr.options$Z
    group<-corr.options$group
    data<-corr.options$data
    param<-corr.options$options$choix
    if(corr.options$options$rscalei==T) rscale<-corr.options$options$rscale/2 else rscale<-corr.options$options$rscale
    n.boot<-corr.options$options$n.boot
    save<-corr.options$options$sauvegarde
    outlier<-corr.options$options$desires
    
    expand.grid(X,Y)->XY
    for(i in 1:length(XY[,1]))
    {
      X1<-as.character(XY[i,1])
      Y1<-as.character(XY[i,2])
      data1<-data[complete.cases(data[,c(Y1,X1,Z)]),]
      R1<-list()
      if(any(outlier%in%  c("Donnees completes", "complete"))){
        R1$"Donnees completes"<-corr.complet.out(X=X1, Y=Y1,Z=Z, data=data1, choix=choix, group=group, param=param, n.boot=n.boot, rscale=rscale)
      } 
      if(any(outlier%in%c("Identification of influential values","id"))|
         any(outlier%in%c("Data without influencing value", "removed"))){
        modele<-as.formula(paste0(X1,"~",Y1))
        if(!is.null(Z)){for(i in 1:length(Z))      modele<-update(modele, as.formula(paste0(".~.+",Z[i])))}
        data1$residu<-resid(lm(modele, data=data1))
        critere<-ifelse(is.null(z), "Grubbs", "z")
        valeurs.influentes(X="residu", critere=critere,z=z, data=data1)->influentes
      }
      if(any(outlier%in% c("id","Identification of influential values"))){influentes->R1$"Valeurs influentes"}
      if(any(outlier%in%c("removed", "Data without influencing value"))) {
        if(length(influentes$"influential observations")!=0 | 
           ! any(outlier %in% c("Donnees completes","complete"))){
          get("nettoyees", envir=.GlobalEnv)->nettoyees
          R1$"Data without influencing value"<-corr.complet.out(X=X1, Y=Y1,Z=Z, data=nettoyees, choix=choix, group=group, param=param, n.boot=n.boot, rscale=rscale)
        }
      }
      Resultats[[i]]<-R1
      names(Resultats)[i]<-paste("Correlation between the variable", X1, "et la variable", Y1)
    }
    
    paste(X, collapse="','", sep="")->X
    paste(Y, collapse="','", sep="")->Y
    if(!is.null(Z)) paste(Z, collapse="','", sep="")->Z
    if(!is.null(group)) paste(group, collapse="','", sep="")->group
    
    
    paste(outlier,  collapse="','", sep="")->outlier
    paste(param,  collapse="','", sep="")->param
    Resultats$Call<-paste0("corr.complet(X=c('", X,
                           "'), Y=c('", Y, 
                           "'), Z =", ifelse(!is.null(Z),paste0("c('",Z,"')"), "NULL"), ",data=",  corr.options$nom, 
                           ", group=", ifelse(!is.null(group),paste0("c('",group,"')"), "NULL"), 
                           ", param=c('", param, "'), save=", save, ",outlier=c('", outlier, "'),z=", ifelse(!is.null(z),z, "NULL"),
                           ", info=T, rscale=", rscale, 
                           ", n.boot=", ifelse(is.null(n.boot), "NULL",n.boot),", html=", html, ")")
    
    .add.history(data=data, command=Resultats$Call, nom=corr.options$nom)
    .add.result(Resultats=Resultats, name =paste(choix, Sys.time() ))
    
    if(save){ try(ez.html(Resultats, html=F), silent=T) }
    
    ref1(packages)->Resultats$"References"
    if(html) try(ez.html(Resultats), silent=T)
    ### Obtenir les Resultats
    return(Resultats) 
  }
