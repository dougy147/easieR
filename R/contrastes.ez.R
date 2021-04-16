contrasts.ez <-
  function(longdata, inter=NULL, intra=NULL){
    Results<-list()
    writeLines("Les contrasts a priori correspondent aux contrasts sans correction de la probabilite en suivant les regles de contrasts.
               Les contrasts 2 a 2 permettent de faire toutes les comparaisons 2 a 2 en appliquant ou non une correction a la probabilite")
    type.cont<- dlgList(c("a priori",  "Comparison 2 to 2", "no"), preselect="a priori",multiple = FALSE, title="What kind of contrast do you want?")$res
    if(length(type.cont)==0) return(NULL)
    Results$type.cont<-type.cont
    c(inter, unlist(intra))->interintra
    if(type.cont=="a priori") {
      contrasts<-list()
      writeLines("Vous pouvez choisir les contrasts que vous souhaitez. Neanless les regles concernant l'application des contrasts doivent etre respectees.
                 Les contrasts peuvent etre specifies manuellement. Dans ce cas, veuillez choisir specify the contrasts")
      cont.exemple<-list()
      contr.helmert(3)->cont.exemple$Orthogonaux
      apply(contr.helmert(3), 2, rev)->cont.exemple$Orthogonaux.inverses
      contr.poly(3)->cont.exemple$Polynomiaux
      contr.treatment(3, contrasts = TRUE, sparse = FALSE)->cont.exemple$comparaison.ligne.de.base
      print(cont.exemple)
      
      for (i in 1:length(interintra)){
        if(i>1) {
          type.cont2<- dlgList(c("orthogonal", "inverse orthogonal", "polynomials","comparison to a baseline", "specify the contrasts"), 
                               preselect=c("orthogonal"), multiple = FALSE, title=paste("What contrasts for the variable",names(longdata[interintra])[i],"?"))$res} else {
                                 type.cont2<- dlgList(c("orthogonal", "inverse orthogonal", "polynomials","comparison to a baseline", 
                                                        "specify the contrasts"),preselect=c("orthogonal"), multiple = FALSE, title=paste("What contrasts for the variable",names(longdata[interintra])[i],"?"))$res                      
                               }
        if(length(type.cont2)==0) return(contrasts.ez()) 
        if(type.cont2=="orthogonal") contr.helmert(nlevels(longdata[,interintra[i]]))->contrasts[[i]]
        if(type.cont2=="inverse orthogonal") apply(contr.helmert(nlevels(longdata[,interintra[i]])), 2, rev)->contrasts[[i]]
        if(type.cont2=="polynomials")  contr.poly(nlevels(longdata[,interintra[i]]))->contrasts[[i]]
        if(type.cont2=="comparison to a baseline") { 
          base<- dlgList(levels(longdata[, interintra[i]]), preselect=levels(longdata[,interintra[i]])[1],
                         multiple = FALSE, title="What is the baseline?")$res
          which(levels(longdata[, interintra[i]])==base)->base
          contr.treatment(levels(longdata[, interintra[i]]), base = base, contrasts = TRUE, sparse = FALSE)->contrasts[[i]]
        } 
        if(type.cont2=="specify the contrasts"){
          ortho<-FALSE
          while(ortho!=TRUE){
            matrix(rep(0,times=nlevels(longdata[,interintra[i]])*(nlevels(longdata[,interintra[i]])-1)), nrow=nlevels(longdata[,interintra[i]]))->contrasts3
            dimnames(contrasts3)[[1]]<-levels(longdata[,interintra[i]])
            dimnames(contrasts3)[[2]]<-paste("contrast", 1:(nlevels(longdata[,interintra[i]])-1), sep=".")
            fix(contrasts3)->contrasts3
            if(any(colSums(contrasts3)!=0)|(nlevels(longdata[,interintra[i]])>2 & max(rle(c(contrasts3))$lengths)>2*(nlevels(longdata[,interintra[i]])-2))) ortho<-FALSE else {
              test.out<-rep(1, length(contrasts3[,1]))
              for(j in 1:length(contrasts3[1,])) {contrasts3[,j]*test.out->test.out}
              if(sum(test.out)==0) ortho<-TRUE else ortho<-FALSE}
            if(ortho==FALSE) {dlgMessage("The contrasts must respect the orthogonality. Do you want to continue ?", "yesno")$res->cont
              if(cont=="no") return(contrasts.ez(longdata=longdata, inter=inter, intra=intra ))  }
            contrasts[[i]]<-contrasts3
            
          }
          
        }
        
        dimnames(contrasts[[i]])[[2]]<-paste("contrast", 1:(nlevels(longdata[,interintra[i]])-1), sep=".")
      }
      names(contrasts)<-interintra
      Results$contrasts<-contrasts
      
    }
    if(type.cont== "Comparison 2 to 2"){
      list()->p.adjust
      writeLines("Which probability correction do you want to apply? To not apply a correction, choose + none +")
      dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"), preselect="holm", multiple = FALSE, title="Type of correction?")$res->p.adjust
      if(length(p.adjust)==0) return(contrasts.ez())
      Results$p.adjust<-p.adjust
    }
    return(Results)
  }
