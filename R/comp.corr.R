comp.corr <-
  function(xy=NULL, xz=NULL, yz=NULL, n=NULL, n2=NULL,twotailed=TRUE, html=FALSE){options (warn=-1) 
    #xy : value of the correlation between x and y
    #xz : value of the correlation between x and z
    #yz : value of the correlation between y and z. Should be null for independant comparisons et having a value for paired.
    # n : sample size for the correlation xy.
    # n2 : sample size for the correlation xz. 
    # twotailed : logical. Should the estimation of p be one(FALSE) or twotailed (TRUE). 
    
    c("psych", "svDialogs")->packages
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)} 
    list()->Resultats # cree une liste appelee Resultats dans laquelle on va stocker les Resultats
    
    if((all(c(xy, yz, xz)<=1) & all(c(xy, yz, xz)>=-1)) & 
       all(c(n,n2)>0) & all(c(n,n2)%%1==0)) {
      paired.r(xy=xy, xz=xz, yz=yz, n=n, n2=n2,twotailed=twotailed)->r
    } else {
      msgBox("Les valeurs des correlations doivent etre comprises entre -1 et 1/n
             et les effectifs doivent etre des entiers positifs")
    }
    
    if(exists("r") && length(r$p)!=0 && !is.na(r$p)) {
      Resultats$"comparison of the two correlations"<-r
      Resultats$call<-paste("comp.corr(xy=", xy, ",xz=", xz, ",yz=",yz, ",n=", n, ",n2=", n2, ",twotailed=",twotailed, ")")
      data1<-data.frame()
      .add.history(data=data1, command=Resultats$call, nom=paste("comparison of correlations XY =", xy, "and YZ =", yz ))
      .add.result(Resultats=Resultats, name =paste("comparison of correlations", Sys.time() ))
      Resultats$"References"<-ref1(packages)
      return(Resultats)
    } else{
      type<- dlgList(c("Matched correlations", "Independent correlations"), preselect=FALSE, multiple = TRUE, title="Comparison of two correlations")$res
      if(length(type)==0) return(choix.corr())
      
      if(type=="Independent correlations") {
        Form <- list(
          "Correlation between XY: NUM" = 0,
          "N of the correlation XY: NUM" = 100,
          "Correlation between XZ: NUM" = 0,
          "N of the correlation XZ: NUM" = 100)
      }else{
        Form <- list(
          "Correlation between XY: NUM" = 0,
          "Correlation between XZ: NUM" = 0,
          "Correlation between YZ: NUM" = 0,
          "Sample size: NUM" = 100)
      }
      
      value<-dlgForm(Form, "Please enter the different values")$res
      if(any(is.na(value))) {
        msgBox("Not all values entered are numeric. Please enter numeric values only")
        comp.corr(xy=NULL, xz=NULL, yz=NULL, n=NULL, n2=NULL,twotailed=TRUE)->Resultats
        return(Resultats)
      }
      xy<-value$"Correlation between XY"
      xz<-value$"Correlation between XZ"
      yz<-value$"Correlation between YZ"
      if(type==  "Matched correlations"){n<-value$"Sample size"} else {
        n<-value$"N of the correlation XY"
        n2<-value$"N of the correlation XZ"
      }
      comp.corr(xy=xy, xz=xz, yz=yz, n=n, n2=n2,twotailed=twotailed)->Resultats
      if(html) html<-FALSE
      return(Resultats)
    }
    }

