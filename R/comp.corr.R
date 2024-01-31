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
      Resultats$TXT_comparison_of_two_correlations<-r
      Resultats$call<-paste("comp.corr(xy=", xy, ",xz=", xz, ",yz=",yz, ",n=", n, ",n2=", n2, ",twotailed=",twotailed, ")")
      data1<-data.frame()
      .add.history(data=data1, command=Resultats$call, nom=paste(TXT_comparisons_XY, xy, TXT_and_YZ, yz ))
      .add.result(Resultats=Resultats, name =paste(TXT_correlations_comparison, Sys.time() ))
      Resultats$TXT_references<-ref1(packages)
      return(Resultats)
    } else{
      type<- dlgList(c(TXT_apparied_correlations, TXT_independant_correlations), preselect=FALSE, multiple = TRUE, title=TXT_compare_two_correlations)$res
      if(length(type)==0) return(choix.corr())
      
      if(type==TXT_independant_correlations) {
        Form <- list(
          TXT_XY_NUM_correlation = 0,
          TXT_N_of_XY_NUM_corr = 100,
          TXT_XZ_NUM_correlation = 0,
          TXT_N_of_XZ_NUM_corr = 100)
      }else{
        Form <- list(
          TXT_XY_NUM_correlation = 0,
          TXT_XZ_NUM_correlation = 0,
          TXT_YZ_NUM_correlation = 0,
          TXT_sample_size_NUM = 100)
      }
      
      value<-dlgForm(Form, ASK_enter_different_values)$res
      if(any(is.na(value))) {
        msgBox(INFO_some_values_are_not_numeric)
        comp.corr(xy=NULL, xz=NULL, yz=NULL, n=NULL, n2=NULL,twotailed=TRUE)->Resultats
        return(Resultats)
      }
      xy<-value$TXT_XY_correlation
      xz<-value$TXT_XZ_correlation
      yz<-value$TXT_YZ_correlation
      if(type==  TXT_apparied_correlations){n<-value$TXT_sample_size} else {
        n<-value$TXT_N_of_XY_corr
        n2<-value$TXT_N_of_XZ_corr
      }
      comp.corr(xy=xy, xz=xz, yz=yz, n=n, n2=n2,twotailed=twotailed)->Resultats
      if(html) html<-FALSE
      return(Resultats)
    }
    }

