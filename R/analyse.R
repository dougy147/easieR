analyse <-
  function(html=T){options (warn=-1)
    require(svDialogs)
    dlgList(c("Descriptive statistics","chi.two","correlations", 
              "Student's t", "analysis of variance and covariance",
              "regressions",
              "factor and component analyzes",
              "reliability analysis and agreement"), preselect=NULL, multiple = FALSE, title="What analysis do you want to perform?")$res->choix
    if(length(choix)==0) return(easieR())
    if(choix=="chi.two") chi(html=html)->Resultats
    if(choix=="Student's t") test.t(html=html)->Resultats
    if(choix=="analysis of variance and covariance") {
      Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
      if(length(nom1)==0) ez.anova(html=html)->Resultats else {html=html
        dlgList(c("Main analysis", 
                  "Complementary results (e.g. interaction contrasts and adjusted means)"), 
                preselect=NULL, multiple = FALSE, title="What analysis do you want to perform?")$res->choix
        if(choix== "Main analysis") ez.anova(html=html)->Resultats else aov.plus(html=html)->Resultats
        
      }
      
    }
    if(choix=="correlations") choix.corr(html=html)->Resultats
    if(choix=="regressions") choix.reg(html=html)->Resultats
    #if(choix=="logistical regressions") regressions.log()->Resultats
    if(choix=="factor and component analyzes") factor.an(html=html)->Resultats
    if(choix=="reliability analysis and agreement") fiabilite(html=html)->Resultats
    if(choix=="Descriptive statistics") stat.desc(html=html)->Resultats
    return(Resultats)
  }
