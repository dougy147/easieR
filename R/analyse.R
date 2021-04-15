analyse <-
  function(html=T){options (warn=-1)
    require(svDialogs)
    dlgList(c("Descriptive statistics","chi.deux","correlations", 
              "Student's t", "analysis of variance and covariance",
              "regressions",
              "factor and component analyzes",
              "reliability analysis and agreement"), preselect=NULL, multiple = FALSE, title="What analysis do you want to perform?")$res->choice
    if(length(choice)==0) return(easieR())
    if(choice=="chi.deux") chi(html=html)->Results
    if(choice=="Student's t") test.t(html=html)->Results
    if(choice=="analysis of variance and covariance") {
      Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
      if(length(nom1)==0) ez.anova(html=html)->Results else {html=html
        dlgList(c("Main analysis", 
                  "Complementary results (e.g. interaction contrasts and adjusted means)"), 
                preselect=NULL, multiple = FALSE, title="What analysis do you want to perform?")$res->choice
        if(choice== "Main analysis") ez.anova(html=html)->Results else aov.plus(html=html)->Results
        
      }
      
    }
    if(choice=="correlations") choice.corr(html=html)->Results
    if(choice=="regressions") choice.reg(html=html)->Results
    #if(choice=="logistical regressions") regressions.log()->Results
    if(choice=="factor and component analyzes") factor.an(html=html)->Results
    if(choice=="reliability analysis and agreement") fiabilite(html=html)->Results
    if(choice=="Descriptive statistics") stat.desc(html=html)->Results
    return(Results)
  }
