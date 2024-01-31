analyse <-
  function(html=T){options (warn=-1)
    require(svDialogs)
    dlgList(c(TXT_descriptive_statistics,TXT_chi_squared,TXT_correlations, 
              TXT_student_t, TXT_anova_ancova,
              TXT_regressions,
              TXT_analysis_factor_component,
              TXT_fiability_analysis), preselect=NULL, multiple = FALSE, title=ASK_analysis_type)$res->choix
    if(length(choix)==0) return(easieR())
    if(choix==TXT_chi_squared) chi(html=html)->Resultats
    if(choix==TXT_student_t) test.t(html=html)->Resultats
    if(choix==TXT_anova_ancova) {
      Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
      if(length(nom1)==0) ez.anova(html=html)->Resultats else {html=html
        dlgList(c(TXT_principal_analysis, 
                  TXT_complementary_results), 
                preselect=NULL, multiple = FALSE, title=ASK_analysis_type)$res->choix
        if(choix== TXT_principal_analysis) ez.anova(html=html)->Resultats else aov.plus(html=html)->Resultats
        
      }
      
    }
    if(choix==TXT_correlations) choix.corr(html=html)->Resultats
    if(choix==TXT_regressions) choix.reg(html=html)->Resultats
    #if(choix==TXT_logistic_regressions) regressions.log()->Resultats
    if(choix==TXT_analysis_factor_component) factor.an(html=html)->Resultats
    if(choix==TXT_fiability_analysis) fiabilite(html=html)->Resultats
    if(choix==TXT_descriptive_statistics) stat.desc(html=html)->Resultats
    return(Resultats)
  }
