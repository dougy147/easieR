donnees <-
  function(){options (warn=-1)
    require(svDialogs)
     choix<- c(TXT_import_data, TXT_view_data, TXT_import_results,TXT_export_data,
              TXT_compile_report)
     if( "RGtk2Extras" %in% installed.packages()) choix<-c(TXT_new_data_set, choix)
     title<-ASK_what_to_do
    dlgList(choix, preselect=NULL, multiple = FALSE,
            title=title)$res->choix
    if(length(choix)==0) return(easieR())
    if(choix %in% c(TXT_new_data_set, "new data set")) blank.data()->Resultats
    if(choix %in% c(TXT_view_data,"View data")) voir()->Resultats
    if(choix %in% c(TXT_import_results, TXT_import_results)) import.results()->Resultats
    if(choix %in% c(TXT_import_data,"import data") ) import()->Resultats
    if(choix %in% c(TXT_export_data, "export data")) exporterD()->Resultats
    if(choix %in% c(TXT_compile_report, "Compile the report of the session")) {ez.report()
                                                                                 Resultats<-NULL}
    return(Resultats)
  }

