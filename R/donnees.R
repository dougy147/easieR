donnees <-
  function(){options (warn=-1)
    require(svDialogs)
  if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())) {
     choix<- c(txt_import_data, txt_view_data, txt_import_results,txt_export_data,
              txt_compile_report)
     if( "RGtk2Extras" %in% installed.packages()) choix<-c(txt_new_data_set, choix)
     title<-ask_what_to_do
       }else{
    choix<- c("import data", "View data", txt_import_results,"export data", "Compile the report of the session")
     if( "RGtk2Extras" %in% installed.packages()) choix<-c("new data set", choix)
    title<-"What do you want to do?"
  }
    dlgList(choix, preselect=NULL, multiple = FALSE,
            title=title)$res->choix
    if(length(choix)==0) return(easieR())
    if(choix %in% c(txt_new_data_set, "new data set")) blank.data()->Resultats
    if(choix %in% c(txt_view_data,"View data")) voir()->Resultats
    if(choix %in% c(txt_import_results, txt_import_results)) import.results()->Resultats
    if(choix %in% c(txt_import_data,"import data") ) import()->Resultats
    if(choix %in% c(txt_export_data, "export data")) exporterD()->Resultats
    if(choix %in% c(txt_compile_report, "Compile the report of the session")) {ez.report()
                                                                                 Resultats<-NULL}
    return(Resultats)
  }

