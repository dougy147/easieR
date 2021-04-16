donnees <-
  function(){options (warn=-1)
    require(svDialogs)
  if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())) {
     choix<- c("import data", "see data", "import results","export data",
              "generate report")   
     if( "RGtk2Extras" %in% installed.packages()) choix<-c("new data set", choix)
     title<-"What do you want to achieve?"
       }else{
    choix<- c("import data", "View data", "import results","export data", "Compile the report of the session")   
     if( "RGtk2Extras" %in% installed.packages()) choix<-c("new data set", choix)
    title<-"What do you want to do?"
  }
    dlgList(choix, preselect=NULL, multiple = FALSE, 
            title=title)$res->choix
    if(length(choix)==0) return(easieR())
    if(choix %in% c("new data set", "new data set")) blank.data()->Resultats
    if(choix %in% c("see data","View data")) voir()->Resultats
    if(choix %in% c("import results", "import results")) import.results()->Resultats
    if(choix %in% c("import data","import data") ) import()->Resultats
    if(choix %in% c("export data", "export data")) exporterD()->Resultats
    if(choix %in% c("generate report", "Compile the report of the session")) {ez.report()
                                                                                 Resultats<-NULL}
    return(Resultats)
  }

