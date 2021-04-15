donnees <-
  function(){options (warn=-1)
    require(svDialogs)
  if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())) {
     choice<- c("import data", "see data", "import results","export data",
              "generate report")   
     if( "RGtk2Extras" %in% installed.packages()) choice<-c("new data set", choice)
     title<-"What do you want to achieve?"
       }else{
    choice<- c("import data", "View data", "import results","export data", "Compile the report of the session")   
     if( "RGtk2Extras" %in% installed.packages()) choice<-c("new data set", choice)
    title<-"What do you want to do?"
  }
    dlgList(choice, preselect=NULL, multiple = FALSE, 
            title=title)$res->choice
    if(length(choice)==0) return(easieR())
    if(choice %in% c("new data set", "new data set")) blank.data()->Results
    if(choice %in% c("see data","View data")) voir()->Results
    if(choice %in% c("import results", "import results")) import.results()->Results
    if(choice %in% c("import data","import data") ) import()->Results
    if(choice %in% c("export data", "export data")) exporterD()->Results
    if(choice %in% c("generate report", "Compile the report of the session")) {ez.report()
                                                                                 Results<-NULL}
    return(Results)
  }

