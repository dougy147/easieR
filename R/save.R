save <-
  function(Results=NULL, choice, env=.GlobalEnv){options (warn=-1)
    # Results = object that must be saved
    # choice = name of the file 
    # env = environment in which to find the object
    packages<-c("svDialogs", "htmltools" )
    test2<-try(lapply(packages, library, character.only=T), silent=T)
    if(class(test2)== "try-error") return(ez.install())
    Results <- list()
                                                  
    if(is.null(Results) & exists("ez.results")) Results<-ez.results else return("no results have been saved")
    ez.html <-function(ez.results=Results)
      
    fileHTML<-file.path("file:/", tempdir(), "easieR/Rapport.easieR.html")
    fileNAME<-dlgInput("What name do you want to give to the file")$res 
    fileNAME<-strsplit(fileNAME, ":")
    fileNAME<-tail(fileNAME[[1]],n=1)                                         
    save_html(html=fileHTML, file=fileNAME, background = "white")
    
    Results<-NULL
    Results$SAUVEGARDE<-paste("the data is saved in", getwd())
    
  }
