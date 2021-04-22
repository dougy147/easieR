import.results <-
function(){
  require(pander)
    if(grepl("Linux", Sys.info()[[1]])){
	    fichier <- try(tk_choose.files(), silent=TRUE)
	    }
    else{
	    fichier <- try(file.choose(), silent=TRUE)
    }
  if(class(fichier)=="try-error") return(donnees())
  openFileInOS(fichier)
    Resultats<-paste("The results were correctly imported into", fichier)
  return(Resultats)
}
