import.results <-
function(){
  require(pander)
  fichier <- try(file.choose(), silent=TRUE)
  if(class(fichier)=="try-error") return(donnees())
  openFileInOS(fichier)
    Resultats<-paste(INFO_result_succesfully_imported_in, fichier)
  return(Resultats)
}
