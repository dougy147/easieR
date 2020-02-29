import.results <-
function(){
  require(pander)
  fichier <- try(tk_choose.files(), silent=TRUE)
  if(class(fichier)=="try-error") return(donnees())
  openFileInOS(fichier)
    Resultats<-paste("Les resultats ont ete correctement importes dans", fichier)
  return(Resultats)
}
