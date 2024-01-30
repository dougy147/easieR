import.results <-
function(){
  require(pander)
    if (Sys.info()[["sysname"]] == "Linux") {
    	require("tcltk")
    	fichier <- try(tk_file.choose(), silent=TRUE)
    } else {
  	fichier <- try(file.choose(), silent=TRUE)
    }
  if(class(fichier)=="try-error") return(donnees())
  openFileInOS(fichier)
    Resultats<-paste("Les resultats ont ete correctement importes dans", fichier)
  return(Resultats)
}
