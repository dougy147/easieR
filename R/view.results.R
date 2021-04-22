view.results <-
function(){
  c("svDialogs", "TeachingDemos")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  list()->Resultats
  Resultats$Call<-"view.results()"
  ref1(packages)->Resultats$"Packages used for this function"
  if(!exists("ez.results")) return("No saved scan could be found") else get("ez.results")
  TkListView(ez.results)
  return(Resultats)
}
