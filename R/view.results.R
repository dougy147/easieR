view.results <-
function(){
  c("svDialogs", "TeachingDemos")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  list()->Resultats
  Resultats$Call<-"view.results()"
  ref1(packages)->Resultats$INFO_packages_used_for_this_function
  if(!exists("ez.results")) return(INFO_no_saved_analysis_found) else get("ez.results")
  TkListView(ez.results)
  return(Resultats)
}
