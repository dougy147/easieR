graphiques <-
function(){
  
  c("ggplotgui", "svDialogs")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  data<-choice.data(nom=TRUE)
  if(length(data)==0) return(easieR())
  nom<-data[[1]]
  data<-data[[2]]
  
  msgBox("Do not forget to close the htmlt window (firexfox, chrome, internet explorer ...) to return to the R session")
   print(ref1(packages))
  ggplot_shiny(dataset=data)
 
}
