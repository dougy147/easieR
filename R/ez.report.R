ez.report<-function(html=NULL){
  options (warn=-1)
  require(svDialogs)
  if(is.null(html)){  choice<- c("html", "MS WORD")   
  if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())) {
    
    title<-"What format do you want?"
  }else{
       title<-"Which output do you want?"
  }
  choice<-dlgList(choice, preselect=NULL, multiple = FALSE, 
          title=title)$res
  if(length(choice)==0) return(donnees())
  if(choice=="html") html<-T else html<-F
    }
  ez.html(ez.results, html= html)
 
}
