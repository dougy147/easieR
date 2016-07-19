# easieR

#
################################
####                        ####
####          easieR        ####
####                        ####
################################


### easieR
# Ã  faire : inclure dans easier l'installation  et la mise Ã  jour des packages
# faire en sorte de pouvoir sourcer le code

# si tu veux source()r ce script, il faudra ajouter un truc du genre :
message("easieR Ã©crit par Nicolas Stefaniak", "\nVersion 1.6.19, modifiÃ©e le 11/07/2016", "\nTaper 'easieR()' dans la console pour utiliser ce package", "\n") 

pack.to.inst <- c("afex", "akima",  "asbio", "car", "cobs", "corpcor", "deldir", "DescTools", "epitools", "ggplot2", "gmodels", "GPArotation", "gsl", "lars", "lsr", "MBESS", "mc2d", "mlogit", "nFactors", "nortest", "outliers", "pgirmess", "phia", "plyr", "ppcor", "psych", "pwr", 
                  "QuantPsyc", "quantreg", "Rcpp", "readxl", "Rfit", "reshape2", "rms", "robust", "robustbase", "rtf", "rrcov", "scatterplot3d", "sos", "sp", "stringi", "stringr", "svDialogs", "trimcluster", "wle", "WRS2")
#install packages
if(length(pack.uninst)>0) install.packages(pack.uninst)

#WRS is a special case because it is not on CRAN
if (!("WRS" %in% installed.packages())) install.packages("WRS", repos="http://R-Forge.R-project.org", type="source")


easieR<-function(info=TRUE){options (warn=-1)
  # l'argument info permettra a terme de choisir les informations qui s'affichent dans la console ou non 
  options(scipen=999)
  require(svDialogs)
  print("version 1.7.0 de easieR 06/07/2016")
  dlgList(c("Donnees - Importation, tri, selection, pretraitements", 
            "Analyses - Tests d hypothese", "Interface - objets en memoire, nettoyer la memoire, repertoire de travail"), preselect=NULL, multiple = FALSE, title="Que voulez vous?")$res->choix
  if(length(choix)==0) return("Vous avez quitte easier")else{
    if(choix=="Donnees - Importation, tri, selection, pretraitements") donnees()->Resultats
    if(choix=="Analyses - Tests d hypothese") {analyse()->Resultats
    }
    if(choix=="Interface - objets en memoire, nettoyer la memoire, repertoire de travail") interfaceR()->Resultats
  }
  return(Resultats)
}




################################
####                        ####
####    interface de R      ####
####                        ####
################################

### interface de R (rÃ©pertoire, donnÃ©es en mÃ©moire, connaÃ®tre les fonctions qui permettent de rÃ©aliser une analyse particuliÃ¨re)
# il manque la possibilitÃ© de choisir en fonction de la nature des objets qu'on veut voir dans ls()
interfaceR<-function(){options (warn=-1) 
  require(svDialogs)
  list()->Resultats
  
  dlgList(c("obtenir le repertoire de travail","specifier le repertoire de travail", 
            "Suppression d objet en memoire", "liste des objets en memoire", "rechercher une nouvelle fonction"), preselect=NULL, multiple = FALSE, title="Quel est votre choix ?")$res->choix
  while(length(choix)==0) {return(easieR())
  }
  
  switch(choix, 
         "obtenir le repertoire de travail" = getwd()->Resultats,
         "liste des objets en memoire"= ls(envir=.GlobalEnv)->Resultats,
         "specifier le repertoire de travail"={
           dlgDir(title="Veuillez choisir le repertoire de travail")$res->repertoire
           if(length(repertoire)==0) repertoire<-getwd()
           setwd(repertoire)
           Resultats<-paste("Le repertoire de travail est a present", repertoire)
         },
         "Suppression d objet en memoire"= {options (warn=-1)
           packages<-c("svDialogs")
           if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
             require(packages)}
           X<-dlgList(ls(envir=.GlobalEnv), multiple = TRUE, title="Objets a supprimer")$res
           if(length(X)==0) return(easieR())
           rm(list=X, envir=.GlobalEnv)
           Resultats<-list()
           ls(envir=.GlobalEnv)->Resultats[[1]]
           names(Resultats[[1]])<-"Liste des objects encore en memoire de R"
         },
         "rechercher une nouvelle fonction"={require(sos)
           critere <- dlgInput("Quelle analyse recherchez vous", "Tapez le nom de l'analyse ici")$res
           if(length(critere)==0) return(easieR())
           strsplit(critere, ":")->critere
           tail(critere[[1]],n=1)->critere
           findFn(critere)->Resultats
         })
  return(Resultats)
}

################################
####                        ####
####        Donnees         ####
####                        ####
################################

### Donnees

#### function qui permet centrer / centrer rÃ©duire 
Centrer.red<-function(x, data=NULL, info=TRUE){options (warn=-1) 
  packages<-c("svDialogs")
  #faire l analyse par groupe # regler le probleme des noms
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  X<-"autres donnees" 
  while(any(X=="autres donnees")){nom <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  if(info==TRUE) {print(("veuillez choisir la base de donnees"))}
  nom<-dlgList(c(nom, "annuler"), multiple = FALSE, title="Choix du dataframe")$res
  if(length(nom)==0||any(nom=="annuler")) return(donnees())
  data<-get(nom)
  if(info==TRUE) {print(("veuillez choisir la ou les variables "))}
  X<-dlgList(names(data), multiple = TRUE, title="Variable(s)")$res
  if(length(X)==0) X<-"autres donnees"
  if(any(X=="annuler")) return(donnees())}
  if(class(data[,X])=="factor") {print("la variable n est pas numerique")
    return(donnees())}
  if(info==TRUE) {print("Centrer permet d avoir un moyenne a zero en maintenant l ecart type. Centrer reduire correspond a la formule du z. 
                        La moyenne est de 0 et l ecart-type vaut 1. La proabilite inferieure correspond la probabilite d'avoir un z inferieur ou egal au z.
                        La probabilite superieure correspond a la probabilite d avoir un z superieur ou egal au z")}
  dlgList(c("centrer", "centrer reduire", "probabilite inferieure", "probabilite superieure"), preselect="centrer reduire", multiple = TRUE, title="Format du fichier?")$res->choix
  if(length(choix)==0) return(donnees())
  for(i in 1:length(X)){
    X[i]->Y
    if(any(choix=="centrer")){data[,Y]-mean(data[,Y])->data$centree
      names(data)[length(data)]<-paste(Y, ".centree")
    }
    
    if(any(choix=="centrer reduire")){scale(data[,Y])->data$centree
      names(data)[length(data)]<-paste(Y, ".centree.reduite")
    }
    
    if(any(choix=="probabilite inferieure")){scale(data[,Y])->centree
      pnorm(centree, lower.tail = TRUE)->data$p.inf
      names(data)[length(data)]<-paste(Y, ".p.inferieure")
    }
    
    if(any(choix=="probabilite superieure")){scale(data[,Y])->centree
      pnorm(centree, lower.tail = FALSE)->data$p.sup
      names(data)[length(data)]<-paste(Y, ".p.superieure")
    }
    assign(nom, data, envir=.GlobalEnv)
    View(data)
    Resultats<-paste(choix, "la (les) variable-s", Y, "realise")
    return(Resultats)
  }}

#### function qui permet de choisir l'opÃ©ration qu'on veut rÃ©aliser
donnees<-function(){options (warn=-1)
  require(svDialogs)
  dlgList(c("importer des donnees", "voir des donnees", "importer des resultats",
            "Selectionner des observations",
            "Selectionner des variables",
            "Exporter des donnees",
            "Centrer / centrer reduire","trier","Operations mathematiques sur des variables"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez vous realiser?")$res->choix
  if(length(choix)==0) return(easieR())
  if(choix=="voir des donnees") voir()->Resultats
  if(choix=="importer des resultats") import.results()->Resultats
  if(choix=="importer des donnees") import()->Resultats
  if(choix=="Selectionner des observations") selectionO()->Resultats
  if(choix=="Selectionner des variables") SelectionV()->Resultats
  if(choix=="Exporter des donnees") exporterD()->Resultats
  if(choix=="trier") trier()->Resultats
  if(choix=="Centrer / centrer reduire") Centrer.red()->Resultats
  if(choix=="Operations mathematiques sur des variables") maths()->Resultats
  return(Resultats)
}

#### Export des donnÃ©es en fichier csv
exporterD<-function(data=NULL, nom=NULL){options (warn=-1)   
  packages<-c("svDialogs")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  data <- dlgList(Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv)), multiple = FALSE, title="Quels Resultats voulez vous?")$res 
  if(length(data)==0) return(donnees())
  data<-get(data)
  nom <- dlgInput("Quel nom voulez vous attribuer au fichier ?", "Nouveau.fichier")$res
  if(length(nom)==0) nom<-"Nouveau.fichier"
  strsplit(nom, ":")->nom
  tail(nom[[1]],n=1)->nom
  write.csv(data, file=paste(nom, ".csv"))
  paste("le fichier est sauvegarde dans", getwd())->Resultats
  return(Resultats)
}

#### fonction d'importation de donnÃ©es
import<-function(info=TRUE){
  options (warn=-1)
  
  #simplifiÃ© car fait avant
  
  c("svDialogs", "stringr", "readxl")->package
  lapply(packages, require)
  
  #Ã  quoi Ã§a sert ? Voir aussi Ã  la fin
  Resultats <- list()
  
  if(info==TRUE) print("Dans quel format est enregistre votre fichier?")
  choix <- dlgList(c("Fichier CSV", "Fichier txt", "Fichier Excel"), preselect="Fichier Excel", multiple = FALSE, title="Format du fichier?")$res
  
  #pourquoi utiliser return() ?
  if(length(choix)==0) return(easieR())
  fichier <- file.choose()

  setwd(dirname(fichier))
  
  #Ã§a pourrait Ãªtre aussi intÃ©ressant dans le cas d'un xls donc je l'ai sorti du 'if'
  if(info==TRUE) print("Est-ce que le nom des variables est sur la premiere ligne de votre base de donnees ? Choisir TRUE si c est le cas")
  noms <- dlgList(c(TRUE, FALSE), preselect=TRUE, multiple = FALSE, title="Nom de variables?")$res
  if(length(noms)==0) noms <- TRUE
  
  #idem
  if(info==TRUE) print("Si certaines donnees sont manquantes, comment sont-elles definies ? Vous pouvez laisser NA si les cellule sont vides")
  manquant <- dlgInput("Par quelle valeur sont definies les valeurs manquantes ?", "NA")$res
  if(length(manquant)==0) manquant <- "NA"
  manquant <- strsplit(manquant, ":")
  manquant <- tail(manquant[[1]],n=1)
  
  if(choix=="Fichier CSV"|choix=="Fichier txt"){
    if(info==TRUE) print("Lors de l enregistrement de votre fichier, quel indice de separation des colonnes avez-vous choisi ?")
    sep <- dlgList(c("espace","tab","point virgule","virgule"), preselect="point virgule", multiple = FALSE, title="Separateur de colonnes")$res
    if(length(sep)==0) sep <- ";"
    m1 <- matrix(c("espace","tab","point virgule","virgule"," ","\t",";",","),nrow=4)
    sep <- subset(m1, m1[,1] %in% sep)[,2]
    if(info==TRUE) print("Si certaines donnees contiennent des decimales, quel est le symbole indiquant la decimale ?")
    dec <- dlgList(c("point", "virgule"), preselect=NULL, multiple = FALSE, title="Separateur de decimales")$res
    if(length(dec)==0) dec <- "."
    m1 <- matrix(c("point", "virgule",".",","),nrow=2)
    dec <- subset(m1, m1[,1] %in% dec)[,2]  
  }
  
  if(choix=="Fichier CSV") data1 <- read.csv2(fichier, header=as.logical(noms), sep=sep, dec=dec, na.strings=manquant)
  if(choix=="Fichier txt") data1 <- read.table(fichier, header=as.logical(noms), sep=sep, dec=dec, na.strings=manquant)
  if(choix=="Fichier Excel"){
    fichier2 <- dlgInput("Quelle est le nom ou le numero de la feuille de calcul?", 1)$res
    
    #pourquoi utiliser return() ?
    if(length(fichier2)==0) return(import())
    fichier2 <- strsplit(fichier2, ":")
    fichier2 <- tail(fichier2[[1]],n=1)
    
    #Ca peut Ãªtre 'character(0)' mais pas 'NA', non ? Et si c'est 'character(0)', tu relances import(). Donc je mettrais seulement la partie aprÃ¨s 'else'
    if(is.na(as.numeric(fichier2))) fichier2 <- fichier2 else fichier2 <- as.numeric(fichier2)
    
    #avec readxl::read_excel
    #les arguments 'skip' et 'col_types' peuvent Ãªtre intÃ©ressants
    #j'ai aussi modifiÃ© un peu la partie factor() ; pour moi, c'est plus clair, et Ã§a devrait marcher aussi bien
    data1 <- read_excel(path=fichier, sheet=fichier2, col_names=as.logical(noms), na=manquant)
    col.char <-sapply(data1, is.character)
    if(any(col.char)) data1[col.char] <- lapply(data1[which(col.char)], factor)
  }
  
  fichier <- dlgInput("Quel nom voulez vous donner au fichier?", "data1")$res
  if(length(fichier)==0) fichier <- "data1"
  fichier <- strsplit(fichier, ":")
  fichier <- tail(fichier[[1]],n=1)
  assign(x=fichier, value=data1, envir=.GlobalEnv)
  View(data1, "donnees que vous venez d importer")
  
  #pourquoi ne pas simplement faire un 'print' ? J'imagine que c'est par rapport Ã  easierR(), mais ce n'est peut-Ãªtre pas nÃ©cessaire ici
  Resultats <- "les donnees ont ete importees correctement"
  return(Resultats)
  
}

#### fonction d'importation de rÃ©sultats/donnes issu du codage R (dput dget)
import.results<-function(){
  
  file.choose()->fichier
  dget(fichier)->data1
  fichier<- dlgInput("Quel nom voulez vous donner au fichier?", "Resultats")$res
  if(length(fichier)==0) fichier<-"data1"
  strsplit(fichier, ":")->fichier
  tail(fichier[[1]],n=1)->fichier
  assign(x=fichier, value=data1, envir=.GlobalEnv)
  Resultats<-paste("Les resultats ont ete correctement importes dans", fichier)
  return(Resultats)
}



#### RÃ©aliser les opÃ©rations mathÃ©matiques
# il manque la possibilitÃ© de faire les analyses avec des valeurs particuliÃ¨res (ajouter ou soustraire une valeur)
# les exponentielles, les log, les fonctions gÃ©omÃ©triques 
maths<-function(info=TRUE){
  options (warn=-1) 
  packages<-c("svDialogs")
  #faire l analyse par groupe # regler le probleme des noms
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  if(info=="TRUE") print("Veuillez choisir la base de donnees sur laquelle appliquer ")
  nom <- dlgList(Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv)), multiple = FALSE, title="Donnees ?")$res  
  if(length(nom)==0) return(easieR())
  data<-get(nom)
  if(info=="TRUE") print("Veuillez  choisir l operation mathematique vous desirez realiser ")
  dlgList(c("additions de colonnes","multiplication", "division", "soustraction","moyenne de colonnes"), preselect="additions", multiple = TRUE, title="Quelle operation voulez vousr?")$res->choix
  if(length(choix)==0) return(donnees())
  
  if(choix=="additions de colonnes") {
    if(info=="TRUE") print("Veuillez selectionner les variables a additionner ")
    X<-dlgList(c(names(data), "annuler"), multiple = TRUE, title="Variable(s)")$res
    if(length(X)==0|| any(X=="annuler")) return(donnees())
    if(any(sapply(data[,X], class)=="factor")) {print("au moins une des variables n est pas numerique")
      print(str(data))
      return(maths())}
    rowSums(data[,X])->data$nouvelle_variable
  }
  
  if(choix=="multiplication"){
    if(info=="TRUE") print("Veuillez selectionner les variables a multiplier ")
    X<-dlgList(c(names(data), "annuler"), multiple = TRUE, title="Variable(s) a multiplier")$res
    if(length(X)==0|| any(X=="annuler")) return(maths())
    if(any(sapply(data[,X], class)=="factor")) {print("au moins une des variables n est pas numerique")
      print(str(data))
      return(maths())}
    1*data[,X[1]]->nouvelle
    for(i in 1:(length(X)-1)) nouvelle*data[,X[i+1]]->nouvelle
    data.frame(data, nouvelle)->data
  }
  
  if(choix=="division"){
    if(info=="TRUE") print("Veuillez selectionner la variable au numerateur ")
    X<-dlgList(c(names(data), "annuler"), multiple = FALSE, title="Numerateur")$res
    if(length(X)==0 || any(X=="annuler")) return(donnees())
    if(info=="TRUE") print("Veuillez selectionner la variable au denominateur ")
    Y<-dlgList(c(names(data)), multiple = FALSE, title="Denominateur")$res
    if(length(Y)==0) return(maths())
    if(any(sapply(data[,c(X,Y)], class)=="factor")) {print("au moins une des variables n est pas numerique")
      print(str(data))
      return(maths())}
    data[,X]/data[,Y]->data$nouvelle_variable
  }
  
  if(choix=="soustraction") {
    if(info=="TRUE") print("Veuillez selectionner les variables positives. Si plusieurs variables sont selectionnees, elles sont additionnees au prealable")
    X<-dlgList(c(names(data), "annuler"), multiple = TRUE, title="Variable(s) positives")$res
    if(length(X)==0 || any(X=="annuler"))  return(maths())
    if(info=="TRUE") print("Veuillez selectionner les variables negatives pour lesquelles un coefficient negatif est attribue. Si plusieurs variables sont selectionnees, elles sont additionnees 
                           avant d etre soustraite aux variables positives")
    Y<-dlgList(c(names(data)), multiple = TRUE, title="Variable(s) negatives")$res
    if(length(Y)==0)  return(maths())
    if(any(sapply(data[,c(X,Y)], class)=="factor")){print("au moins une des variables n est pas numerique")
      print(str(data))
      return(maths())}
    if(length(X)>1) rowSums(data[,X])->nouvelle else data[,X]->nouvelle
    if(length(Y)>1) rowSums(data[,Y])->nouvelle2 else data[,Y]->nouvelle2
    nouvelle-nouvelle2->nouvelle
    data.frame(data, nouvelle)->data
  }
  
  if(choix=="moyenne de colonnes")  {
    if(info=="TRUE") print("Veuillez selectionner les variables a moyenner ")
    X<-dlgList(c(names(data), "annuler"), multiple = TRUE, title="Variable(s)")$res
    if(length(X)==0 || any(X=="annuler")) return(maths())
    if(any(sapply(data[,X], class)=="factor")){print("au moins une des variables n est pas numerique")
      print(str(data))
      return(maths())}
    rowMeans(data[,X])->data$nouvelle_variable
  }
  
  if(info=="TRUE") print("Quel nom voulez-vous attribuer Ã  la nouvelle variable ? ")
  variable<-dlgInput("Nom de la nouvelle variable ?","nouvelle.variable")$res
  if(length(variable)==0) variable<-"nouvelle.variable"
  strsplit(variable, ":")->variable
  tail(variable[[1]],n=1)->variable
  names(data)<-c(names(data)[1:(length(data)-1)], variable)
  assign(nom, data, envir=.GlobalEnv)
  Resultats<-paste("La variable", variable, "a ete ajoutee a", nom)
  View(data)
  return(Resultats)
}

#### Fonction de sauvegarde. fonction qui n'apparaÃ®t pas directement dans easieR mais est inclut dans le reste des fonctions
save<-function(Resultats, choix, env=.GlobalEnv){options (warn=-1)
  require(rtf)
  gsub(":",".",date())->date
  output<-paste(choix,date, ".doc")
  
  rtf<-RTF(output,width=30,height=20,font.size=12,omi=c(1,1,1,1))
  
  for(i in 1:length(Resultats)){
    names(Resultats)[[i]]->titres
    addHeader(rtf,title=titres, font.size=16)
    if(any(class(Resultats[[i]])=="chr")|any(class(Resultats[[i]])=="character")) {addText.RTF(rtf, Resultats[[i]])
      addNewLine(rtf, n=2)
    }
    if(any(class(Resultats[[i]])=="matrix")) {
      data.frame(Resultats[[i]]) ->essai
      round(essai,4)->essai
      addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
      addNewLine(rtf, n=2)
    }
    if(any(class(Resultats[[i]])=="data.frame") && length(Resultats[[i]])!=0) {
      if(any(sapply(Resultats[[i]], class)=="numeric")) Resultats[[i]][,sapply(Resultats[[i]], class)=="numeric"]<-lapply(Resultats[[i]][,sapply(Resultats[[i]], class)=="numeric"],round,4)
      addTable(rtf,Resultats[[i]], row.names=TRUE,col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(Resultats[[i]]))))
      addNewLine(rtf, n=2)
    }
    if(any(class(Resultats[[i]])=="table")) {
      matrix(Resultats[[i]], ncol=ncol(Resultats[[i]]))->essai
      data.frame(essai)->essai
      dimnames(Resultats[[i]])[[2]]->names(essai)
      addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
      addNewLine(rtf, n=2)
    }
    
    if(class(Resultats[[i]])=="list"){
      for(j in 1:length(Resultats[[i]])) {
        names(Resultats[[i]])[[j]]->ss.titre
        addHeader(rtf,title=ss.titre, TOC.level=2,font.size=14)
        if(class(Resultats[[i]][[j]])=="formula") { 
          as.character(paste(Resultats[[i]][[j]][2],Resultats[[i]][[j]][1], Resultats[[i]][[j]][3]))->formule
          addText.RTF(rtf,formule )
          addNewLine(rtf, n=2)
        }
        if(class(Resultats[[i]][[j]])=="numeric" ) { 
          round(matrix(Resultats[[i]][[j]],nrow=1),4)->essai
          dimnames(essai)[[2]]<-names(Resultats[[i]][[j]])
          dimnames(essai)[[1]]<-list()
          addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(Resultats[[i]][[j]])) ))}
        addNewLine(rtf, n=2)
        
        if(any(class(Resultats[[i]][[j]])=="list")){
          for(k in 1: length(Resultats[[i]][[j]])){
            if(any(class(Resultats[[i]][[j]][[k]])=="chr")|any(class(Resultats[[i]][[j]][[k]])=="character")) {addText.RTF(rtf, Resultats[[i]][[j]][[k]])
              addNewLine(rtf, n=2)
            }
            if(any(class(Resultats[[i]][[j]][[k]])=="numeric")&& length(Resultats[[i]][[j]][[k]])>1)   { 
              round(matrix(Resultats[[i]][[j]][[k]],nrow=1),4)->essai
              dimnames(essai)[[2]]<-names(Resultats[[i]][[j]][[k]])
              dimnames(essai)[[1]]<-list()
              addTable(rtf,essai, row.names=TRUE,col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
              addNewLine(rtf, n=2)}
            
            
            if(any(class(Resultats[[i]][[j]][[k]])=="table")) {
              matrix(Resultats[[i]][[j]][[k]], ncol=ncol(Resultats[[i]][[j]][[k]]))->essai
              data.frame(essai)->essai
              dimnames(Resultats[[i]][[j]][[k]])[[2]]->names(essai)
              addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
              addNewLine(rtf, n=2)
            }
            
            if(any(class(Resultats[[i]][[j]][[k]])=="matrix")) {
              data.frame(Resultats[[i]][[j]][[k]]) ->essai
              round(essai,4)->essai
              addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
              addNewLine(rtf, n=2)
            }
            if(any(class(Resultats[[i]][[j]][[k]])=="data.frame") && length(Resultats[[i]][[j]][[k]])!=0) {
              if(any(sapply(Resultats[[i]][[j]][[k]], class)=="numeric")) Resultats[[i]][[j]][[k]][,sapply(Resultats[[i]][[j]][[k]], class)=="numeric"]<-lapply(Resultats[[i]][[j]][[k]][,sapply(Resultats[[i]][[j]][[k]], class)=="numeric"],round,4)
              addTable(rtf,Resultats[[i]][[j]][[k]], row.names=TRUE,col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(Resultats[[i]][[j]][[k]]))))
              addNewLine(rtf, n=2)
            }
            if(any(class(Resultats[[i]][[j]][[k]])=="list")){
              for(l in 1: length(Resultats[[i]][[j]][[k]])){
                if(any(class(Resultats[[i]][[j]][[k]])=="chr")|any(class(Resultats[[i]][[j]][[k]][[l]])=="character")) {addText.RTF(rtf, Resultats[[i]][[j]][[k]][[l]])
                  addNewLine(rtf, n=2)
                }
                if(any(class(Resultats[[i]][[j]][[k]][[l]])=="numeric")&& length(Resultats[[i]][[j]][[k]][[l]])>1)   { 
                  round(matrix(Resultats[[i]][[j]][[k]][[l]],nrow=1),4)->essai
                  dimnames(essai)[[2]]<-names(Resultats[[i]][[j]][[k]][[l]])
                  dimnames(essai)[[1]]<-list()
                  addTable(rtf,essai, row.names=TRUE,col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
                  addNewLine(rtf, n=2)}
                if(any(class(Resultats[[i]][[j]][[k]][[l]])=="list")){ data.frame(Resultats[[i]][[j]][[k]][[l]])->essai
                  if(any(sapply(essai, class)=="numeric")) essai[,sapply(essai, class)=="numeric"] <- lapply(essai[,sapply(essai, class)=="numeric"],round,5)
                  
                  addTable(rtf,essai, row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
                  addNewLine(rtf, n=2)
                }
                
                if(any(class(Resultats[[i]][[j]][[k]][[l]])=="table")) {
                  matrix(Resultats[[i]][[j]][[k]], ncol=ncol(Resultats[[i]][[j]][[k]][[l]]))->essai
                  data.frame(essai)->essai
                  dimnames(Resultats[[i]][[j]][[k]][[l]])[[2]]->names(essai)
                  addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
                  addNewLine(rtf, n=2)
                }
                
                if(any(class(Resultats[[i]][[j]][[k]][[l]])=="matrix")) {
                  data.frame(Resultats[[i]][[j]][[k]][[l]]) ->essai
                  round(essai,4)->essai
                  addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
                  addNewLine(rtf, n=2)
                }
                if(any(class(Resultats[[i]][[j]][[k]][[l]])=="data.frame") && length(Resultats[[i]][[j]][[k]][[l]])!=0) {
                  if(any(sapply(Resultats[[i]][[j]][[k]][[l]], class)=="numeric")) Resultats[[i]][[j]][[k]][[l]][,sapply(Resultats[[i]][[j]][[k]][[l]], class)=="numeric"]<-lapply(Resultats[[i]][[j]][[k]][[l]][,sapply(Resultats[[i]][[j]][[k]][[l]], class)=="numeric"],round,4)
                  addTable(rtf,Resultats[[i]][[j]][[k]][[l]], row.names=TRUE,col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(Resultats[[i]][[j]][[k]][[l]]))))
                  addNewLine(rtf, n=2)
                }
              }
              addNewLine(rtf, n=2)
              
              
              
            }
          }
          addNewLine(rtf, n=2)
        }
        if(any(class(Resultats[[i]][[j]])=="data.frame") && length(Resultats[[i]][[j]])!=0) { 
          if(any(sapply(Resultats[[i]][[j]], class)=="numeric")) Resultats[[i]][[j]][,sapply(Resultats[[i]][[j]], class)=="numeric"] <- lapply(Resultats[[i]][[j]][,sapply(Resultats[[i]][[j]], class)=="numeric"],round,4)
          
          
          addTable(rtf,Resultats[[i]][[j]], row.names=TRUE,col.justify= "C",header.col.justify="C",col.widths=rep(1.5,(1+length(Resultats[[i]][[j]])) ))
          addNewLine(rtf, n=2)
        }
        if(any(class(Resultats[[i]][[j]])=="matrix")) {
          data.frame(Resultats[[i]][[j]]) ->essai
          round(essai,4)->essai
          addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
          addNewLine(rtf, n=2)
        }
        if(any(class(Resultats[[i]][[j]])=="table")) {
          matrix(Resultats[[i]][[j]], ncol=ncol(Resultats[[i]][[j]]))->essai
          data.frame(essai)->essai
          dimnames(Resultats[[i]][[j]])[[2]]->names(essai)
          addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
          addNewLine(rtf, n=2)
        }
        if(any(class(Resultats[[i]][[j]])=="character")) {addText.RTF(rtf,Resultats[[i]][[j]])
          addNewLine(rtf, n=2)}
      }
    }
  }
  
  
  done(rtf)   
  
  data<-get("data", env=env)
  data->Resultats$donnees
  date()->date
  gsub(":",".",date)->date
  dput(Resultats, file=paste(choix, date,".txt"))
  Resultats[[length(Resultats)]]<-NULL
  Resultats$SAUVEGARDE<-paste("les donnees sont sauvegardees dans", getwd())
  
}

#### sÃ©lectionner des observations
selectionO<-function(data=NULL, info=TRUE){options (warn=-1)
  packages<-c("svDialogs")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  if(info==TRUE) print("Il est possible de selectionner sur plusieurs criteres simultanes, impliquant plusieurs variables. Veuillez preciser le nombre
                       de variables sur lesquelles vous desirez appliquer un critere de selection") 
  critere<- dlgInput("Sur combien de variables s appliquent les criteres de selection?", 1)$res
  if(length(critere)==0) critere<-1
  strsplit(critere, ":")->critere
  tail(critere[[1]],n=1)->critere
  as.numeric(critere)->critere
  
  for(i in 1:critere){
    X<-"autres donnees"
    while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
    if(length(data)==0) return(donnees())
    data<-get(data)
    X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees", "annuler"), multiple = FALSE, 
               title="Variable")$res
    if(length(X)==0) X<-"autres donnees"
    if(any(X=="annuler")) return(donnees())}
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% X)[,2]->X
    if(class(data[,X])=="factor"){
      if(info==TRUE) print("Les modalites que vous selectionnez sont celles que vous desirez garder")
      Y<-dlgList(levels(data[,X]), multiple = TRUE, 
                 title="Quelles modalites voulez vous selectionner?")$res
      if(length(Y)==0) return(selectionO())
      data[data[,X]%in% Y,]->data
      factor(data[,X])->data[,X]}else{
        if(info==TRUE) print("Le critere permet de GARDER les observations qui correspondent au critere")
        dlgList(c("superieur a","superieur ou egal a", "inferieur ou egal a", "egal a", "est different de", "entre", "au-dela (avec une limite inferieure et superieure"), 
                preselect=NULL, multiple = FALSE, title="Quel critere voulez vous utiliser ?")$res->choix
        if(length(choix)==0) return(selectionO())
        if(choix=="superieur a"|choix=="inferieur a"|choix=="egal a"|choix=="superieur ou egal a"|choix=="inferieur ou egal a"|choix=="est different de"){
          if(info==TRUE) print("Veuillez preciser la valeur pour selectionner les observations")
          seuil<- dlgInput("Precisez la valeur?", 0)$res
          if(length(seuil)==0) seuil<-0 else {
            strsplit(seuil, ":")->seuil
            tail(seuil[[1]],n=1)->seuil
            as.numeric(seuil)->seuil}} else{seuil.inf<- dlgInput("Limite inferieure?", 0)$res
            while(length(seuil.inf)==0) {print("vous devez preciser la limite inferieure")
              seuil.inf<- dlgInput("Limite inferieure?", 0)$res}
            strsplit(seuil.inf, ":")->seuil.inf
            tail(seuil.inf[[1]],n=1)->seuil.inf
            as.numeric(seuil.inf)->seuil.inf
            seuil.sup<- dlgInput("Limite superieure?", 0)$res
            while(length(seuil.sup)==0) {print("vous devez preciser la limite superieure")
              seuil.sup<- dlgInput("Limite superieure?", 0)$res}
            strsplit(seuil.sup, ":")->seuil.sup
            tail(seuil.sup[[1]],n=1)->seuil.sup
            as.numeric(seuil.sup)->seuil.sup}
        if(choix=="superieur a"){data[data[,X]>seuil,]->data}
        if(choix=="inferieur a"){data[data[,X]<seuil,]->data}
        if(choix=="egal a"){data[data[,X]==seuil,]->data}
        if(choix=="est different de"){data[data[,X]!=seuil,]->data}
        if(choix=="superieur ou egal a"){data[data[,X]>=seuil,]->data}
        if(choix=="inferieur ou egal a"){data[data[,X]<=seuil,]->data}
        if(choix=="entre"){data[data[,X]>=seuil.inf & data[,X]<=seuil.sup,]->data}
        if(choix=="au-dela (avec une limite inferieure et superieure"){data[data[,X]<seuil.inf & data[,X]>seuil.sup,]->data}
      }}
  
  fichier<- dlgInput("Quel nom voulez vous donner au fichier?", "selection")$res
  if(length(fichier)==0) fichier<-"selection"
  strsplit(fichier, ":")->fichier
  tail(fichier[[1]],n=1)->fichier
  assign(x=fichier, value=data, envir=.GlobalEnv)
  View(data, "donnees que vous venez de selectionner")
  Resultats<-paste("les variables selectionnees sont dans", fichier)
  return(Resultats)
}

#### sÃ©lectionner des variables 
SelectionV<-function(data=NULL,info=TRUE){options (warn=-1)
  packages<-c("svDialogs")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  if(info==TRUE) print("Quelles sont les variables a selectionner ?")
  X<- "autres donnees"
  while(any(X=="autres donnees")) {data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
  if(length(data)==0) return(donnees())
  data<-get(data)
  X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees", "annuler"), multiple = TRUE, 
             title="Variable")$res
  if(length(X)==0) X<- "autres donnees"
  if(any(X=="annuler")) return(donnees())}
  listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
  subset(listes, listes[,1] %in% X)[,2]->X
  data[,X]->data
  fichier<- dlgInput("Quel nom voulez vous donner au fichier?", "selection")$res
  if(length(fichier)==0) fichier<-"selection"
  strsplit(fichier, ":")->fichier
  tail(fichier[[1]],n=1)->fichier
  assign(x=fichier, value=data, envir=.GlobalEnv)
  View(data, "donnees que vous venez de selectionner")
  Resultats<-paste("les variables selectionnees sont dans", fichier)
  return(Resultats)
}

#### trier des observations
trier<-function(X, data=NULL, info=TRUE){options (warn=-1) 
  packages<-c("svDialogs")
  # faire en sorte que les donnees triees portent le nom initial des donnees
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  if(info==TRUE)print("Veuillez choisir vos donnees")
  X<-"autres donnees"
  while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  data<-dlgList(data, multiple = FALSE, title="Choix du dataframe")$res
  if(length(data)==0) return(donnees())
  data<-get(data)
  if(info==TRUE)print("Veuillez selectionner la premiere variable de tri")
  X<-dlgList(c(names(data), "autres donnees", "annuler"), multiple = TRUE, title="Variable(s)")$res
  if(length(X)==0) X<-"autres donnees"
  if(any(X=="annuler")) return(donnees())}
  data[do.call("order", data[X]), ]->>donnees.triees
  View(donnees.triees)
  Resultats<-"les donnees sont stockees dans  donnees.triees "
  return(Resultats)}


#### voir un dataframe 
# Il manque modifier un dataframe
voir<-function(){
  data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
  if(length(data)==0) return(donnees())
  View(get(data))
}





################################
####                        ####
####        Analyse         ####
####                        ####
################################
### Analyses

#### function qui permet de choisir l'analyse qu'on veut rÃ©aliser
analyse<-function(){options (warn=-1)
  require(svDialogs)
  dlgList(c("Statistiques descriptives","chi deux","correlations", 
            "t de Student", "analyse de variance et covariance",
            "regressions","regressions logistiques",
            "analyses de facteurs et de composantes",
            "analyse de fiabilite et d accord"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez vous realiser?")$res->choix
  if(length(choix)==0) return(easieR())
  if(choix=="chi deux") chi()->Resultats
  if(choix=="t de Student") test.t()->Resultats
  if(choix=="analyse de variance et covariance") AN.C.OVA()->Resultats
  if(choix=="correlations") choix.corr()->Resultats
  if(choix=="regressions") regressions()->Resultats
  if(choix=="regressions logistiques") regressions.log()->Resultats
  if(choix=="analyses de facteurs et de composantes") factor.an()->Resultats
  if(choix=="analyse de fiabilite et d accord") fiabilite()->Resultats
  if(choix=="Statistiques descriptives") stat.desc()->Resultats
  return(Resultats)
} 


#### Analayse de variance et de covariance 
# test des assumptions pour ancova
# stat descriptives par variables
# effets simples --> package phia et multcomps
# vÃ©rifier tousles contrastes
# ajouter les tailles d'effets sur les contrastes
AN.C.OVA<-function(option=T){
  packages<-c("outliers", "nortest", "psych", "lsr", "ggplot2", "reshape2", "car", "lawstat", 
              "plyr","pgirmess","WRS","svDialogs", "WRS2", "DescTools", "phia", "nlme", "afex")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  .e <- environment()
  Resultats<-list()
  
  type.v<-dlgList(c("Groupes independants", "Mesure repetee", "Covariables"), multiple = TRUE, title="Quel type de variables?")$res
  if(length(type.v)==0) return(analyse())
  if(any(type.v== "Groupes independants") & any(type.v== "Mesure repetee")) plan<-"Plan mixte" else {
    if(all(type.v!="Mesure repetee") & any(type.v== "Groupes independants"))plan<-"Groupes independants" else {
      if(any(type.v=="Mesure repetee") & all(type.v!= "Groupes independants")) plan<-"Mesure repetee"
      else {
        print("il est indispensable d'avoir au minimum des variables a groupes independants ou en mesure repetee",quote=F)
        return(AN.C.OVA())
      }
    }
  }
  
  if(plan=="Mesure repetee"| plan=="Plan mixte") {
    VIR<-"autres donnees"
    print("veuillez choisir la base de donnees",quote=F)
    while(VIR=="autres donnees"){
      data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
      data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
      if(length(data)==0) return(AN.C.OVA())
      data<-get(data)
      writeLines("veuillez selectionner les colonnes correspondant aux modalites de la (des) variables a mesure(s) repetee(s)
                 les mesures doivent etre numeriques ou des integers.")
      VIR<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "annuler", "autres donnees"), multiple = TRUE, title="Modalites a mesures repetees")$res
      if(length(VIR)==0 |length(VIR)==1) {
        print("pour un facteur en mesure repetee, il faut au moins deux colonnes",quote=F)
        VIR<-"autres donnees"}
      if(any(VIR=="annuler")) return(AN.C.OVA())
      listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
      subset(listes, listes[,1] %in% VIR)[,2]->VIR
      if(length(setdiff(sapply(data[,VIR], class), c("numeric","integer")))!=0 ){ print("les mesures doivent etre numeriques ou des integers.",quote=F)
        VIR<-"autres donnees" 
      }
    }
    
    data[complete.cases(data[,VIR]),]->data
    paste("p", 1:length(data[,1]))->data$IDeasy
    melt(data, setdiff(1:length(data),VIR))->longdata
    if(length(VIR)>3) N.facteurs <- dlgInput("Combien de facteurs en mesure repetee ?", 1)$res else N.facteurs<-"1"
    while(length(N.facteurs)=="0"){print("vous devez specifier le nombre de facteurs",quote=F)
      dlgMessage("Vous n avez pas precise le nombre de facteurs en mesure repetee, voulez-vous quitte l analyse?", "yesno")$res->quitte
      if(quitte=="yes") return(AN.C.OVA()) else  N.facteurs <- dlgInput("Combien de facteurs en mesure repetee ?", 1)$res }
    strsplit(N.facteurs, ":")->N.facteurs
    tail(N.facteurs[[1]],n=1)->N.facteurs
    as.numeric(N.facteurs)->N.facteurs
    
    if(N.facteurs==1){list()->noms.F
      list()->modalites
      dlgInput("Nom du facteur 1 ?", "Variable.1")$res->noms.F[[1]]
      if(length(noms.F[[1]])==0) "Variable.1"->noms.F[[1]]
      strsplit(noms.F[[1]], ":")->noms.F[[1]]
      tail(noms.F[[1]][[1]],n=1)->noms.F[[1]]
      colnames(longdata)[length(longdata)-1]<-noms.F[[1]]
      names(data[,VIR])->modalites[[1]] 
      length(VIR)->N.modalites2} else {
        c()->N.modalites2
        while(prod(N.modalites2)!=length(VIR)){list()->noms.F
          list()->modalites
          c()->N.modalites2
          print(paste("vous avez selectionne", length(VIR), "colonnes"),quote=F)
          print("le produit des modalites de chacune des variables doit correspondre au nombre de colonnes selectionnees.",quote=F)
          for(i in 1:N.facteurs) {dlgInput(paste("Nom du facteur",i,  "?"), paste("Variable",i, sep="."))$res->noms.F[[i]]
            if(length(noms.F[[i]])==0) paste("Variable.", i)->noms.F[[i]]
            strsplit(noms.F[[i]], ":")->noms.F[[i]]
            tail(noms.F[[i]][[1]],n=1)->noms.F[[i]]
            N.modalites <- dlgInput(paste("Combien de modalites", noms.F[[i]]), 2)$res 
            if(length(N.modalites)==0) return(AN.C.OVA())
            strsplit(N.modalites, ":")->N.modalites
            tail(N.modalites[[1]],n=1)->N.modalites
            as.numeric(N.modalites)->N.modalites
            c(N.modalites2,N.modalites)->N.modalites2
          }
        }
      }
    
    if(length(noms.F)>1){
      for(i in 1:length(noms.F)){
        dlgForm(setNames(as.list(paste("modalite", 1:N.modalites2[i])), paste("modalite", 1:N.modalites2[i])), 
                paste("Noms des modalites pour", noms.F[[i]]) )$res->modalites[[i]] }  
      
      
      for(i in 1:length(noms.F)){
        if(i==length(noms.F)){a<-1} else {
          a<-prod(N.modalites2[(i+1):length(noms.F)])
        }
        gl(n=N.modalites2[[i]], k=length(data[,1])*a, length=length(data[,1])*prod(N.modalites2), labels=modalites[[i]])->longdata$variable1
        names(longdata)<-c(names(longdata[1:(length(longdata)-1)]),noms.F[[i]])
      }}
    
    type.cont<- dlgList(c("a priori", "a posteriori", "aucun"), preselect="a priori",
                        multiple = FALSE, title="Quels types de contrastes voulez-vous ?")$res
    if(length(type.cont)==0) type.cont<-"aucun"
    if(type.cont=="aucun") type.cont2<-"aucun"
    
    if(type.cont=="a priori") {contrastes<-list()
    print("Si vous choisissez comparaison 2 a 2, cela apparait dans les resultats comme une comparaison a posteriori sans correction",quote=F)
    for (i in 1:length(noms.F)){
      writeLines("Vous pouvez choisir les contrastes que vous souhaitez. NÃ©anmoins les rÃ¨gles concernant l'application des contrastes doivent Ãªtre respectees.
                 Les contrastes peuvent etre specifies manuellement. Choisir specifier les contrastes")
      if(plan=="Mesure repetee"){
        if(i>1) {type.cont2<- dlgList(c("orthogonaux", "orthogonaux inverses", "polynomiaux","specifier les contrastes", "annuler"), preselect=c("orthogonaux"),
                                      multiple = FALSE, title=paste("Quels contrastes pour", noms.F[i],"?"))$res} else{
                                        type.cont2<- dlgList(c("orthogonaux", "orthogonaux inverses", "polynomiaux","comparaison 2 a 2 (deconseille)","specifier les contrastes", "annuler"), preselect=c("orthogonaux"),
                                                             multiple = FALSE, title=paste("Quels contrastes pour", noms.F[i],"?"))$res
                                      }} else {if(i>1) {type.cont2<- dlgList(c("orthogonaux", "orthogonaux inverses", "polynomiaux","comparaison a une ligne de base",
                                                                               "ligne de base inversee","specifier les contrastes", "annuler"), preselect=c("orthogonaux"),
                                                                             multiple = FALSE, title=paste("Quels contrastes pour", noms.F[i],"?"))$res }else{
                                                                               type.cont2<- dlgList(c("orthogonaux", "orthogonaux inverses", "polynomiaux","comparaison a une ligne de base",
                                                                                                      "ligne de base inversee", "comparaison 2 a 2 (deconseille)",
                                                                                                      "specifier les contrastes", "annuler"), preselect=c("orthogonaux"),
                                                                                                    multiple = FALSE, title=paste("Quels contrastes pour", noms.F[i],"?"))$res }
                                        
                                        if(type.cont2=="annuler") return(AN.C.OVA())                                                   }
      if(type.cont2=="orthogonaux") contr.helmert(N.modalites2[i])->contrastes[[i]]
      
      if(type.cont2=="orthogonaux inverses") apply(contr.helmert(N.modalites2[i]), 2, rev)->contrastes[[i]]
      if(type.cont2=="polynomiaux")  contr.poly(N.modalites2[i])->contrastes[[i]]
      if(type.cont2=="comparaison a une ligne de base") { 
        base<- dlgList(modalites[[i]], multiple = FALSE, title="Quelle est la ligne de base?")$res
        which(modalites[[i]]==base)->base
        contr.treatment(N.modalites2[i], base = base, contrasts = TRUE, sparse = FALSE)->contrastes[[i]]
      } 
      if(type.cont2=="comparaison 2 a 2 (deconseille)") {type.cont<-"a posteriori"
      type.cont2<-"none"} 
      if(type.cont2=="specifier les contrastes"){
        contrastes2<-matrix(rep(1,4), nrow=2)
        while(sum(apply(contrastes2, 1, prod))){
          writeLines("Les contrastes doivent respecter les regles de l orthogonalite. Pour annuler, laissez la matrice par defaut")
          
          matrix(rep(0,times=N.modalites2[i], nrow=N.modalites2[i]))->contrastes2
          dimnames(contrastes2)[[1]]<-modalites[[i]]
          dimnames(contrastes2)[[2]]<-"contraste1"
          fix(contrastes2)->contrastes2
          if(all(contrastes2==0)) return(AN.C.OVA())
        }
        contrastes[[i]]<-contrastes2
      }
    }
    
    } 
    if(type.cont=="a posteriori") {
      list()->type.cont2
      dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none", "annuler"), 
              preselect=NULL, multiple = FALSE, title="Type de correction ?")$res->type.cont2[[1]]
      if(type.cont2[[1]]=="annuler") return(AN.C.OVA())
      if(length(type.cont2)==0) type.cont2[[1]]<-"holm"
      if(type.cont2!="Tukey" && length(noms.F)>1) {for(i in 1:(length(noms.F)-1)){
        dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none","annuler"), 
                preselect=NULL, multiple = FALSE, title="Type de correction ?")$res->type.cont3
        if(type.cont3=="annuler") return(AN.C.OVA())
        if(length(type.cont3)==0) type.cont3<-"holm"
        type.cont2[[i+1]]<-type.cont3
        
      }
      }
    }
    
    if(plan=="Mesure repetee" & type.cont=="a priori" & all(type.v!="Covariables")){
      if(length(noms.F)>1){
        #### Effets simples = OK ###
        liste.C<-list()
        for(i in 1:length(contrastes)) {c()->LL2
          for(j in 1:dim(contrastes[[i]])[2]){
            if(i==length(contrastes)) {
              rep(contrastes[[i]][,j], length.out=prod(N.modalites2))->LL
              c(LL2,LL)->LL2
              matrix(LL2, ncol=prod(N.modalites2), nrow=length(LL)/prod(N.modalites2), byrow=TRUE)->LL
              liste.C[[i]]<-LL} else {
                rep(contrastes[[i]][,j], each =prod(N.modalites2[(i+1):length(N.modalites2)]),length.out=prod(N.modalites2))->LL
                c(LL2,LL)->LL2}}
          matrix(LL2, ncol=prod(N.modalites2), nrow=length(LL2)/prod(N.modalites2), byrow=TRUE)->LL
          liste.C[[i]]<-LL  
        }
        EG1<-c()
        for(i in 1:length(contrastes)) {
          if(is.null(dim(contrastes[[i]])[2])) expand.grid(i,1)->EG else {
            expand.grid(i,1:dim(contrastes[[i]])[2])->EG}
          rbind(EG1, EG)->EG1}
        EG1$N<-1:length(EG1[,1])
        nom.C<-c()
        for(i in 1:length(noms.F)){
          for(j in 1: dim(liste.C[[i]])[1]){
            paste(noms.F[[i]], "Cont", j, sep="")->nom.C1
            c(nom.C, nom.C1)->nom.C
          }
        }
        EG1$nom.C<-nom.C
        list()->combinaison
        for(i in 1:(length(contrastes)-1)) {combn(EG1$N, m=1+i)->combinaison[[i]]}
        
        nom.C<-c()
        CV2<-c()
        for(i in 1:length(combinaison)){
          combinaison[[i]]->matrice1
          for(j in 1:dim(matrice1)[2]) {
            matrice1[,j]->colonne
            if(anyDuplicated(EG1[colonne,1])==0){
              liste.C[[EG1[colonne[1],1]]][EG1[colonne[1],2],]->CV1
              EG1[colonne[1],4]->nom.C1  
              for(k in 1:(length(colonne)-1)){
                CV1*liste.C[[EG1[colonne[k+1],1]]][EG1[colonne[k+1],2],]->CV1
                paste(nom.C1, EG1[colonne[k+1],4],sep=":")->nom.C1
              }
              rbind(CV2,CV1)->CV2
              CV1<-c()
              c(nom.C,nom.C1)->nom.C
            }
          }
        }
        c()->CV1
        for(i in 1:length(liste.C)) rbind(CV1, liste.C[[i]])->CV1
        rbind(CV1, CV2)->CV2
        c(EG1$nom.C, nom.C)->nom.C
      }else {
        t(contrastes[[1]])->CV2
        nom.C<-paste("cont", i:dim(contrastes[[1]])[2], sep=".")
        nom.C->dimnames(contrastes[[1]])[[2]]
        dimnames(contrastes[[1]])[[1]]<-levels(longdata[,noms.F[[1]]])
      }
    }
  }
  if(plan=="Groupes independants"|plan=="Plan mixte"){ print("veuillez choisir la base de donnees",quote=F)
    if(plan=="Groupes independants") {
      writeLines("Veuillez choisir vos donnees.")  
      VI<-"autres donnees"
      while(VI=="autres donnees"){ 
        longdata <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
        longdata<-dlgList(longdata, multiple = TRUE, title="Choix du dataframe")$res
        if(length(longdata)==0) return(AN.C.OVA())
        longdata<-get(longdata)
        listes<-data.frame(paste(names(longdata), "(format :", sapply(longdata, class), ")", sep=" "), 1:length(longdata))
        writeLines("Veuillez choisir la ou les variables independantes.") 
        VI<-dlgList(c(paste(names(longdata), "(format :", sapply(longdata, class), ")", sep=" "), "autres donnees", "aucune"), multiple = TRUE,
                    title="Variable-s a groupes independants")$res
        if(length(VI)==0) VI<-"autres donnees"
        if((VI=="aucune") & length(VI)==1) return(AN.C.OVA())
        if((any(VI=="aucune") |any(VI=="autres donnees"))& length(VI)>1){
          if(okCancelBox("Parmi vos choix, il y a aucune ou autres donnees ainsi qu'une variable. Voulez-vous continuer (ok) ou abandonner (annuler) ?")) setdiff(VI, c("aucune", "autres donnees")) else return(AN.C.OVA())
        }
        if(VI!="autres donnees") subset(listes, listes[,1] %in% VI)[,2]->VI
      }
      
      writeLines("Veuillez choisir la variable dependante.")  
      VD<-dlgList(c(paste(names(longdata), "(format :", sapply(longdata, class), ")", sep=" ")), multiple = FALSE, title="Variable dependante")$res
      if(length(VD) == 0L) return(AN.C.OVA())
      subset(listes, listes[,1] %in% VD)[,2]->VD
      longdata[complete.cases(longdata[,c(VI,VD)]),]->longdata
      if(length(VI)==1){
        if(class(longdata[,VI])!="factor") factor(longdata[,VI])->longdata[,VI]
      }else {
        if(any(sapply(longdata[,VI],class)=="factor")) lapply(longdata[,VI],factor)->longdata[,VI] 
      }
      paste("p", 1:length(longdata[,1]))->longdata$IDeasy
      type.cont<- dlgList(c("a priori", "a posteriori", "aucun"), preselect="a priori",
                          multiple = FALSE, title="Quels types de contrastes voulez-vous ?")$res
      if(length(type.cont)==0) type.cont<-"aucun"
      if(type.cont=="aucun") type.cont2<-"aucun"
    }
    
    
    if(plan=="Plan mixte"){
      setdiff(names(longdata), c("IDeasy", "variable", "value", noms.F))->diffs
      writeLines("Veuillez choisir les variables a groupes independants")
      if(length(diffs)==1) {VI<-dlgList(c(paste(diffs, "(format :",class(longdata[,diffs]),")"), "aucune"), multiple = TRUE, 
                                        title="Variables a groupes independants")$res} else {VI<-dlgList(c(paste(diffs, "(format :", sapply(longdata[,diffs], class), ")", sep=" "), "aucun"), multiple = TRUE, 
                                                                                                         title="Variables a groupes independants")$res}
      while(length(VI)==0) {
        if (okCancelBox("Vous n avez pas choisi de variable independante inter participant. Continuer ?")) {
          if(length(diffs)==1) {VI<-dlgList(c(paste(diffs, "(format :",class(longdata[,diffs]),")"), "aucune"), multiple = TRUE, 
                                            title="Variables a groupes independants")$res} else {VI<-dlgList(c(paste(diffs, "(format :", sapply(longdata[,diffs], class), ")", sep=" "), "aucun"), multiple = TRUE, 
                                                                                                             title="Variables a groupes independants")$res}
          
        }else return(AN.C.OVA())
      }
      if(VI=="aucune") plan<-"Mesure repetee"
      listes<-data.frame(paste(names(longdata), "(format :", sapply(longdata, class), ")", sep=" "), 1:length(longdata))
      subset(listes, listes[,1] %in% VI)[,2]->VI 
      longdata[complete.cases(longdata[,VI]),]->longdata
      if(length(VI)==1){
        if(class(longdata[,VI])!="factor")factor(longdata[,VI])->longdata[,VI]
      }else {
        if(any(sapply(longdata[,VI],class)!="factor")) lapply(longdata[,VI],factor)->longdata[,VI]
      }
    }
    
    if(type.cont=="a priori") {contrastes2<-list()
    for (i in 1:length(VI)){
      if(i==1 & !exists("VIR"))  print("Si vous choisissez comparaison 2 a 2, cela apparait dans les resultats comme une comparaison a posteriori sans correction",quote=F)
      if(i>1 | exists("VIR")){ type.cont2<- dlgList(c("orthogonaux", "orthogonaux inverses", "polynomiaux","comparaison a une ligne de base",
                                                      "ligne de base inversee", "specifier les contrastes", "annuler"), preselect=c("orthogonaux"),
                                                    multiple = FALSE, title=paste("Quels contrastes pour", 
                                                                                  names(longdata)[VI[i]],"?"))$res}else{ 
                                                                                    type.cont2<- dlgList(c("orthogonaux", "orthogonaux inverses", "polynomiaux","comparaison a une ligne de base",
                                                                                                           "ligne de base inversee", "comparaison 2 a 2 (deconseille)",
                                                                                                           "specifier les contrastes","annuler"), preselect=c("orthogonaux"),
                                                                                                         multiple = FALSE, title=paste("Quels contrastes pour", 
                                                                                                                                       names(longdata)[VI[i]],"?"))$res}
      if(type.cont2=="annuler") return(AN.C.OVA())
      if(type.cont2=="orthogonaux") contr.helmert(nlevels(longdata[,VI[i]]))->contrastes2[[i]]
      
      if(type.cont2=="orthogonaux inverses") apply(contr.helmert(nlevels(longdata[,VI[i]])), 2, rev)->contrastes2[[i]]
      if(type.cont2=="polynomiaux")  contr.poly(nlevels(longdata[,VI[i]]))->contrastes2[[i]]
      if(type.cont2=="comparaison a une ligne de base") { 
        base<- dlgList(levels(data[, VI[i]]), preselect=levels(longdata[,VI[i]])[1],
                       multiple = FALSE, title="Quelle est la ligne de base?")$res
        which(levels(longdata[, VI[i]])==base)->base
        contr.treatment(levels(longdata[, VI[i]]), base = base, contrasts = TRUE, sparse = FALSE)->contrastes2[[i]]
      } 
      if(type.cont2=="comparaison 2 a 2 (deconseille)") {type.cont<-"a posteriori"
      type.cont2<-"none"} 
      if(type.cont2=="specifier les contrastes"){
        matrix(rep(0,times=nlevels(longdata[,VI[i]])), nrow=nlevels(longdata[,VI[i]]))->contrastes3
        dimnames(contrastes3)[[1]]<-levels(longdata[,VI[i]])
        dimnames(contrastes3)[[2]]<-"contraste1"
        fix(contrastes3)->contrastes3
        contrastes2[[i]]<-contrastes3
      }
    }
    
    } 
    if(type.cont=="a posteriori"){
      if(!exists("type.cont2"))   list()->type.cont2
      dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none", "annuler"), 
              preselect=NULL, multiple = FALSE, title="Type de correction ?")$res->type.cont3
      if(type.cont3=="annuler") return(AN.C.OVA())
      if(length(type.cont3)==0) type.cont2[[length(type.cont2)+1]]<-"holm" else type.cont2[[length(type.cont2)+1]]<-type.cont3
      if(length(VI)>1) {
        for(i in 1:(length(VI)-1)){
          dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none", "annuler"), 
                  preselect=NULL, multiple = FALSE, title="Type de correction ?")$res->type.cont3
          if(type.cont3=="annuler") return(AN.C.OVA())
          if(length(type.cont3)==0) type.cont3<-"holm"
          type.cont2[[i+1]]<-type.cont3
          
        }
      }
    }
  }
  if(plan!="Groupes independants") VD<-"value"
  if(plan=="Groupes independants") {VIR<-NULL
  N.modalites2<-NULL
  data<-NULL
  nettoyees2<-NULL
  noms.F<-NULL}
  if(any(type.v=="Covariables")) {
    if(exists("diffs")) setdiff(diffs, names(longdata)[VI])->diffs
    if(plan=="Mesure repetee") setdiff(names(longdata), c("IDeasy", "variable", "value", noms.F))->diffs
    if(plan=="Groupes independants") setdiff(names(longdata),names(longdata)[VI])->diffs
    writeLines("Veuillez choisir la ou les covariables")
    cov<-dlgList(c(paste(diffs, "(format :",sapply(longdata[, diffs], class),")"), "aucune"), multiple = TRUE, title="Covariable-s?")$res
    if(length(cov) == 0L | cov=="aucune") cov<-NULL
    subset(listes, listes[,1] %in% cov)[,2]->cov
    longdata[complete.cases(longdata[,c(cov)]),]->longdata
  }else cov<-NULL
  
  
  desires<-NULL
  if(option==TRUE) {
    writeLines("Les donnees completes representent l analyse realisee habituellement par les logiciels commerciaux. L analyse sans les valeurs influentes
               est une analyse pour laquelle les valeurs influentes ont ete supprimees des donnees. L identification des valeurs influentes est realisee
               sur la bae du test de Grubbs")
    desires<- dlgList(c("Donnees completes","Identification des outliers", "Donnees sans valeur influente"), 
                      preselect=c("Donnees completes","Identification des outliers", "Donnees sans valeur influente"),
                      multiple = TRUE, title="Quels Resultats voulez-vous obtenir ?")$res}
  if(length(desires)==0) desires<-c("Donnees completes", "Identification des outliers", "Donnees sans valeur influente")
  sauvegarde<-NULL 
  if(option==TRUE) {writeLines("Voulez vous sauvegarder les resultats de l analyse ?")
    dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Voulez-vous sauvegarder?")$res->sauvegarde}
  if(length(sauvegarde)==0) sauvegarde<-FALSE
  SumS<-NULL
  
  
  desires2<-NULL
  if(option==TRUE){     
    writeLines("le modele paramatrique renvoie l anova classique, 
               le non paramatrique calcule soit l anova de Friedman ou de Kruskal Wallis, 
               le modele mixte est le modele lineaire generalise qui reduit la variance d'erreur,
               les statistiques robustes sont des anovas sur des medianes avec ou sans bootstrap")  
    if(!is.null(cov)) {desires2<- dlgList(c("Modele parametrique","Modele mixte - Modele multiniveaux"), 
                                          preselect=c("Modele parametrique", "Modele mixte - Modele multiniveaux"),
                                          multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res } else {
                                            if((exists("VI") && (length(VI)==1 & is.null(noms.F))) | (!is.null(noms.F) && (length(noms.F)==1 & !exists("VI")))) {
                                              desires2<- dlgList(c("Modele parametrique", "Modele non parametrique", "Modele mixte - Modele multiniveaux", "Statistiques robustes - peut prendre du temps"), 
                                                                 preselect=c("Modele parametrique", "Modele mixte - Modele multiniveaux", "Modele non parametrique", "Statistiques robustes - peut prendre du temps"),
                                                                 multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res 
                                            } else { 
                                              if((exists("VI") && (length(VI)==1 & !is.null(noms.F) && length(noms.F)==1)) || (exists("VI") && (length(VI)<4 & is.null(noms.F)))) {
                                                desires2<- dlgList(c("Modele parametrique","Modele mixte - Modele multiniveaux", "Statistiques robustes - peut prendre du temps"), 
                                                                   preselect=c("Modele parametrique", "Modele mixte - Modele multiniveaux", "Statistiques robustes - peut prendre du temps"),
                                                                   multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res 
                                              } else {
                                                desires2<- dlgList(c("Modele parametrique","Modele mixte - Modele multiniveaux"), 
                                                                   preselect=c("Modele parametrique", "Modele mixte - Modele multiniveaux"),
                                                                   multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res 
                                                
                                              }
                                            }
                                          }
  }
  if(length(desires2)==0) desires2<-c("Modele parametrique","Modele mixte - Modele multiniveaux")
  ES<-NULL
  if(option==TRUE){
    if(any(desires2=="Modele parametrique")){
      writeLines("la taille d'effet la plus frequente est le eta carre partiel - pes - celle qui est la plus precise est le eta carre generalise - ges")
      ES<- dlgList(c("ges", "pes"), 
                   preselect=c("ges"),
                   multiple = TRUE, title="Quelle taille d effet voulez-vous  ?")$res }}
  if(length(ES)==0) ES<-"ges"
  if(option==TRUE) {
    if(any(desires2=="Modele parametrique")) {writeLines("Il existe plusieurs maniere de calculer la somme des carres. Le choix par defaut des logiciels commerciaux est une somme des carres
                                                         de type 3, mettant la priorite sur les interactions plutot que sur les effets principaux.")
      SumS<- dlgList(c(2,3), preselect=3,multiple = FALSE, title="Quels sommes des carres voulez-vous utiliser ?")$res
      as.numeric(SumS)->SumS
    }}
  if(length(SumS)==0) SumS<-3
  
  
  if(plan=="Mesure repetee") VI<-NULL
  if(plan!="Mesure repetee" | !is.null(cov)) CV2<-NULL
  
  anova2<-function(VD=NULL, VI=NULL, VIR=NULL, noms.F, longdata, data=NULL, type.cont,type.cont2,type.cont2b, plan, SumS,N.modalites2, CV2,
                   desires2, cov=NULL)  {
    
    list()->Resultats
    VD->VD2
    if(plan!="Groupes independants")  paste0(VD," ~ ")->VD else  paste0(names(longdata)[VD]," ~ ")->VD
    if(!is.null(cov)) { for(i in 1:length(cov)) {paste0(VD, names(longdata)[cov[i]],"+")->VD}}
    
    if(!is.null("VI"))  {pred.ind<-names(longdata)[VI[1]]  
    if(length(VI)>1) {
      for(i in 1:(length(VI)-1)){ paste(pred.ind, "*",names(longdata)[VI[1+i]])->pred.ind}}}
    
    if(!is.null("noms.F"))  {
      principal<-noms.F[[1]]
      erreur<-paste0("+Error(", noms.F[[1]])
      paste0("~1|IDeasy/", noms.F[[1]])->random
      if(length(noms.F)>1) {for(i in 1:(length(noms.F)-1)){
        paste(principal, "*",noms.F[[i+1]])->principal
        paste(erreur, "*", noms.F[[i+1]])->erreur
        paste0(random, "/", noms.F[[i+1]])->random
      }
      }
      paste(principal, erreur,"|IDeasy)")->pred.rep
    }
    if(length(pred.ind)!=0 & length(principal)!=0) paste(pred.ind, "*",pred.rep)->predicteurs else {
      if(length(pred.ind)!=0 & length(principal)==0) paste0(pred.ind,"+Error(1|IDeasy)")->predicteurs else pred.rep->predicteurs
    }
    as.formula(paste0(VD,predicteurs))->modele  
    modele->Resultats$"Modele teste"
    
    if(plan=="Groupes independants"){
      psych::describeBy(longdata[,VD2], longdata[ ,names(longdata[VI])] ,mat=TRUE,type=3)->Resultats$"statistiques descriptives"}
    if(plan=="Mesure repetee"){
      psych::describeBy(longdata[,VD2], longdata[ ,unlist(noms.F)] ,mat=TRUE,type=3)->Resultats$"statistiques descriptives"}
    if(plan=="Plan mixte"){
      psych::describeBy(longdata[,VD2], longdata[ ,c(names(longdata[VI]),unlist(noms.F))] ,mat=TRUE,type=3)->Resultats$"statistiques descriptives"}
    
    if(any(Resultats$"statistiques descriptives"$n<2)) {
      "il y a moins de 3 observations pour un des groupes"-> Resultats$"information"
      return(Resultats)
    }
    
    if(any(desires2=="Modele parametrique") | any(desires2=="Modele mixte - Modele multiniveaux")){
      if(length(pred.ind)!=0 & length(principal)!=0) paste(pred.ind, "*",principal)->predicteurs else {
        if(length(pred.ind)!=0 & length(principal)==0) pred.ind->predicteurs else principal->predicteurs}
      lm(as.formula(paste0(VD,predicteurs)),na.action=na.exclude, data=longdata)->lm.r1
      resid(lm.r1)->longdata$residu
      assign(x="longdata", value=longdata, envir=.e)
      if(length(longdata$residu)<5000){
        shapiro.test(longdata$residu)->Shapiro_Wilk # realise le Shapiro-Wilk
        lillie.test(longdata$residu)->Lilliefors  # realise le Lilliefors
        round(data.frame(Shapiro_Wilk$statistic,Shapiro_Wilk$p.value, Lilliefors$statistic, Lilliefors$p.value),4)->normalite
        names(normalite)<-c("W de Shapiro-Wilk", "valeur.p SW", "D de Lilliefors", "valeur.p Llfrs")
        dimnames(normalite)[1]<-" "
        format(normalite, width = max(sapply(names(normalite), nchar)), justify = "centre")->Resultats$"Tests de normalite"}
      h<-hist(longdata$residu, breaks=10, density=10, col="black", xlab="residus", main="Distribution des residus") 
      xfit<-seq(min(longdata$residu),max(longdata$residu),length=40) 
      yfit<-dnorm(xfit,mean=mean(longdata$residu),sd=sd(longdata$residu)) 
      yfit <- yfit*diff(h$mids[1:2])*length(longdata$residu) 
      lines(xfit, yfit, col="darkblue", lwd=2) 
      if(any(desires2=="Modele parametrique")){
        if(!is.null(VI)){
          if(is.numeric(VD2)) VD2 else VD2<-which(names(longdata)==VD2)
          paste0(names(longdata)[VD2], "~",names(longdata)[VI[1]])->modele2
          if(length(VI)>1) {for(i in 1:(length(VI)-1)){ paste(modele2, "*",names(longdata)[VI[i+1]])->modele2}}
          leveneTest(as.formula(modele2),data=longdata)->Levene # test de Levene pour homogeneite des variances
          round(unlist(Levene)[c(1,2,3,5)],3)->Levene
          names(Levene)<-c("ddl1","ddl2","F","valeur.p")
          Levene->Resultats$"Test de Levene verifiant l homogeneite des variances"
        }
        if(any(Resultats$"statistiques descriptives"$sd==0)) Resultats$Avertissement<-"La variance d au moins un groupe vaut 0. Les resultats risquent d etre considerablement biaises"  
        options(contrasts=c("contr.sum","contr.poly"))
        if(!is.null(cov)) factorize<-FALSE else factorize<-TRUE
        aov_4(as.formula(modele),data=longdata, es_aov=ES, type=SumS,factorize=factorize)->aov.out
        summary(aov.out)->aov.out2 
        nice(aov.out, correction="none", intercept=T, es=ES,type=SumS)->aov.out
        names(aov.out)<-c("Effet","ddl.num, ddl.denom", "CME", "F", names(aov.out)[5], "valeur.p" )
        format(aov.out, width = max(sapply(names(aov.out), nchar)), justify = "centre")->aov.out
        format(names(aov.out), justify = "centre")->names(aov.out)
        
        if(!is.null(N.modalites2) & any(N.modalites2>2)) aov.out2$sphericity.test->Resultats$"test de Mauchly testant la sphericite de la matrice de covariance"
        aov.out->Resultats$"Analyse principale"
        if(!is.null(N.modalites2) & any(N.modalites2>2)) {data.frame(aov.out2$pval.adjustments)->GG.HF
          names(GG.HF)<-c("GG.eps", "GG.valeur.p","HF.eps", "HF.valeur.p")
          GG.HF->Resultats$"Correction de Greenhouse-Geisser et de  Hyunh-Feldt"}
        if(length(VI)==1 & is.null(noms.F) & is.null(cov)) {oneway.test(as.formula(paste(VD,names(longdata)[VI])),data=longdata)->Welch
          round(data.frame(Welch$statistic,Welch$parameter[1],Welch$parameter[2],Welch$p.value),4)->Welch
          names(Welch)<-c("F","ddl.num","ddl.denom","valeur.p")
          Welch->Resultats$"Anova avec correction de Welch pour variances heterogenes"
        }  
      }
      
      
      
      if(type.cont=="a priori"){
        
        if(!is.null(noms.F)){
          for(i in 1:length(noms.F)){
            contrastes[[i]]->contrasts(longdata[,noms.F[[i]]])
            dimnames(contrastes[[i]])[[2]]<-paste("contraste", 1:ncol(contrastes[[i]]), sep=".")
          }
          contrastes->Resultats$"Contrastes a priori"$"Matrice de coefficients variables intra" 
        }
        
        if(!is.null(VI)){
          for(i in 1:length(VI)) {dimnames(contrastes2[[i]])[[1]]<-levels(longdata[,VI[i]])
          dimnames(contrastes2[[i]])[[2]]<-paste("contraste", 1:ncol(contrastes2[[i]]), sep=".")
          contrastes2[[i]]->contrasts(longdata[,VI[i]])}
          contrastes2->Resultats$"Contrastes a priori"$"Matrice de coefficients des variables inter"    }    
        
        if(plan=="Groupes independants"){
          lm(as.formula(paste0(VD,predicteurs)),na.action=na.exclude, data=longdata)->lm.r1  
          round(summary(lm.r1)$coefficients,4)->Resultats$"Contrastes a priori"$"Table des contrastes"}
        if(plan=="Mesure repetee" & is.null(cov)){
          Table.contrastes<-c()
          for(k in 1:length(CV2[,1])){
            rep(0, length(data[ ,1]))->data$contraste
            for (i in 1:length(VIR)){data$contraste<-data[ ,VIR[i]]*CV2[k,i]+ 
              data$contraste}
            t.test(data$contraste, mu = 0, paired = FALSE, conf.level = 0.95)->C1
            rbind(Table.contrastes,c(C1$estimate, C1$parameter, C1$statistic, C1$p.value))->Table.contrastes}
          round(Table.contrastes,4)->Table.contrastes
          data.frame(Table.contrastes)->Table.contrastes  
          names(Table.contrastes)<-c("Estimation", "ddl", "t", "valeur.p")
          dimnames(Table.contrastes)[[1]]<-nom.C
          Table.contrastes->Resultats$"Table des contrastes imitant les logiciels commerciaux"
          
        }
      }
      if(plan=="Plan mixte" | any(desires2== "Modele mixte - Modele multiniveaux")){
        
        if(plan=="Groupes independants") {
          anova(gls(as.formula(paste0(VD,predicteurs)),na.action=na.exclude, data=longdata, method="REML"))->Resultats$"modele multiniveaux - methode REML"}
        if(plan=="Mesure repetee"){ paste0(VD, principal)->modele.lme
          lme(as.formula(modele.lme), random=as.formula(random), data=longdata, method="REML")->modele.lme
          anova(modele.lme)->Resultats$"modele lineaire mixte - methode REML"
          if(type.cont=="a priori"){  round(summary(modele.lme)$tTable,4)->tableT
            data.frame(tableT)->tableT
            names(tableT)<-c("estimateur", "erreur.st", "ddl","valeur.t", "valeur.p")
            tableT->Resultats$"Table des contrastes sur le modele lineaire multiniveau"}
        }
        if(plan=="Plan mixte" & (type.cont=="a priori"|any(desires2== "Modele mixte - Modele multiniveaux"))) {
          paste0(VD, pred.ind, "*", principal)->modele.lme
          paste0("~1|IDeasy/", noms.F[[1]])->random
          lme(as.formula(modele.lme), random=as.formula(random), data=longdata, method="REML")->modele.lme
          if(any(desires2=="Modele mixte - Modele multiniveaux")) anova(modele.lme)->Resultats$"modele lineaire mixte - methode REML"
          if(type.cont=="a priori"){round(summary(modele.lme)$tTable,4)->tableT
            data.frame(tableT)->tableT
            names(tableT)<-c("estimateur", "erreur.st", "ddl","valeur.t", "valeur.p")
            tableT->Resultats$"Table des contrastes sur le modele lineaire multiniveau"
            
          }
        } 
        
      }
      
      if(type.cont=="a posteriori"){
        if(plan=="Mesure repetee"|plan=="Plan mixte"){
          list()[1:length(noms.F)]->comparaisons3
          
          for(i in 1:length(noms.F)){
            noms.F[[i]]->names(comparaisons3)[i]
            list()->comparaisons2
            comparaisons2$Correction<-type.cont2[[i]]
            pairwise.t.test(longdata[,VD2],longdata[,noms.F[[i]]], paired=T,p.adj=type.cont2[[i]])$p.value->comparaisons2$"table des valeurs p"
            comparaisons2->comparaisons3[[i]]
          }
          Resultats$"Comparaisons a posteriori des variables intra"<-comparaisons3 
        }
        
        if(plan=="Groupes independants" | plan=="Plan mixte"){
          list()[1:length(VI)]->comparaisons
          names(longdata)[VI]->names(comparaisons)
          length(noms.F)->a 
          for(i in 1:(length(VI))){
            list()->comparaisons2
            comparaisons2$Correction<-type.cont2[[i+a]]
            pairwise.t.test(longdata[,VD2],longdata[,VI[i]],p.adj=type.cont2[[i+a]])$p.value->comparaisons2$"table des valeurs p"
            comparaisons2->comparaisons[[i]]
          }
          Resultats$"Comparaisons a posteriori des variables inter"<-comparaisons 
          
        }
      }
    }
    
    if(any(desires2=="Modele non parametrique" )){
      if(!is.null(VI)){
        kruskal.test(as.formula( paste0(names(longdata)[VD2], "~",names(longdata)[VI[1]])), data = longdata)->KW
        round(data.frame(KW$statistic,KW$parameter,KW$p.value),4)->KW
        names(KW)<-c("H","ddl","valeur.p")
        KW->Resultats$"Test de Kruskal-Wallis"
        if(type.cont2!="Comparaison a une ligne de base") kruskalmc(as.formula(paste0(VD,names(longdata)[VI[1]])), data=longdata)->Resultats$"Test de Kruskal-Wallis - Comparaison deux a deux"
        if(type.cont2=="Comparaison a une ligne de base") kruskalmc(as.formula(paste0(VD,names(longdata)[VI[1]])), data=longdata, cont='two-tailed')->Resultats$"Test de Kruskal-Wallis - Comparaison a une ligne de base"
        
      }else{
        friedman.test(as.matrix(data[,VIR]))->friedman
        round(data.frame(friedman$statistic,friedman$parameter,friedman$p.value),4)->friedman
        names(friedman)<-c("chi.deux","ddl","valeur.p")
        friedman->Resultats$"Analyse non parametrique"$"Anova de Friedman"
        
        friedmanmc(as.matrix(data[,VIR]))->Resultats$"Comparaison 2 a 2 pour ANOVA de Friedman"
        
      }
    }
    
    if(any(desires2=="Statistiques robustes - peut prendre du temps")){
      if(length(VI)==1 & is.null(noms.F)){
        if(!exists("contrastes")) Contrasts(levels(longdata[,VI]))->contrastes
        split(longdata[,VD2], longdata[,VI])->robuste
        unlist(WRS::med1way(robuste,iter = 1000))->mediane
        names(mediane)<-c("Test", "Valeur.critique","valeur.p")
        round(mediane,4)->Resultats$"Anova basee sur les medianes"$"Analyse principale"
        WRS::medpb(robuste,alpha=.05,nboot=1000,con=contrastes,bhop=FALSE)->cont
        #WRS::medpb(robuste,alpha=.05,nboot=1000,con=contrastes[[1]],bhop=FALSE)->cont
        dimnames(cont$output)[[2]]<-c("Numero.contraste","Valeur.contraste",
                                      "valeur.p","p.critique.corrigee","lim.inf.IC","lim.sup.IC")
        cont->Resultats$"Anova basee sur les medianes"$"Contrastes"
        WRS2::t1way(as.formula(paste0(VD, names(longdata)[VI])), tr=.2,data=longdata)->AR1
        WRS2::t1waybt(as.formula(paste0(VD, names(longdata)[VI])), tr=.2, nboot=2000,data=longdata)->AR2
        data.frame(AR1[[2]],AR1[[3]],AR1[[1]],AR2[[2]],AR2[[3]],AR2[[4]], AR2[[5]])->AR1
        names(AR1)<-c("ddl.num","ddl.denom","Stat","valeur.p","Var.expliquee","Taille.effet","Nombre.bootstrap" )
        AR1->Resultats$"Anova basee sur les moyennes tronquees"$"Analyse principale"
        "Les probabilites et les IC sont estimes sur la base d'un bootsrap"->Resultats$"Anova basee sur les moyennes tronquees"$"Information"
        WRS::lincon(robuste, tr=.2, con=contrastes[[1]])->cont
        WRS::mcppb20(robuste, tr=.2, nboot=2000, con=contrastes)->cont2
        data.frame(cont$psihat[,2],cont$test[,4],cont$test[,5],cont$test[,2],cont$test[,3],cont2$psihat[,4],cont2$psihat[,5],cont2$psihat[,6])->cont
        names(cont)<-c("Valeur.contraste","erreur.standard","ddl","test","seuil.critique","lim.inf.IC","lim.sup.IC","valeur.p")
        cont->Resultats$"Anova basee sur les moyennes tronquees"$"Contrastes"
        cont2[3]->Resultats$"Anova basee sur les moyennes tronquees"$"Coefficients des contrastes"}
      
      
      if(length(VI)==2 & is.null(noms.F)) { 
        
        WRS2::t2way(as.formula(paste0(names(longdata)[VD2], "~",names(longdata)[VI[1]],"*",names(longdata)[VI[2]])), data=longdata, tr = 0.2)->T2
        round(matrix(unlist(T2[1:6]), ncol=2, byrow=T),4)->T2
        dimnames(T2)[[2]]<-c("valeur", "valeur.p")
        c(names(longdata[,VI]), paste(names(longdata[,VI])[1],":",names(longdata[,VI])[2]))->dimnames(T2)[[1]]
        T2->Resultats$"ANOVA sur moyennes tronquees a 0.2"$"Analyse principale"
        WRS2::pbad2way(as.formula(paste0(names(longdata)[VD2], "~",names(longdata)[VI[1]],"*",names(longdata)[VI[2]])), data=longdata, est = "mom", nboot = 599)->Resultats$"ANOVA sur M estimator"$"Analyse principale"
        WRS2::pbad2way(as.formula(paste0(names(longdata)[VD2], "~",names(longdata)[VI[1]],"*",names(longdata)[VI[2]])), data=longdata, est = "median", nboot = 599)->Resultats$"ANOVA sur medianes"$"Analyse principale"
        model.matrix(mcp2a(as.formula(paste0(names(longdata)[VD2], "~",names(longdata)[VI[1]],"*",names(longdata)[VI[2]])), data=longdata, est = "median"))->Resultats$"Comparaisons post hoc"$"Matrice de contrastes"
        WRS2::mcp2a(as.formula(paste0(names(longdata)[VD2], "~",names(longdata)[VI[1]],"*",names(longdata)[VI[2]])), data=longdata, est = "mom", nboot = 599)->Resultats$"Comparaisons post hoc"$"ANOVA sur M estimator"
        WRS2::mcp2a(as.formula(paste0(names(longdata)[VD2], "~",names(longdata)[VI[1]],"*",names(longdata)[VI[2]])), data=longdata, est = "median", nboot = 599)->Resultats$"Comparaisons post hoc"$"ANOVA sur medianes"
      }
      
      
      if(length(VI)==3 & is.null(noms.F))  t3way(as.formula(paste0(names(longdata)[VD2], "~",names(longdata)[VI[1]],"*",
                                                                   names(longdata)[VI[2]],"*",names(longdata)[VI[3]])), data=longdata, tr = 0.2)->Resultats$'Anova sur les moyennes tronquees'   
      
      if(length(noms.F)==1 & is.null(VI)){
        rmanova(longdata$value,longdata[,noms.F[[1]]] ,longdata$IDeasy)->ANOVA.tr
        data.frame(ANOVA.tr$test,ANOVA.tr$df1, ANOVA.tr$df2,ANOVA.tr$p.value)->ANOVA.tr
        names(ANOVA.tr)<-c("Valeur.test", "ddl1","ddl2","valeur.p")
        ANOVA.tr->Resultats$"Statistiques robustes"$"Anova sur moyennes tronquees a 20%"
        if((nlevels(longdata[,length(longdata)-1]))>2) {rmmcp(longdata$value,longdata[,length(longdata)-1] 
                                                              ,longdata$IDeasy)->Resultats$"Statistiques robustes"$"Comparaisons 2 a 2 sur moyennes tronquees a 20%"
        }
        rmanovab(longdata$value,longdata[,noms.F[[1]]] ,longdata$IDeasy)->ANOVA.tr
        data.frame(ANOVA.tr[[1]],ANOVA.tr[[2]], if(ANOVA.tr[[1]]<ANOVA.tr[[2]]){"non significatif"}else"significatif")->ANOVA.tr
        names(ANOVA.tr)<-c("Valeur.test", "Valeur critique","significativite")
        ANOVA.tr->Resultats$"Statistiques robustes"$"Anova sur moyennes tronquees a 20% avec bootstrap"
        if((nlevels(longdata[,noms.F[[1]]]))>2) {
          pairdepb(longdata$value,longdata[,noms.F[[1]]] ,
                   longdata$IDeasy)->Resultats$"Statistiques robustes"$"Comparaisons 2 a 2 sur moyennes tronquees a 20% avec bootsrap"
        }
      } 
      
      if(length(VI)==1 & length(noms.F)==1){
        as.formula(paste0("value~", noms.F[[1]],"*", names(longdata)[VI]))->modeleR
        WRS2::tsplit( modeleR, IDeasy, data=longdata, tr = 0.2)->Resultats$'Anova sur les moyennes tronquees' # anova mixte sur moyennes tronquÃÂÃÂ©es 
        WRS2::sppba(modeleR, IDeasy, data=longdata, est = "mom", avg = TRUE, nboot = 500, MDIS = FALSE)->MoMa # anova sur moyenne oÃÂÃÂ¹ on enlÃÂÃÂ¨ve les valeurs aberrantes avec bootstrap pour l'effet de A
        WRS2::sppbb(modeleR, IDeasy, data=longdata, est = "mom", nboot = 500)->MoMb# anova avec bootstrap pour l'effet de B
        WRS2::sppbi(modeleR, IDeasy, data=longdata, est = "mom", nboot = 500)->MoMi # # anova avec bootstrap pour l'effet d'interaction
        data.frame(c(names(longdata)[VI],noms.F[[1]],"inteaction"), c(MoMa$p.value,MoMb$p.value, MoMi$p.value) )->MoM
        names(MoM)<-c("effet", "valeur.p")
        MoM->Resultats$'Anova sur l estimateur modifie de localisation de Huber'
      }
      
      
    }
    return(Resultats)
  }   
  
  anova2(VD=VD, VI=VI, VIR=VIR, noms.F=noms.F, longdata=longdata, data=data, 
         type.cont=type.cont,type.cont2=type.cont2,plan=plan, SumS=SumS,N.modalites2=N.modalites2, CV2=CV2,
         desires2=desires2, cov=cov)->complet
  if(any(desires=="Donnees completes")){
    complet->Resultats$"Donnees completes"}
  
  # print(ggplot(data, aes(data[ ,VI], data[ ,VD), environment = .e)+labs(x=names(data[,VI]), y=names(data[,VD]))+stat_summary(fun.y=mean, geom="bar", fill="Black", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2))
  # identification de valeurs aberrantes a l aide du test de Grubbs et stockage de l info dans l objet outliers
  
  if(any(desires=="Identification des outliers")|any(desires=="Donnees sans valeur influente")) { 
    
    valeurs.influentes(X="residu", critere="Grubbs",z=3.26, data=longdata)->influentes
    
    if(any(desires=="Identification des outliers")) influentes->Resultats$"Valeurs influentes"
    if(any(desires=="Donnees sans valeur influente")){ 
      if(!is.null(influentes$"observations influentes"$IDeasy)){
        setdiff(longdata$IDeasy,influentes$"observations influentes"$IDeasy)->diffs
        longdata[which(longdata$IDeasy%in%diffs), ]->nettoyees
        
        
        if(plan=="Mesure repetee"){
          data[which(data$IDeasy%in%diffs),]->nettoyees2
        }
      }
      else{
        nettoyees<-longdata
        if(plan=="Mesure repetee") {
          nettoyees2<-data
        }
      }
    }
    anova2(VD=VD, VI=VI, VIR=VIR, noms.F=noms.F, longdata=nettoyees, data=nettoyees2, 
           type.cont=type.cont,type.cont2=type.cont2,plan=plan, SumS=SumS,N.modalites2=N.modalites2, CV2=CV2,
           desires2=desires2, cov=cov)->Resultats$"Donnees sans valeur influente"
  }
  if(sauvegarde==T) save(Resultats=Resultats ,choix ="anova", env=.e)
  return(Resultats)
}

### chi deux #### 
# fonction OK 

chi<-function(X=NULL, Y=NULL, Effectifs=NULL, save=F, p=NULL, choix=NULL, data=NULL, info=TRUE){options (warn=-1)
  c("svDialogs","epitools", "lsr")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  if(!is.null(X) && class(X)!="character") {{data.frame(X)->datab
    1:length(datab)->X}
    if(!is.null(Y) && class(Y)!="character") {cbind(datab, data.frame(Y))->data
      rm(datab)
      (length(X)+1):length(data)->Y} else {datab->data
        rm(datab)}
    if(!is.null(Effectifs) && class(Effectifs)!="character") {Effectifs->data$Effectifs
      Effectifs<-length(data)}}
  .e <- environment()
  if(!is.null(X) && class(X)=="character") match(X,names(data))->X  
  if(!is.null(Y) && class(Y)=="character") match(Y,names(data))->Y 
  if(!is.null(Effectifs) && class(Effectifs)=="character") match(Effectifs,names(data))->Effectifs
  
  if(is.null(X) ||any(c(X,Y, Effectifs)>ncol(data))) {
    if(info==T) print("Les chi ne peuvent etre realise que sur des variables categorielles. Quel type de chi deux desirez vous realiser ?")
    choix<- dlgList(c("AJUSTEMENT", "INDEPENDANCE", "TEST de McNemar"), preselect="INDEPENDANCE", multiple = FALSE, title="Type de khi.deux")$res
    if(length(choix)==0) return(analyse())
    if(info==T) print("Dans le tableau de contingence, quel facteur categoriel doit etre utilise pour les entetes des lignes ?
                      Attention : une variable ne peut Ãªtre utilisee Ã  la fois en entete de lignes et de colonnes.")
    X<-"autres donnees"
    while(any(X=="autres donnees") )     {data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
    if(length(data)==0) return(analyse())
    data<-get(data)
    X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees", "annuler"), multiple = TRUE, 
               title="Entetes de lignes")$res
    if(length(X)==0) X<-"autres donnees"
    if(any(X=="annuler")) return(chi())}
    
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% X)[,2]->X
    if(choix!="AJUSTEMENT"){if(info==T) print("Dans le tableau de contingence, quel facteur categoriel doit etre utilise pour les entetes des colonnes ?")
      Y<-dlgList(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), multiple = TRUE, title="Entetes de colonnes")$res
      if(length(Y) == 0L) return(chi())
      subset(listes, listes[,1] %in% Y)[,2]->Y}
    if(info==T) print("Faut il ponderer l analyse par une variable effectif ? Si vous n avez pas de variable effectif, choisissez aucune")
    Effectifs<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "aucune"), multiple = TRUE, title="Variable  Effectifs ")$res
    if(length(Effectifs) == 0L) Effectifs<-NULL
    if (Effectifs=="aucune") Effectifs<-NULL                                                
    if (!is.null(Effectifs)) subset(listes, listes[,1] %in% Effectifs)[,2]->Effectifs
    if(choix=="AJUSTEMENT") {if(length(X)>1) return("On ne peut faire qu une analyse a la fois pour le chi.deux d ajustement")
      while(is.null(p) || sum(p)!=1 ||length(p)!=nlevels(data[,X])){
        if(dlgMessage("La somme des probabilites n est pas egal a 1 ou le nombre de probabilites ne correspond pas au nombre de modalites de la variable.Veuillez entrer un vecteur de probabilites valide", 
                      "okcancel")$res=="cancel") return(analyse()) else{
                        dlgForm(setNames(as.list(rep(1/nlevels(data[,X]),times=nlevels(data[,X]))), levels(data[,X])), "Vecteur des probabilites. Attention : ne pas entrer des fractions")$res->niveaux
                        stack(niveaux)[,1]->p} } }                                                                    
    
    save<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = TRUE, title="Enregistrer les Resultats ?")$res
    if(length(save)==0) save<-FALSE
  }
  Resultats<-list()  
  
  if(!is.null(Effectifs) && class(data[,Effectifs])=="factor")return("La variable effectif n est pas numerique") 
  if (!is.null(Effectifs) && any((data[,Effectifs]/round(data[,Effectifs],0))!=1)) {
    Resultats$'Avertissement sur la variable Effectifs'<-"Les effectifs ne peuvent contenir de decimales. Ce n est pas le cas dans cette analyse. La variable  effectifs  n a pas ete prise en compte."
    NULL->Effectifs }
  
  if((!is.null(choix) && choix=="AJUSTEMENT")| is.null(Y)){
    if (is.null(p)) rep(1/nlevels(data[,X]),times=nlevels(data[,X]))->p
    if(!is.null(Effectifs)){tapply(data[,Effectifs], list(data[,X]),sum,na.rm=TRUE)->tab
      rbind(tab, p*sum(data[,Effectifs]))->Distribution} else {
        table(data[,X])->tab
        rbind(tab, p, sum(tab)*p)->Distribution}
    chisq.test(tab, p=p)->chi  
    data.frame(chi.deux=chi$statistic, ddl=chi$parameter, valeur.p=chi$p.value)->chi
    dimnames(Distribution)[[1]]<-c("Observes", "probabilites","Theoriques")
    Resultats$"Tableau de synthese"<-Distribution 
    Resultats$"chi.deux d ajustement"<-chi 
    if(save){
      data->Resultats$donnees
      date()->date
      gsub(":",".",date)->date
      dput(Resultats, file=paste("chi carre d ajustement de la variable", names(data)[X[i]], date,".txt"))
      Resultats[[length(Resultats)]]<-NULL
      Resultats$SAUVEGARDE<-paste("les donnees sont sauvegardees dans", getwd())}
    ### Obtenir les Resultats
    return(Resultats) }
  
  if(!is.null(Y)){
    if(any(sapply(data[,c(X,Y)], class)!="factor") &&
       dlgMessage(paste("Certaines variables (", names(data)[c(X,Y)[which(sapply(data[,c(X,Y)], class)!="factor")]],") ne sont pas des facteurs categoriels, 
                        faut-il les convertir? (Attention, temps de calcul tres long si beaucoup de modalites)", sep=" "),"yesno")$res=="no") return("vous avez abandonne l analyse") else{
                          data[, c(X,Y)] <- lapply(data[, c(X,Y)], as.factor)}
    c()->Resultats2
    for(i in 1 : length(expand.grid(X,Y)[,1]) ){
      Resultats$"NOM DE L ANALYSE"<-paste("Resultats du chi.deux entre la variable", names(data)[expand.grid(X,Y)[i,1]], "et la variable", names(data)[expand.grid(X,Y)[i,2]],sep=" ")
      if(length(expand.grid(X,Y)[,1])>1) Resultats$"Avertissement alpha"<-paste("vous multipliez l erreur de 1e espece. Le risque de commettre une erreur de 1e espece est de", 100*(1-0.95^length(expand.grid(X,Y)[,1])), "%", sep=" ")
      # permet de creer un tableau de contingence en prenant en compte les effectifs
      if (is.null(Effectifs)) tab<-table(data[,expand.grid(X,Y)[i,1]],data[ ,expand.grid(X,Y)[i,2]])else {
        tab<-tapply(data[,Effectifs],list(data[,expand.grid(X,Y)[i,1]],data[,expand.grid(X,Y)[i,2]]),sum,na.rm=TRUE) 
        tab <- as.data.frame(tab)
        tab[is.na(tab)] <- 0
        as.matrix(tab)->tab}
      table.margins(tab)->Resultats$"Effectifs Observes"
      if(!is.null(choix) && choix=="INDEPENDANCE"){
        # Verification des conditions d application
        chisq.test(tab)$expected->Resultats$"Effectifs attendus"
        
        # fais les differentes analyses
        chisq.test(tab, correct = FALSE)->khi_sans_Yates # khi carre d independance sans correction de Yates
        cramersV(tab, correct = FALSE)^2->phi  # calcul du phi carre de Cramer, mesure de la taille de l effet
        fisher.test(tab)->fisher # Fisher exact test
        
        # Creation d un tableau pour soigner la presentation 
        data.frame(khi_sans_Yates$statistic, khi_sans_Yates$parameter, khi_sans_Yates$p.value, phi, fisher$p.value)->resultats # combine les differents Resultats
        names(resultats)<-c("chi.deux", "ddl", "valeur.p", "phi.deux Cramer", "Fisher Exact Test") # ajoute le nom aux variables
        round(resultats,4)->Resultats$"analyse principale"
        
        # Calcul de l odd ratio (ou rapport de cote)
        as.matrix(tab)->tab
        if(length(tab[1,])>2 & length(tab[,1])==2) {
          x<-tab[1,] 
          y<-tab[2,]
          cbind(x,y)->tab2
          oddsratio.wald(x=tab2, conf.level = 0.95,rev = c("neither"),
                         correction = FALSE,verbose = FALSE)->Odd_Ratio
          Odd_Ratio$measure->Resultats$"Odd ratio"} else{
            if(length(tab[1,])==2 & length(tab[,1])>=2){
              oddsratio.wald(x=tab,conf.level = 0.95,
                             rev = c("neither"),correction = FALSE,verbose = FALSE)->Odd_Ratio
              Odd_Ratio$measure->Resultats$"Odd ratio"
            }else "les Odd ratios ne sont pas adaptes pour les tableaux 3x3 et plus grands"->Resultats$"Odd ratio"}
        
        
        data.frame(chisq.test(tab, correct = TRUE)$statistic, chisq.test(tab, correct = TRUE)$parameter,chisq.test(tab, correct = TRUE)$p.value)->yates
        names(yates)<-c("chi.deux", "ddl", "valeur.p")
        yates->Resultats$"chi.deux avec correction de Yates" # khi carre d independance avec correction de Yates
        if(khi_sans_Yates$p.value<0.05)  {
          chisq.test(tab)$residuals->Resultats$"Residus"
          (tab-chisq.test(tab)$expected)/(chisq.test(tab)$expected^0.5)->Resultats$"Residus standardises"
          chisq.test(tab,correct=FALSE)$stdres->Resultats$"Residus standardises ajustes"
          p.adjust(2*(1-pnorm(abs(Resultats$"Residus standardises ajustes"))), method="holm")->valeur.p
          matrix(valeur.p, nrow=nrow(tab))->valeur.p
          dimnames(tab)->dimnames(valeur.p)
          round(valeur.p,4)->Resultats$"Significativite des residus - probabilite corrigee en appliquant la methode de Holm"
        }
      }
      if(((!is.null(choix) && choix=="TEST de McNemar")) & nlevels(data[,X])==2 & nlevels(data[,Y])==2){
        mcnemar.test(tab)->McN
        data.frame(McN$statistic, McN$parameter, round(McN$p.value,4))->McN
        names(McN)<-c("chi.deux", "ddl", "valeur.p")
        McN->Resultats$"Test de McNemar avec correction de Yates" # test de McNemar    
        # mcnemar.test(tab, correct=FALSE))# pour ne pas avoir la correction de continuite mcnemar.test(tab, correct=FALSE))      
        if(length(tab[1,])==2|length(tab[,1])==2 & all(dimnames(tab)[[1]]==dimnames(tab)[[2]])) Resultats$Avertissement<-"Les cellules utilisees pour le McNemar dans le calcul sont celle de la 1e ligne 2e colonne et de la 2e ligne 1e colonne" else
          Resultats$Avertissement<-"Test de McNemar : les modalites ne sont pas les memes pour le test de McNemar. Est-ce bien un facteur en mesure repetee ?"}
      
      if(save){
        save(Resultats, choix, env=.e)
        # data->Resultats$donnees
        # date()->date
        # gsub(":",".",date)->date
        # dput(Resultats, file=paste("chi carre entre", names(data)[expand.grid(X,Y)[i,1]], "et", names(data)[expand.grid(X,Y)[i,2]], date,".txt"))
        # Resultats[[length(Resultats)]]<-NULL
        # Resultats$SAUVEGARDE<-paste("les donnees sont sauvegardees dans", getwd())
      }
      return(Resultats)
    }
    cbind(Resultats2, Resultats)->Resultats2
    ### Obtenir les Resultats
  }}

#### fonction qui permet de choisir lle type de correlation Ã  rÃ©aliser
choix.corr<-function(){options (warn=-1) 
  c( "svDialogs")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  writeLines("l analyse detaillee permet d avoir les statistiques descriptives, les tests de normalite, le nuage de points,
             \n des statistiques robustes, l'ensemble des coefficients de correlations. 
             \n la matrice de correlation permet de controler l erreur de 1e espece et est adapte pour un grand nombre de correlation
             \n la comparaison de correlation permet de comparer 2 correlations dependantes ou independantes
             \n Le choix autre correlations permet d avoir les correlation tetrachoriques et polychoriques")
  dlgList(c("Analyse detaillee (Bravais Pearson/Spearman/tau) pour une ou peu de correlations", 
            "Matrice de correlations", 
            "Comparaison de deux correlations",
            "Autres correlations"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez vous?")$res->choix
  if(length(choix)==0) return(analyse())
  switch(choix,
         "Analyse detaillee (Bravais Pearson/Spearman/tau) pour une ou peu de correlations"=corr.complet()->Resultats,
         "Matrice de correlations"= corr.matrice()->Resultats,
         "Comparaison de deux correlations"= comp.corr()->Resultats,
         "Autres correlations"= tetrapoly()->Resultats
  )
  return(Resultats)
}

#### Comapraison de corrÃ©lations 
comp.corr<-function(){options (warn=-1) 
  c("psych", "svDialogs")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  list()->Resultats # cree une liste appelee Resultats dans laquelle on va stocker les Resultats
  
  type3<- dlgList(c("appariees", "independantes"), preselect=FALSE, multiple = TRUE, title="Comparer deux comparaisons")$res
  if(length(type3)==0) return(choix.corr())
  
  if(type3=="appariees"){
    n <- dlgInput("Quelle est la taille de l echantillon ?", 0)$res
    if(length(n)==0) return(choix.corr())
    strsplit(n, ":")->n
    tail(n[[1]],n=1)->n
    as.numeric(n)->n
    xy <- dlgInput("Que vaut la 1e correlation - XY?", 0)$res
    if(length(xy)==0) return(choix.corr())
    strsplit(xy, ":")->xy
    tail(xy[[1]],n=1)->xy
    as.numeric(xy)->xy
    xz <- dlgInput("Que vaut la 2e correlation - XZ?", 0)$res
    if(length(xz)==0) return(choix.corr())
    strsplit(xz, ":")->xz
    tail(xz[[1]],n=1)->xz
    as.numeric(xz)->xz
    
    yz <- dlgInput("Que vaut la 1e correlation - YZ?", 0)$res
    if(length(yz)==0) return(choix.corr())
    strsplit(yz, ":")->yz
    tail(yz[[1]],n=1)->yz
    as.numeric(yz)->yz
    paired.r(xy=xy, xz=xz, yz=yz, n=n, n2=NULL,twotailed=TRUE)->Resultats$"comparaisons de deux correlations dependantes"}else{
      n <- dlgInput("Quelle est la taille de l echantillon pour la 1e correlation?", 0)$res
      if(length(n)==0) return(choix.corr())
      strsplit(n, ":")->n
      tail(n[[1]],n=1)->n
      as.numeric(n)->n
      xy <- dlgInput("Que vaut la 1e correlation - XY?", 0)$res
      if(length(xy)==0) return(choix.corr())
      strsplit(xy, ":")->xy
      tail(xy,n=1)->xy
      as.numeric(xy)->xy
      n2 <- dlgInput("Quelle est la taille de l echantillon pour la 2e correlation?", 0)$res
      if(length(n2)==0) return(choix.corr())
      strsplit(n2, ":")->n2
      tail(n2[[1]],n=1)->n2
      as.numeric(n2)->n2
      xz <- dlgInput("Que vaut la 2e correlation - XZ?", 0)$res
      if(length(xz)==0) return(choix.corr())
      strsplit(xz, ":")->xz
      tail(xz[[1]],n=1)->xz
      as.numeric(xz)->xz
      paired.r(xy=xy, xz=xz, yz=NULL, n=n, n2=n2,twotailed=TRUE)->Resultats$"comparaisons de deux correlations independantes"
    }
  
  return(Resultats)}

#### Analyse dÃ©taillÃ©e de corrÃ©lation simple/partielle/semi-partielle + Graphique
# fonction ok 
corr.complet<-function(X=NULL, Y=NULL, groupes=NULL, save=F, desires=c("Donnees completes", 
                                                                       "Identification des outliers", 
                                                                       "Analyse sans les valeurs influentes"), data=NULL, z=NULL){options (warn=-1) 
  c("outliers", "nortest", "psych", "lsr", "ggplot2", "reshape2", "car", "boot", "svDialogs", "ppcor","plyr","asbio")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  
  .e <- environment()
  dlgList(c("simple", "partielle et semi partielle"), preselect="simple", multiple = FALSE, title="Correlations simples ou partielles?")$res->type2
  if(length(type2)==0) type2<-"simple"
  X<-"autres donnees"
  while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
  if(length(data)==0) return(choix.corr())
  data<-get(data)
  X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees","annuler"), multiple = TRUE, 
             title="variable en abcisse")$res
  if(length(X)==0) X<-"autres donnees"
  if(any(X=="annuler")) return(corr.complet())}
  listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
  subset(listes, listes[,1] %in% X)[,2]->X
  Y<-dlgList(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), multiple = TRUE, title="Variable(s) en ordonnees")$res
  while(length(Y)==0) {print("veuillez preciser la variable sur l'axe des ordonnees")
    Y<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "),"annuler"), multiple = TRUE, title="Variable(s) en ordonnees")$res
    if(any(Y=="annuler")) return(corr.complet())  }
  subset(listes, listes[,1] %in% Y)[,2]->Y
  if(type2=="partielle et semi partielle"){Z<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "),"annuler"), multiple = TRUE, title="Variable(s) a controler")$res
  if(length(Z)==0||any(Z=="annuler")) return(corr.complet())
  subset(listes, listes[,1] %in% Z)[,2]->Z}
  
  dlgMessage("Faut-il faire les analyses par groupe ?", "yesno")$res->groupes
  if(groupes=="yes") {groupes<- dlgList(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), multiple = FALSE, title="choix de la variable  groupe ")$res
  if(length(groupes)==0) groupes<-NULL else{
    subset(listes, listes[,1] %in% groupes)[,2]->groupes}}else groupes<-NULL
  desires<- dlgList(c("Analyse sur les donnees completes", "Identification des outliers", "Analyse sans les valeurs influentes"), 
                    preselect=c("Analyse sur les donnees completes", "Identification des outliers", "Analyse sans les valeurs influentes"),
                    multiple = TRUE, title="Quels Resultats voulez vous obtenir ?")$res
  if(length(desires)==0) desires<-c("Analyse sur les donnees completes", "Identification des outliers", "Analyse sans les valeurs influentes")
  sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = TRUE, title="Enregistrer les Resultats ?")$res
  if(length(sauvegarde)==0) sauvegarde<-FALSE
  list()->Resultats # cree une liste appelee Resultats dans laquelle on va stocker les Resultats
  for(i in 1:(length(data[,c(X,Y)]))){
    if(is.factor(data[,c(X,Y)[i]])){           
      if(dlgMessage(paste("la variable", names(data)[c(X,Y)[i]], "n est pas numerique. \n
                          si c est une variable ordinale, vous pouvez preciser l ordre des modalites dans la console  \n
                          voulez vous continuez ?"), "okcancel")$res=="cancel") return(corr.complet()) else {
                            dlgForm(setNames(as.list(rank(levels(data[,i]))), levels(data[,i])), "Ordre des modalites")$res->niveaux
                            stack(niveaux)->niveaux                                                
                            factor(data[,i], levels(data[,i])[niveaux[,1]])->data[,i] 
                            as.numeric(data[,i])->data[,i]
                            Resultats$Avertissement[[i]]<-paste("la variable", names(data)[i], "a ete convertie en variable numerique")
    }}}  
  
  
  complete<-function(X,Y, data, groupes){ 
    list()->Resultats
    psych::describe(data[,c(X,Y)], type=3)->Resultats$"statistiques descriptives"
    lm.r<-lm(data[ ,Y] ~ data[ ,X],na.action=na.exclude)  
    qplot(data[ ,X], data[ ,Y], xlab=names(data[X]), ylab=names(data[Y]))->gr
    gr+geom_abline(intercept=lm.r$coefficients[[1]],slope=lm.r$coefficients[[2]] )->gr
    print(gr)
    resid(lm.r)->data$residus # recuperation du residu sur le modele lineaire
    assign("data", data,envir=.e)
    if(length(data$residus)<5000){
      shapiro.test(data$residus)->Shapiro_Wilk # test du Shapiro-Wilk sur le modele lineaire
      lillie.test(data$residus)->Lilliefors # test de Lilliefors sur le modele lineaire
      round(data.frame(Shapiro_Wilk$statistic,Shapiro_Wilk$p.value, Lilliefors$statistic, Lilliefors$p.value),4)->normalite
      names(normalite)<-c("W de Shapiro-Wilk", "valeur.p SW", "D de Lilliefors", "valeur.p Llfrs")
      dimnames(normalite)[1]<-" "
      format(normalite, width = max(sapply(names(normalite), nchar)), justify = "centre")->Resultats$"Tests de normalite"}
    cor.test(data[, X], data[ ,Y], method = "pearson")->BP
    boot_BP<-function(data,i)cor(data[ , X][i], data[ , Y][i], use="complete.obs", method="pearson")
    boot_BP_results<-boot(data, boot_BP, 2000)
    round(data.frame(BP$estimate,BP$estimate^2, BP$conf.int[1],BP$conf.int[2],BP$statistic, BP$parameter, BP$p.value,
                     boot.ci(boot_BP_results)$bca[,4], boot.ci(boot_BP_results)$bca[,5]),4)->BP
    names(BP)<-c("r", "r.deux", "IC lim inf", "IC lim sup", "t", "ddl", "valeur.p","Bca lim inf", " Bca lim sup")
    format(BP, width = max(sapply(names(BP), nchar)), justify = "centre")->Resultats$"Correlation de Bravais Pearson"
    cor.test(data[,X], data[ ,Y], method = "spearman", exact=T, continuity=T)->Spear
    boot_Spearman<-function(data,i)cor(data[ ,X][i], data[ , Y][i], use="complete.obs", method="spearman")
    boot_Spearman_results<-boot(data, boot_Spearman, 2000)
    round(data.frame(Spear$estimate,Spear$estimate^2,Spear$statistic,Spear$p.value, boot.ci(boot_Spearman_results)$bca[,4],boot.ci(boot_Spearman_results)$bca[,5]),4)->Spear
    names(Spear)<-c("rho", "r.deux", "S", "valeur.p","Bca lim inf", "Bca lim sup")
    format(Spear, width = max(sapply(names(Spear), nchar)), justify = "centre")->Resultats$"Rho de Spearman"
    cor.test(data[,X], data[ ,Y], method = "kendall")->Kendall  
    round(data.frame(Kendall$estimate,Kendall$statistic,Kendall$p.value),4)->Kendall 
    c("tau", "z", "valeur.p")->names(Kendall)
    Resultats$"Tau de Kendall"<-Kendall
    if(!is.null(groupes)){  
      if(any(sapply(data[,groupes], class)!="factor")){ 
        Resultats$AVERTISSEMENT2<-"Au moins une des variables  groupes  n etait pas un facteur et a ete converti en facteur"
        for(i in 1:length(groupes)){factor(data[,groupes[i]])->data[,groupes[i]]}}
      
      
      func <- function(data){ return(data.frame(n.observations= cor.test(data[, 1], data[ ,2], method = "pearson")$parameter+2,
                                                BP.r= cor.test(data[, 1], data[ ,2], method = "pearson")$estimate,
                                                BP.p= cor.test(data[, 1], data[ ,2], method = "pearson")$p.value,
                                                Spearman.rho= cor.test(data[, 1], data[ ,2], method = "spearman")$estimate,
                                                Spearman.p= cor.test(data[, 1], data[ ,2], method = "spearman")$p.value,
                                                Kendall.tau= cor.test(data[, 1], data[ ,2], method = "kendall")$estimate,
                                                Kendall.p= cor.test(data[, 1], data[ ,2], method = "kendall")$p.value))}
      data.frame(data[,c(X,Y)])->YY
      ddply(YY, .(data[, groupes]), func)->corr.groupes
      corr.groupes->Resultats$"Analyse par groupe"
      
      print(qplot(data[ ,X], data[ ,Y], xlab=names(data[X]), ylab=names(data[Y]), colour = data[,groupes]))
    }  
    return(Resultats)
  }
  
  complete.part<-function(X,Y,Z, data, groupes){ 
    list()->Resultats
    psych::describe(data[,c(X,Y, Z)], type=3)->Resultats$"statistiques descriptives"
    as.formula(paste0(names(data)[X]," ~ ",names(data)[Y]))->modele # on cree le modele le plus simple, donc un seul predicteur (seul le modele est ecrit mais on n en fait encore rien)
    length(Z)->nb_VI
    for(i in 1:nb_VI){update(modele, as.formula(paste0(".~.+",names(data)[Z[i]])))->modele}
    lm(modele,na.action=na.exclude, data=data)->lm.r
    partial.resid.plot(lm.r)->gr
    print(gr)
    resid(lm.r)->data$residus # recuperation du residu sur le modele lineaire
    assign("data", data,envir=.e)
    if(length(data$residus)<5000){
      shapiro.test(data$residus)->Shapiro_Wilk # test du Shapiro-Wilk sur le modele lineaire
      lillie.test(data$residus)->Lilliefors # test de Lilliefors sur le modele lineaire
      round(data.frame(Shapiro_Wilk$statistic,Shapiro_Wilk$p.value, Lilliefors$statistic, Lilliefors$p.value),4)->normalite
      names(normalite)<-c("W de Shapiro-Wilk", "valeur.p SW", "D de Lilliefors", "valeur.p Llfrs")
      dimnames(normalite)[1]<-" "
      format(normalite, width = max(sapply(names(normalite), nchar)), justify = "centre")->Resultats$"Tests de normalite"}
    data[complete.cases(data[,c(X,Y,Z)]),]->data
    
    rbind( pcor.test(data[,X], data[ ,Y], data[ , Z], method = "pearson")[1:3],
           pcor.test(data[,X], data[ ,Y], data[ ,Z], method = "spearman")[1:3],
           spcor.test(data[,X], data[ ,Y], data[ ,Z], method = "pearson")[1:3],
           spcor.test(data[,X], data[ ,Y], data[ ,Z], method = "spearman")[1:3])->cor.part
    cor.part$estimate^2->cor.part$r.carre
    round(cor.part, 4)->cor.part
    dimnames(cor.part)[[1]]<-c("Correlation partielle de Bravais Pearson",
                               "Correlation partielle de Spearman",
                               "Correlation semi-partielle de Bravais Pearson",
                               "Correlation semi-partielle de Spearman")
    dimnames(cor.part)[[2]]<-c("Correlation", "valeur.p", "test.t", "r.carre")
    cor.part->Resultats$"Correlations partielles"
    
    if(!is.null(groupes)){  
      if(any(sapply(data[,groupes], class)!="factor")){ 
        Resultats$AVERTISSEMENT<-"Au moins une des variables  groupes  n etait pas un facteur et a ete converti en facteur"
        for(i in 1:length(groupes)){factor(data[,groupes[i]])->data[,groupes[i]]}}
      
      
      func <- function(data){ return(data.frame(BP.r.part= pcor.test(data[, 1], data[ ,2],data[ ,3:length(data)], method = "pearson")$estimate,
                                                BP.p= pcor.test(data[, 1], data[ ,2],data[ ,3:length(data)], method = "pearson")$p.value,
                                                Spearman.rho.part= pcor.test(data[, 1], data[ ,2],data[ ,3:length(data)], method = "spearman")$estimate,
                                                Spearman.p= pcor.test(data[, 1], data[ ,2],data[ ,3:length(data)], method = "spearman")$p.value))}
      data.frame(data[,c(X,Y,Z)])->YY
      ddply(YY, .(data[, groupes]), func)->corr.groupes
      corr.groupes->Resultats$"Analyse par groupe"
      
    }  
    return(Resultats)
  }
  expand.grid(X,Y)->n.analyse  
  for(i in 1 : length(n.analyse[,1]) ){
    
    X<-n.analyse[i,1]
    Y<-n.analyse[i,2]
    list()->analyse
    paste("correlation entre", names(data)[X], "et", names(data)[Y])->analyse$nom
    if(is.null(desires) | desires=="Analyse sur les donnees completes"){ 
      if(type2=="simple") complete(X=X,Y=Y, data=data, groupes= groupes)->analyse$"Analyse sur les donnees completes"
      if(type2=="partielle et semi partielle") complete.part(X=X,Y=Y, Z=Z, data=data, groupes= groupes)->analyse$"Analyse sur les donnees completes"
    }
    if(is.null(desires) | any(desires=="Identification des outliers")|any(desires=="Analyse sans les valeurs influentes")) valeurs.influentes(X="residus", z=3.26, data=data)->outliers
    if(is.null(desires) |any(desires=="Identification des outliers"))analyse$"Valeurs influentes"<-outliers
    if(is.null(desires) |any(desires=="Analyse sans les valeurs influentes")){ 
      if(type2=="simple") complete(X=X,Y=Y, data=nettoyees, groupes= groupes)->analyse$"Analyse sans les valeurs influentes"
      if(type2=="partielle et semi partielle") complete.part(X=X,Y=Y, Z=Z, data=data, groupes= groupes)->analyse$"Analyse sans les valeurs influentes"
    }
    
    Resultats[[length(Resultats)+1]]<-analyse}
  if(sauvegarde){
    
    save(Resultats=Resultats ,choix =paste("correlation", type2, "entre", names(data)[X], names(data)[Y]), env=.e)}
  
  
  return(Resultats)}


#### Matrice de corrÃ©lations BP, SPEARMAN, KENDALL. PossiblitÃ© d'avoir corrÃ©lations partielles
corr.matrice<-function(X=NULL, Z=NULL, groupes=NULL, save=F, desires=c("Donnees completes", 
                                                                       "Identification des outliers", 
                                                                       "Donnees sans valeurs influentes"), data=NULL, type2="simple") { 
  options (warn=-1) 
  c( "psych",  "ggplot2", "boot", "svDialogs")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  .e <- environment()
  dlgList(c("simple", "partielle"), preselect="simple", multiple = FALSE, title="Correlations simples ou partielles?")$res->type2
  if(length(type2)==0) return(choix.corr())
  carre<<-"carre"
  if(type2=="simple"){
    writeLines("un matrice carree est une matrice avec toutes les correlations 2 a 2. 
               Une matrice rectangulaire est une matrice dans laquelle un premier ensemble de variables est mis en correlations avec un second jeu de variables")
    carre<-dlgList(c("carre", "rectangulaire","annuler"), multiple = TRUE, title="type de matrice")$res
    if(length(carre)==0 || carre=="annuler")return(choix.corr())
  }
  X<-"autres donnees"
  while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
  if(length(data)==0) return(choix.corr())
  data<-get(data)
  X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees","annuler"), multiple = TRUE, 
             title="variables")$res
  if(length(X)==0) X<-"autres donnees"
  if(any(X=="annuler")) return(corr.matrice())}
  
  listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
  subset(listes, listes[,1] %in% X)[,2]->X
  
  if(carre=="rectangulaire"){
    Y<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "annuler"), multiple = TRUE, 
               title="Deuxieme jeu de variable")$res
    if(any(Y=="annuler")) return(corr.matrice())
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% Y)[,2]->Y
    
  }else Y<-NULL
  
  if(type2=="partielle"){Z<-dlgList(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), multiple = TRUE, title="Variable(s) a controler")$res
  while(length(Z)==0) {print("les correlations partielles necessitent de controler au moins une variable. Veuillez la preciser")
    Z<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "),"annuler"), multiple = TRUE, title="Variable(s) a controler")$res
    if(any(Z=="annuler")) return(corr.matrice())}
  subset(listes, listes[,1] %in% Z)[,2]->Z}
  
  if(any(sapply(data[,c(X,Y, Z)], class)=="factor") )  {
    writeLines("au moins une des variables n'est pas numerique. On ne peut faire les analyses que sur des variables numeriques")
    return(choix.corr())
  }
  
  desires<- dlgList(c("Donnees completes", 
                      "Donnees sans valeur influente", "annuler"), 
                    preselect=c("Donnees completes"),
                    multiple = FALSE, title="Quels resultats voulez vous obtenir ?")$res
  if(length(desires)==0) desires<-"Donnees completes"
  if(desires=="annuler") return(corr.matrice())
  
  if(desires=="Donnees sans valeur influente") {          
    
    outlier(data[,c(X,Y,Z)])->data$D.Mahalanobis
    qchisq(p=0.001, df=length(X), ncp = 0, lower.tail = FALSE, log.p = FALSE)->seuil
    data[which(data$D.Mahalanobis>seuil),]->outliers
    length(outliers[,1])/length(data[,1])*100->pourcent
    
    print(paste(pourcent, "% des observations sont considerees comme outliers."))
    outliers->Resultats$"Valeurs considerees comme influentes"
    writeLines("Supprimer l'ensemble des outliers supprime la valeur l'ensemble des valeurs au-delÃ  du seuil
               Supprimer une observation a la fois permet de faire une analyse detaillee de chaque observation
               considerees comme influente en partant de la valeur la plus extreme. La procedure s'arrete 
               quand plus aucune observation n'est consideree comme influente")
    
    suppr<- dlgList(c("Suppression de l ensemble des outliers", 
                      "Suppression manuelle"), 
                    preselect=c("Suppression de l ensemble des outliers"),
                    multiple = FALSE, title="Comment voulez vous les supprimer?")$res
    if(length(suppr)==0) suppr<-"Suppression de l ensemble des outliers"
    if(suppr=="Suppression de l ensemble des outliers") data[which(data$D.Mahalanobis<seuil),]->data else{if(pourcent>0){
      print(data[which.max(data$D.Mahalanobis),])
      cat ("Appuyez [entree] pour continuer")
      line <- readline()
      dlgMessage("Voulez vous supprimer cette observation ?", "yesno")$res->suppression
      while(suppression=="yes"){data[-which.max(data$D.Mahalanobis),]->data
        print(data[which.max(data$D.Mahalanobis),])
        cat ("Appuyez [entree] pour continuer")
        line <- readline()
        dlgMessage("Voulez vous en supprimer cette observation ?", "yesno")$res->suppression}
    }
    }}
  
  print(mardia(data[,X],na.rm = TRUE,plot=TRUE))
  cat ("Appuyez [entree] pour continuer")
  line <- readline()
  
  dlgList(c("pearson", "spearman","kendall","annuler"), preselect="pearson", multiple = FALSE, title="Type de correlations ?")$res->method
  if(length(method)==0) method<-"pearson"
  if(method=="annuler") return(corr.matrice())
  dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none","annuler"), preselect=NULL, multiple = FALSE, title="Type de correction ?")$res->correction
  if(length(correction)==0) correction<-"holm"
  if(correction=="annuler") return(corr.matrice())
  writeLines("Vous pouvez faire l analyse pas sous-groupe. Dans ce cas, il faut choisir une variable categorielle. L analyse sera
             realisee pour chaque modalite de la variable")
  dlgMessage("Faut-il faire les analyses par groupe ?", "yesno")$res->groupe
  if(groupe=="yes") {groupes<- dlgList(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), multiple = TRUE, title="choix de la variable  groupe ")$res
  if(length(groupes)==0) groupes<-NULL else{
    subset(listes, listes[,1] %in% groupes)[,2]->groupes}}else groupes<-NULL
  writeLines("voulez-vous enregistrer les resultats")
  sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = TRUE, title="Enregistrer les Resultats ?")$res
  if(length(sauvegarde)==0) sauvegarde<-FALSE
  
  list()->Resultats # cree une liste appelee Resultats dans laquelle on va stocker les Resultats
  if(type2=="simple"){
    if(carre=="carre")corr.test(data[X], use = "pairwise",method=method,adjust=correction, alpha=.05,ci=TRUE)->matrice else {
      corr.test(data[X],data[Y], use = "pairwise",method=method,adjust=correction, alpha=.05,ci=TRUE)->matrice  }  
    round(matrice$r,3)->Resultats$"matrice de correlation"
    matrice$n->Resultats$"taille de l echantillon"
    round(matrice$p,3)->Resultats$"matrice des probabilites - Les donnees au-dessus de la diagonales sont ajustees pour comparaisons multiples"
    paste("la correction appliquee est celle de",correction)->Resultats$Correction
    if(method!="kendall") round(matrice$r^2,3)->Resultats$"matrice des r.deux"
    round(matrice$r,3)^2->Resultats$"Taille d effets"
    if(carre=="carre") cor.ci(data[,X], n.iter=600, plot=FALSE)$ci->Resultats$"Intervalle de confiance avec bootstrap"
    if(carre!="carre")  round(matrice$ci,4)->Resultats$"Intervalle de confiance" 
    if(!is.null(groupes)) {
      list()->groupes2
      for(i in 1:length(groupes)){data[,groupes[i]]->groupes2[[i]]}
      split(data,groupes2)->data.g
      for(i in 1:length(data.g)){
        list()->groupes2
        names(data.g)[i]->nom
        corr.test(data.g[[i]][X],data.g[[i]][Y], use = "pairwise",method=method,adjust=correction, alpha=.05,ci=TRUE)->matrice
        round(matrice$r,3)->groupes2$"matrice de correlation"
        matrice$n->groupes2$"taille de l echantillon"
        round(matrice$p,3)->groupes2$"matrice des probabilites - Les donnees au-dessus de la diagonales sont ajustees pour comparaisons multiples"
        if(method!="kendall") round(matrice$r^2,3)->groupes2$"matrice des r.deux"
        cor.ci(data.g[[i]][,X], plot=FALSE)$ci->groupes2$"Intervalle de confiance avec bootstrap"
        groupes2->Resultats[[length(Resultats)+1]]
        names(Resultats)<-c(names(Resultats)[1:(length(Resultats)-1)], nom)
      }
      
    }
    
  }
  
  if(type2=="partielle"){
    data[,c(X,Z)]->d2
    partial.r(d2, 1:length(X), (length(X)+1):length(d2))->par.r
    
    Resultats$"Matrice de correlations partielles" <- round(corr.p(par.r, adjust=correction, n=length(data[,1])-length(Z))$r,3)
    Resultats$"Probabilite des correlations partielles" <- round(corr.p(par.r, adjust=correction, n=length(data[,1])-length(Z))$p,4)
    Resultats$"Taille de l effet" <- round((corr.p(par.r, adjust=correction, n=length(data[,1])-length(Z))$r)^2,3)
    if(!is.null(groupes)) {
      list()->groupes2
      for(i in 1:length(groupes)){data[,groupes[i]]->groupes2[[i]]}
      split(data,groupes2)->data.g
      for(i in 1:length(data.g)){
        list()->groupes2
        data.g[[i]][c(X,Z)]->d2
        partial.r(d2, 1:length(X), (length(X)+1):length(d2))->par.r
        groupes2$"Matrice de correlations partielles" <- round(corr.p(par.r, adjust=correction, n=length(data[,1])-length(Z))$r,3)
        groupes2$"Probabilite des correlations partielles" <- round(corr.p(par.r, adjust=correction, n=length(data[,1])-length(Z))$p,4)
        groupes2$"Taille de l effet" <- round((corr.p(par.r, adjust=correction, n=length(data[,1])-length(Z))$r)^2,3)
        names(data.g)[i]->nom
        groupes2->Resultats[[length(Resultats)+1]]
        names(Resultats)<-c(names(Resultats)[1:(length(Resultats)-1)], nom)
        
      }
      
    }
  }
  if(sauvegarde) save(Resultats=Resultats, choix=paste("correlation", type2, "de", method), env=.e)
  return(Resultats)
}


#### Analyse factorielle exploratoire, analyse en composante principale, analye factorielle confirmatoire ####
# il reste l'analyse factorielle confirmatoire, corriger les corrÃ©lations tetrachoriques et polychoriques qui bug sur mac
# vÃ©rifier le fichier de sortie 
factor.an<-function(X, nfactors=2, n.iter=300, scores="regression", residuals=TRUE, rotate="oblimin", desires=c("Donnees completes", 
                                                                                                                "Donnees sans valeur influentes"),
                    missing=TRUE,impute="mean", cor="Pearson", oblique.scores=TRUE, sauvegarde=FALSE, data=NULL, scor.fac=FALSE){options (warn=-1)
  
  packages<-c("svDialogs", "GPArotation","psych")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  .e <- environment()
  list()->Resultats
  dlgList(c("Analyse factorielle exploratoire", 
            "Analyse factorielle confirmatoire",
            "Analyse en composante principale"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez vous realiser?")$res->choix
  if(length(choix)==0) return(analyse())
  
  X<-"autres donnees"
  while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
  if(length(data)==0) return(analyse())
  data<-get(data)
  X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees", "aucune"), multiple = TRUE, 
             title="Variables")$res
  if(length(X)==0L||X=="aucune") return(analyse())
  }
  
  listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
  subset(listes, listes[,1] %in% X)[,2]->X
  writeLines("La nature des variables determine le type de correlation : tetrachorique pour les variales dichotomiques 
             polychorique pour les ordinales et Bravais-Pearson pour les continues")
  
  if(length(unique(unlist(data[,X])))<3) {type<-dlgList(c("dichotomiques","ordinales",
                                                          "continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res}else if(length(unique(unlist(data[,X])))<9){
                                                            type<-dlgList(c("ordinales","continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res}else {
                                                              type<-dlgList(c("continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res 
                                                            }
  
  
  if(length(type) == 0L) type<-"continues"
  
  writeLines("les valeurs influentes sont identifiees sur la base de 
             la distance de Mahalanobis avec un seuil du chi Ã  0.001")
  desires<- dlgList(c("Donnees completes", 
                      "Donnees sans valeur influente", "annuler"), 
                    preselect=c("Donnees completes"),
                    multiple = FALSE, title="Quels resultats voulez vous obtenir ?")$res
  if(length(desires)==0) desires<-"Donnees completes"
  if(desires=="annuler") return(analyse())
  
  
  
  if(desires=="Donnees sans valeur influente") {          
    
    outlier(data[,X])->data$D.Mahalanobis
    qchisq(p=0.001, df=length(X), ncp = 0, lower.tail = FALSE, log.p = FALSE)->seuil
    data[which(data$D.Mahalanobis>seuil),]->outliers
    length(outliers[,1])/length(data[,1])*100->pourcent
    
    print(paste(pourcent, "% des observations sont considerees comme outliers."))
    outliers->Resultats$"Valeurs considerees comme influentes"
    writeLines("Supprimer l'ensemble des outliers supprime la valeur l'ensemble des valeurs au-delÃ  du seuil
               Supprimer une observation a la fois permet de faire une analyse detaillee de chaque observation
               considerees comme influente en partant de la valeur la plus extreme. La procedure s'arrete 
               quand plus aucune observation n'est consideree comme influente")
    
    suppr<- dlgList(c("Suppression de l ensemble des outliers", 
                      "Suppression manuelle"), 
                    preselect=c("Suppression de l ensemble des outliers"),
                    multiple = FALSE, title="Comment voulez vous les supprimer?")$res
    if(length(suppr)==0) suppr<-"Suppression de l ensemble des outliers"
    if(suppr=="Suppression de l ensemble des outliers") data[which(data$D.Mahalanobis<seuil),]->data else{if(pourcent>0){
      print(data[which.max(data$D.Mahalanobis),])
      cat ("Appuyez [entree] pour continuer")
      line <- readline()
      dlgMessage("Voulez vous supprimer cette observation ?", "yesno")$res->suppression
      while(suppression=="yes"){data[-which.max(data$D.Mahalanobis),]->data
        print(data[which.max(data$D.Mahalanobis),])
        cat ("Appuyez [entree] pour continuer")
        line <- readline()
        dlgMessage("Voulez vous en supprimer cette observation ?", "yesno")$res->suppression}
    }
    }}
  
  
  
  writeLines("Cette option permet de sauvegarder les resultats dans un fichier externe")
  dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Voulez vous sauvegarder?")$res->sauvegarde
  if(length(sauvegarde)==0) sauvegarde<-FALSE
  writeLines("Voulez-vous que les scores factoriels soient integres a vos donnees ?")
  dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Scores factoriels?")$res->scor.fac
  if(length(scor.fac)==0) scor.fac<-FALSE
  
  writeLines("Le critere de saturation permet de n afficher dans le tableau de resultats 
             que les saturation superieure au seuil fixe")
  Critere_saturation <- dlgInput("Quel est le critere de saturation que vous voulez utiliser ?", 0.3)$res
  if(length( Critere_saturation)==0)  Critere_saturation<-"0.3"
  strsplit(Critere_saturation, ":")->Critere_saturation
  tail(Critere_saturation[[1]],n=1)->Critere_saturation
  as.numeric(Critere_saturation)->Critere_saturation
  
  if(type=="dichotomiques"){Matrice<-tetrachoric(data[,X],correct=TRUE,smooth=TRUE,global=FALSE,weight=NULL,na.rm=TRUE, delete=TRUE)$rho
  cor="tet"
  methode<-c("minres")}
  
  if(type=="ordinales"){Matrice<-polychoric(data[,X],smooth=TRUE,global=FALSE,polycor=FALSE, ML = FALSE,
                                            std.err=FALSE,weight=NULL,progress=TRUE,na.rm=TRUE, delete=TRUE)$rho
  cor="poly"
  methode<-c("minres")}   
  
  if(type=="continues") {Matrice<-corr.test(data[,X])$r
  cor="Pearson"
  methode<-c("ml")}
  
  if(type=="mixte") {  Matrice<-mixed.cor(x = data[,X],smooth=TRUE, correct=.5,global=FALSE,
                                          ncat=8,use="pairwise",method="pearson",weight=NULL)$rho
  cor="mixed"
  methode<-c("minres")}
  
  View(round(Matrice,3))
  x11()
  cor.plot(Matrice, show.legend=FALSE, main=NULL, labels=NULL, n.legend=0, MAR=TRUE)
  if( type=="continues") print(mardia(data[,X],na.rm = TRUE,plot=TRUE))
  cat ("Appuyez [entree] pour continuer")
  line <- readline()
  writeLines("Pour les variables ordinales et dichomiques, preferes la methode du minimum des resisdus - minres -
             ou des moindres carres ponderes - wls. Pour les variables continues, le maximum de vraisemblance si
             la normalite est respectee - ml")
  methode2<-dlgList(c("minres","wls","gls","pa", "ml","minchi"), preselect= methode, multiple = FALSE, title="Quel algorithme desirez vous?")$res
  if(length(methode2)==0) methode2<-methode
  round(Matrice,3)->Resultats$"Matrice de correlation"
  round(unlist(cortest.bartlett(data[,X])),4)->bartlett
  names(bartlett)<-c("chi.carre","valeur.p","ddl")
  bartlett->Resultats$"Mesure d adequation de la matrice"$"Test de Barlett"
  ### doit etre significatif (attention depend de la taille de l echantillon)
  KMO(Matrice)->KMO1
  list()->kmo
  
  
  round(KMO1$MSA,3)->Resultats$"Mesure d adequation de la matrice"$"Indice de Kaiser-Meyer-Olkin global" ### doit etre superieur a 0.5 sinon la matrice ne convient pas pour analyse factorielle. Dans lÃÂideal, avoir au moins 0.8. Si des X presentent un KMO<0.5, on peut envisager de les supprimer. 
  round(KMO1$MSAi,3)->Resultats$"Mesure d adequation de la matrice"$'Indice de Kaiser-Meyer-Olkin par item'
  round(det(Matrice),5)->Resultats$"Mesure d adequation de la matrice"$"Determinant de la matrice de correlation"
  Resultats$"Mesure d adequation de la matrice"$"Determinant de la matrice de correlation"[[2]]<-"risque de multicolinearite si le determinant de la matrice est inferieur a 0.00001"
  print(Resultats$"Mesure d adequation de la matrice")
  print("le KMO doit absolument etre superieur a 0.5")
  cat ("Appuyez [entree] pour continuer")
  line <- readline()
  dlgMessage(c("La matrice est elle satisfaisante pour une AFE ?", "Continuer ?"), "okcancel")$res->res.kmo
  if(res.kmo=="cancel") {print("vous avez quitte l'AFE")
    return(analyse())}
  x11()
  fa.parallel(Matrice,n.obs=length(data[,1]),fm=methode2,main="Parallel Analysis Scree Plots",fa="both", cor=cor)->Parallele
  paste("l'analyse en parallele suggere", Parallele$nfact, "facteurs et", Parallele$ncomp, "composantes")->Parallele
  print(Parallele)
  Parallele->Resultats$"analyses paralleles"
  
  writeLines("Veuillez preciser le nombre de facteurs. Pour annuler, choisissez 0")
  nombre_facteurs <- dlgInput("Nombre de facteurs ?", 2)$res
  
  while(length(nombre_facteurs)==0){print("vous devez specifier le nombre de facteurs")
    nombre_facteurs <- dlgInput("Nombre de facteurs ?", 2)$res
    if(nombre_facteurs==0) return(analyse())}
  if(nombre_facteurs==0) return(analyse())
  strsplit(nombre_facteurs, ":")->nombre_facteurs
  tail(nombre_facteurs[[1]],n=1)->nombre_facteurs
  as.numeric(nombre_facteurs)->nombre_facteurs
  
  
  # Preciser le nombre dÃÂiterations pour le bootstrap (indiquer 1 si vous ne desirez pas de bootstrap) . Pour la methode dÃÂanalyse, minres (residus minimum, correspond a la methode des moindre carre ordinale utilisee par SPSS) est une des meilleures approches mais pa est la plus utilisee, la plus conventionnelles. La methode wls est la methode des moindre carre pondere, proche de ce qui est propose par LISREL.
  
  if(choix=="Analyse factorielle exploratoire") {  
    writeLines("Veuillez preciser le nombre de bootstrap Pour aucun bootstrap, choisir 1")
    n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
    if(length(n.boot)==0) n.boot<-1 else{
      strsplit(n.boot, ":")->n.boot
      tail(n.boot[[1]],n=1)->n.boot
      as.numeric(n.boot)->n.boot}
    
    if(nombre_facteurs>1){
      writeLines("Veuillez choisir le type de rotation. Oblimin est adapte en sciences humaines")
      rotation<-dlgList(c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor",
                          "promax",  "oblimin",  "simplimax","bentlerQ", "geominQ","biquartimin", "cluster"), 
                        preselect= "oblimin", multiple = FALSE, title="Quelle rotation")$res
      if(length(rotation)==0) rotation<-"none"
    }else  rotation<-"none"
    fa(Matrice,nfactors= nombre_facteurs, n.obs=length(data[,1]),fm=methode2, rotate=rotation, n.iter=n.boot)->FA.results # realise l AFE
    list()->FA.results1
    FA.results1$analyse<-paste("analyse factorielle en utilisant la fonction fa du package psych, la methode", FA.results$fm)
    if(rotation=="none") FA.results1$rotation<-"il n y a pas de rotation" else FA.results1$rotation<-paste("la rotation est un rotation", FA.results$rotation)
    FA.results1$correlations<-paste("les correlations sont des correlation", cor)
    
    FA.results1$"saturations standardisÃ©es basees sur la matrice de correlation"<-round(as(FA.results$loadings, "matrix"),3)
    round(rbind(colSums(FA.results$loadings^2), colSums(FA.results$loadings^2)/length(X),
                cumsum(colSums(FA.results$loadings^2)/length(X))),3)->var.ex
    dimnames(var.ex)[[1]]<-c("Sommes des carres des saturations", "proportion de variance expliquee",
                             "proportion de variance expliquee cumulee")
    FA.results1$"Variance expliquee"<-var.ex
    data.frame(communaute=round(FA.results$communality,3),
               specifite=round(FA.results$uniquenesses,3),
               complexite=round(FA.results$complexity,2))->FA.results1$communaute
    paste("ML",1:nombre_facteurs)->noms
    if(nombre_facteurs>1 & rotation=="oblimin"){
      round(FA.results$Phi)->cor.f
      FA.results1$"correlations entre facteurs"<-cor.f}
    paste("la complexite moyenne est de", mean(FA.results$complexity), "Cela teste si", nombre_facteurs, "facteurs suffise(nt)" )-> FA.results1$"Complexite moyenne"
    round(matrix(c(FA.results$null.chisq, FA.results$null.dof,FA.results$null.model,
                   FA.results$dof, FA.results$objective, FA.results$RMSEA,
                   FA.results$TLI,FA.results$BIC, FA.results$SABIC,FA.results$rms, FA.results$crms, FA.results$fit.off, 
                   FA.results$chi, FA.results$EPVAL, FA.results$STATISTIC, FA.results$PVAL, FA.results$n.obs), ncol=1),4)->stats
    
    
    c("chi carre du modele null", "DegrÃ©s de libertÃ© du modele null", "fonction objective du modele null",
      "degrÃ©s de libertÃ© du modele", "fonction objective du modele", "RMSEA", "limite inferieure du RMSEA", "limite superieure du RMSEA",
      "Seuil de confiance (1- intervalle de confiance)", "facteur de fiabilite de Tucker Lewis - TLI", "BIC", "EBIC", 
      "RMSR", "RMSR corrige", "AdÃ©quation basee sur les valeurs en dehors de la diagonale", "chi carre empirique", "valeur de la proabilite du chi carre empirique",
      "chi carre du maximum de vraisemblance", "valeur de la probabilite du chi carre du maximum de vraisemblance", "nombre total d'observations")->dimnames(stats)[[1]]
    
    "valeurs"->dimnames(stats)[[2]]
    stats->FA.results1$"Indices d'adequation et ajustement"
    round(rbind((FA.results$R2)^0.5,FA.results$R2,2*FA.results$R2-1),2)->stats
    dimnames(stats)[[1]]<-c("Correlations des scores avec les facteurs", "R carre multiple des scores avec les facteurs",
                            "Correlation minimale possible des scores avec les facteurs")
    
    dimnames(stats)[[2]]<-noms
    stats->FA.results1$"Correlations des scores avec les facteurs"
    if(n.boot>1) {
      IC<-c()
      for(i in 1:nombre_facteurs){
        cbind(round(FA.results$cis$ci[,i],3), 
              round(as(FA.results$loadings, "matrix"),3)[,i],
              round(FA.results$cis$ci[,i+nombre_facteurs],3))->IC2
        dimnames(IC2)[[2]]<-c("lim.inf", dimnames(FA.results$loadings)[[2]][i],"lim.inf")
        cbind(IC, IC2)->IC
      }
      IC->FA.results1$"Intervalle de confiance des saturations sur la base du bootstrap - peut Ãªtre biaisÃ© en cas de Heyhood case"
      
    }
    
    FA.results1->Resultats$"Analyse factorielle"
    print.psych(FA.results, cut= Critere_saturation,sort=TRUE)
    print(fa.diagram(FA.results))#representation graphique des saturations}
    if(scor.fac){Scores.fac<-c()
    sapply(data[,X], scale)->centrees
    FA.results$weights->matrice2
    t(matrice2)->matrice2
    for(i in 1 : nombre_facteurs){
      apply(centrees%*%matrice2[i,],1,sum)->centrees2
      cbind(Scores.fac,centrees2)->Scores.fac
    }
    paste("facteur", 1:nombre_facteurs)->names(Scores.fac)
    data.frame(data,Scores.fac)->>donnees.scor.fac
    print("les donnees sont stockees dans l objet donnees.scor.fac")
    
    }
  }
  if(choix=="Analyse factorielle confirmatoire") print("en construction")
  if(choix=="Analyse en composante principale"){
    writeLines("Veuillez choisir le type de rotation. Oblimin est adapte en sciences humaines")
    rotation<-dlgList(c("none", "varimax", "quartimax", "promax",  "oblimin",  "simplimax","cluster"), 
                      preselect= "oblimin", multiple = FALSE, title="Quelle rotation")$res
    if(length(rotation)==0) rotation<-"none"
    principal(Matrice, nfactors= nombre_facteurs, n.obs=length(data[,1]), rotate=rotation)->PCA
    list()->PCA1
    PCA1$analyse<-paste("analyse factorielle en utilisant la fonction fa du package psych, la methode", PCA$fm)
    PCA1$rotation<-paste("la rotation est un rotation", PCA$rotation)
    PCA1$correlations<-paste("les correlations sont des correlation", cor)
    
    PCA1$"saturations standardisÃ©es basees sur la matrice de correlation"<-round(as(PCA$loadings, "matrix"),3)
    round(rbind(colSums(PCA$loadings^2), colSums(PCA$loadings^2)/length(X),
                cumsum(colSums(PCA$loadings^2)/length(X))),3)->var.ex
    dimnames(var.ex)[[1]]<-c("Sommes des carres des saturations", "proportion de variance expliquee",
                             "proportion de variance expliquee cumulee")
    PCA1$"Variance expliquee"<-var.ex
    data.frame(communaute=round(PCA$communality,3),
               specifite=round(PCA$uniquenesses,3),
               complexite=round(PCA$complexity,2))->PCA1$communaute
    paste("TC",1:nombre_facteurs)->noms
    if(nombre_facteurs>1 & rotation=="oblimin"){
      round(round(PCA$r.scores,3))->cor.f
      PCA1$"correlations entre facteurs"<-cor.f}
    paste("la complexite moyenne est de", mean(PCA$complexity), "Cela teste si", nombre_facteurs, "facteurs suffise(nt)" )-> PCA1$"Complexite moyenne"
    round(matrix(c(PCA$null.dof,PCA$null.model,
                   PCA$dof, PCA$objective, 
                   PCA$rms, PCA$fit.off, 
                   PCA$chi, PCA$EPVAL, PCA$STATISTIC, PCA$PVAL, PCA$n.obs), ncol=1),4)->stats
    
    
    c("DegrÃ©s de libertÃ© du modele null", "fonction objective du modele null",
      "degrÃ©s de libertÃ© du modele", "fonction objective du modele",
      "RMSR",  "AdÃ©quation basee sur les valeurs en dehors de la diagonale", "chi carre empirique", "valeur de la proabilite du chi carre empirique",
      "chi carre du maximum de vraisemblance", "valeur de la probabilite du chi carre du maximum de vraisemblance", "nombre total d'observations")->dimnames(stats)[[1]]
    
    "valeurs"->dimnames(stats)[[2]]
    stats->PCA1$"Indices d'adequation et ajustement"
    PCA1->Resultats$"Analyse en composante principale"
    if(scor.fac){
      
      Scores.fac<-c()
      sapply(data[,X], scale)->centrees
      PCA$weights->matrice2
      t(matrice2)->matrice2
      for(i in 1 : nombre_facteurs){
        apply(centrees%*%matrice2[i,],1,sum)->centrees2
        cbind(Scores.fac,centrees2)->Scores.fac
      }
      dimnames(matrice2)[[1]]->names(Scores.fac)
      data.frame(data,Scores.fac)->>donnees.scor.fac
      
      print("les donnees sont stockees dans l objet donnees.scor.fac")
    }
  }
  Resultats$"Information"<-c("les correlations utilisees sont des correlations de Bravais-Pearson pour",
                             "les variables continues, tetrachoriques pour les variables dichotomiques",
                             "et polychoriques pour les variables ordinales",
                             "Ces correlations sont optimales en fonction de la nature des variables")
  if(sauvegarde)save(Resultats=Resultats, choix=choix, env=.e)
  
  return(Resultats)
}


#### alpha de Cronbach, coefficient d'accord de Kendall, corrÃ©lation intra-classe ####
# il reste l'analyse factorielle confirmatoire, corriger les corrÃ©lations tetrachoriques et polychoriques qui bug sur mac
fiabilite<-function(x, data=NULL, choix="Alpha de Cronbach", type="continues",desires="Donnees completes"){options (warn=-1)
  packages<-c("svDialogs", "psych")
  # corriger pour l installation de WRS
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  
  .e <- environment()
  dlgList(c("Alpha de Cronbach", 
            "Correlation intra-classe",
            "Coefficient de concordance de Kendall"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez vous realiser?")$res->choix
  if(length(choix)==0) return(analyse())
  dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="voulez vous sauvegarder?")$res->sauvegarde
  if(length(sauvegarde)==0) sauvegarde<-FALSE
  list()->Resultats
  
  if(choix=="Alpha de Cronbach"|choix=="Correlation intra-classe" ){
    X<-"autres donnees"
    while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
    if(length(data)==0) return(fiabilite())
    data<-get(data)
    X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees", "annuler"), multiple = TRUE, 
               title="Variables")$res
    if(length(X) == 0L)  return(fiabilite())
    if(any(X=="annuler")) return(fiabilite())
    }
    
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% X)[,2]->X
    
    writeLines("les valeurs influentes sont identifiees sur la base de 
               la distance de Mahalanobis avec un seuil du chi Ã  0.001")
    desires<- dlgList(c("Donnees completes", 
                        "Donnees sans valeur influente", "annuler"), 
                      preselect=c("Donnees completes"),
                      multiple = FALSE, title="Quels resultats voulez vous obtenir ?")$res
    if(length(desires)==0) desires<-"Donnees completes"
    if(desires=="annuler") return(analyse())
    
    
    
    if(desires=="Donnees sans valeur influente") {          
      
      outlier(data[,X])->data$D.Mahalanobis
      qchisq(p=0.001, df=length(X), ncp = 0, lower.tail = FALSE, log.p = FALSE)->seuil
      data[which(data$D.Mahalanobis>seuil),]->outliers
      round(length(outliers[,1])/length(data[,1])*100,2)->pourcent
      
      print(paste(pourcent, "% des observations sont considerees comme outliers."))
      outliers->Resultats$"Valeurs considerees comme influentes"
      writeLines("Supprimer l'ensemble des outliers supprime la valeur l'ensemble des valeurs au-delÃ  du seuil
                 Supprimer une observation a la fois permet de faire une analyse detaillee de chaque observation
                 considerees comme influente en partant de la valeur la plus extreme. La procedure s'arrete 
                 quand plus aucune observation n'est consideree comme influente")
      
      suppr<- dlgList(c("Suppression de l ensemble des outliers", 
                        "Suppression manuelle"), 
                      preselect=c("Suppression de l ensemble des outliers"),
                      multiple = FALSE, title="Comment voulez vous les supprimer?")$res
      if(length(suppr)==0) suppr<-"Suppression de l ensemble des outliers"
      if(suppr=="Suppression de l ensemble des outliers") data[which(data$D.Mahalanobis<seuil),]->data else{if(pourcent>0){
        print(data[which.max(data$D.Mahalanobis),])
        cat ("Appuyez [entree] pour continuer")
        line <- readline()
        dlgMessage("Voulez vous supprimer cette observation ?", "yesno")$res->suppression
        while(suppression=="yes"){data[-which.max(data$D.Mahalanobis),]->data
          print(data[which.max(data$D.Mahalanobis),])
          cat ("Appuyez [entree] pour continuer")
          line <- readline()
          dlgMessage("Voulez vous en supprimer cette observation ?", "yesno")$res->suppression}
      }
      }}
    
    
    if(choix=="Alpha de Cronbach")  {
      writeLines("Vous pouvez preciser ici si il y a des items qui sont inverses")
      keys<-dlgList(c(paste(names(data)[X], "(format :", sapply(data[,X], class), ")", sep=" "), 
                      "aucun"), multiple = TRUE, title="items inverses?")$res
      if(length(keys)==0) keys<-"aucun"
      if(any(keys=="aucun")) keys<-"aucun"
      subset(listes, listes[,1] %in% keys)[,2]->keys
      
      writeLines("Veuillez preciser le nombre de bootstrap Pour aucun bootstrap, choisir 1.
                 Si vous choisissez un bootstrap, vous ne pouvez pas choisir le type de correlations.")
      n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
      if(length(n.boot)==0) n.boot<-1 else{
        strsplit(n.boot, ":")->n.boot
        tail(n.boot[[1]],n=1)->n.boot
        as.numeric(n.boot)->n.boot}
      if(n.boot==1){
        type<-dlgList(c("dichotomiques", 
                        "ordinales",
                        "continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res
        if(length(type)==0) type<-"continues"
        
        if(type=="dichotomiques")Matrice<-tetrachoric(data[,X],correct=TRUE,smooth=TRUE,global=FALSE,weight=NULL,na.rm=TRUE, delete=TRUE)$rho
        
        if(type=="ordinales"){Matrice<-polychoric(data[,X],smooth=TRUE,global=FALSE,polycor=FALSE, ML = FALSE,
                                                  std.err=FALSE,weight=NULL,progress=TRUE,na.rm=TRUE, delete=TRUE)$rho}   
        
        if(type=="continues") Matrice<-corr.test(data[,X])$r
        
        if(type=="mixte") {  Matrice<-mixed.cor(x = data[,X],smooth=TRUE, correct=.5,global=FALSE,
                                                ncat=8,use="pairwise",method="pearson",weight=NULL)$rho}
        psych::alpha(Matrice, keys=keys, n.iter=n.boot,n.obs=length(data[,1]))->cron}else psych::alpha(data[,X], keys=keys, n.iter=n.boot,n.obs=length(data[,1]))->cron
      
      round(cron$total,3)->Resultats$"Alpha de Cronbach sur la totalite de l echelle"
      if(n.boot>1) cron$boot.ci->Resultats$"Intervalle de confiance base sur le bootstrap"
      cron$total[,1]->a1
      cron$total[,6]->ase
      data.frame(Lim.inf.IC.95=a1-1.96*ase, alpha=a1, Lim.sup.IC.95=a1+1.96*ase)->Resultats$"Intervalle de confiance base sur l erreur standard de l alpha"
      round(data.frame(cron$alpha.drop, cron$item.stats ),3)->Resultats$"fiabilite si un item est supprime"
      
    }
    
    
    if(choix=="Correlation intra-classe"){ICC(data[,X], missing=FALSE)->ICC.out
      ICC.out[[1]]->Resultats$"correlation intra-classe"
      Resultats$"informations"<-paste("le nombre de juge =", length(X), "et le nombre d'observation =", ICC.out$n.obs) } }
  if(choix=="Coefficient de concordance de Kendall"){  
    X<-"autres donnees"
    while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    data<-dlgList(data, multiple = FALSE, title="Choix du dataframe")$res
    if(length(data)==0) return(fiabilite())
    data<-get(data)
    X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees"), multiple = TRUE, 
               title="Juge 1")$res
    if(length(X) == 0L)  return(fiabilite())
    }
    
    Y<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = FALSE, title="Juge 2")$res
    if(length(Y) == 0L)  return(fiabilite())
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% X)[,2]->X
    subset(listes, listes[,1] %in% Y)[,2]->Y  
    cohen.kappa(data[,c(X,Y)], w=NULL,n.obs=NULL,alpha=.05)->CK.out
    CK.out$confid->Resultats$"Coefficient de concordance de Kendall"
    Resultats$information<-paste("le nombre d observations =", CK.out$n.obs)
  }
  
  if(sauvegarde)save(Resultats=Resultats, choix=choix, env=.e)
  
  return(Resultats)
  
}


#### ModÃ¨les linÃ©aires avec ou sans interaction - rÃ©gressions + mediation et donc modÃ©ration ####
# ajouter la possibilitÃ© d'Ã©crire son propre modÃ¨le
# ajouter les fonctions pour les modÃ¨les non linÃ©aires 
regressions<-function(){
  options (warn=-1) 
  .e <- environment()
  c("boot","car", "QuantPsyc","psych", "wle","DAAG","gsl","MBESS","MASS","svDialogs","nortest")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  list()->Resultats
  
  dlgList(c("Effets simples ou multiples", 
            "Effets de mediation", 
            "Modele complet - avec interaction"), preselect="Effets simples ou multiples", multiple = FALSE, title="Quelle type de regression?")$res->choix
  if(length(choix)==0) return(analyse())
  if(choix=="Effets de mediation"){
    dlgList(c("Effets de mediation simple", 
              "Effet de mediation distante"), preselect=NULL, multiple = FALSE, title="Quelle type de regression?")$res->choix2
    if(length(choix2)==0) return(regressions())
    
    
    if(choix2=="Effets de mediation simple"){
      print("veuillez choisir les donnees")
      X<-"autres donnees"
      while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
      data<-dlgList(data, multiple = F, title="Choix du dataframe")$res
      if(length(data)==0) return(regressions())
      data<-get(data)
      print("veuillez preciser le predicteur")
      X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees","annuler"), multiple = TRUE, 
                 title="Predicteur")$res
      if(length(X)==0) X<-"autres donnees"
      if(any(X=="annuler")) return(regressions())}
      listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
      subset(listes, listes[,1] %in% X)[,2]->X
      print("veuillez choisir le mediateur")
      Mediator<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = F, 
                        title="Mediateur")$res
      while(length(Mediator) == 0L) {print("vous devez specifier le mediateur")
        Mediator<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "),"annuler"), multiple = F, 
                          title="Mediateur")$res
        if(Mediator=="annuler") return(regressions())}
      subset(listes, listes[,1] %in% Mediator)[,2]->Mediator
      print("veuillez choisir la variable dependante")
      VD<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = F, 
                  title="Variable dependante")$res
      while(length(VD) == 0L) {print("vous devez specifier la variable dependante")
        VD<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "),"annuler"), multiple = F, 
                    title="Variable dependante")$res
        if(VD=="annuler") return(regressions())}
      subset(listes, listes[,1] %in% VD)[,2]->VD
      print("veuillez preciser le nombre de bootstrap. Un minimum de 500 est idealement requis. Peut prendre du temps pour N>1000")
      n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
      if(length(n.boot)==0) n.boot<-"0"
      strsplit(n.boot, ":")->n.boot
      tail(n.boot[[1]],n=1)->n.boot
      as.numeric(n.boot)->n.boot
      if(any(n.boot>50)) bootstrap<-TRUE else bootstrap<-FALSE
      (mediation(data[,X], data[,Mediator], data[,VD], conf.level = 0.95, bootstrap = bootstrap, B = n.boot, which.boot="both", save.bs.replicates=TRUE, complete.set=TRUE))->Resultats$Analyse.mediation
      Resultats$Information<-"Pour une description detaillee des Resultats, ?mediation"
      mediation.effect.bar.plot2(data[,X], data[,Mediator], data[,VD],main = "Mediation Effect Bar Plot", width = 1, left.text.adj = 0,right.text.adj = 0, rounding = 3, file = "", save.pdf = FALSE,save.eps = FALSE, save.jpg = FALSE)
    }
    
    if(choix2=="Effet de mediation distante"){
      print("veuillez choisir les donnees")
      X<-"autres donnees"
      while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
      data<-dlgList(data, multiple = F, title="Choix du dataframe")$res
      if(length(data)==0) return(regressions())
      data<-get(data)
      print("veuillez preciser le predicteur")
      X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees","annuler"), multiple = FALSE, 
                 title="Predicteur")$res
      if(length(X)==0) X<-"autres donnees"
      if(X=="annuler") return(regressions())}
      listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
      subset(listes, listes[,1] %in% X)[,2]->X
      
      print("veuillez preciser le mediateur 1")
      Mediator1<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = F, 
                         title="Mediateur 1")$res
      while(length(Mediator1) == 0L) {print("vous devez specifier le mediateur")
        Mediator1<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "),"annuler"), multiple = F, 
                           title="Mediateur 1")$res
        if(Mediator1=="annuler") return(regressions())}
      subset(listes, listes[,1] %in% Mediator1)[,2]->Mediator1
      print("veuillez preciser le mediateur 2")
      Mediator2<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = F, 
                         title="Mediateur 2")$res
      while(length(Mediator2) == 0L) {print("vous devez specifier le mediateur")
        Mediator2<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "),"annuler"), multiple = F, 
                           title="Mediateur 1")$res
        if(Mediator2=="annuler") return(regressions())}
      subset(listes, listes[,1] %in% Mediator2)[,2]->Mediator2
      print("veuillez preciser la variable predite")
      VD<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = F, 
                  title="Variable dependante")$res
      while(length(VD) == 0L) {print("vous devez specifier la variable dependante")
        VD<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "),"annuler"), multiple = F, 
                    title="Variable dependante")$res
        if(VD=="annuler") return(regressions())}
      subset(listes, listes[,1] %in% VD)[,2]->VD
      print("veuillez preciser le nombre de bootstrap. Un minimum de 500 est idealement requis. Peut prendre du temps pour N>1000")      
      n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
      if(length(n.boot)==0) n.boot<-"1"
      strsplit(n.boot, ":")->n.boot
      tail(n.boot[[1]],n=1)->n.boot
      as.numeric(n.boot)->n.boot
      if(n.boot<1 ||is.na(n.boot)) nboot<-1
      data2<-data[,c(X, Mediator1, Mediator2, VD)]
      names(data2)<-c("x", "m1","m2","y")
      distal.med(data2)->results
      data.frame(results)->results
      round(as.numeric(as.character(results$Effect)),4)->results$Effect
      round(as.numeric(as.character(results$SE)),4)->results$SE
      round(as.numeric(as.character(results[,3])),3)->results$t.ratio
      round(as.numeric(as.character(results$Med.Ratio)),4)->results$Med.Ratio 
      names(results)<-c("Effet", "Erreur.st","test.t", "Ratio.med")
      results->Resultats$"Mediation a distance"
      Resultats$Information<-"Pour une description detaillee des Resultats, ?distal.med"
      distmed.boot <- boot(data2, distInd.ef, R=n.boot)
      boot.ci(distmed.boot, conf=.95, type=c("basic","perc", "norm"))->IC.boot
      round(matrix(c(IC.boot$normal[,2:3],IC.boot$basic[,4:5],IC.boot$percent[,4:5]), ncol=2 ),4)->IC.boot
      dimnames(IC.boot)[[1]]<-c("normal","basic","percentile")
      dimnames(IC.boot)[[2]]<-c("limite.inf","limite.sup")
      IC.boot->Resultats$"Intervalle de confiance estime par bootstrap"
      
      
    } 
  }
  if(choix=="Effets simples ou multiples"|choix=="Modele complet - avec interaction"){ 
    X<- "autres donnees"
    print("Veuillez choisir les donnees")
    while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
    if(length(data)==0) return(regressions())
    data<-get(data)
    print("Veuillez choisir le-s predicteur-s")
    X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees","annuler"), multiple = TRUE, 
               title="Predicteur(s)")$res
    if(length(X)==0) X<- "autres donnees"
    if(any(X=="annuler")) return(regressions())}
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% X)[,2]->X
    
    
    VD<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "annuler"), multiple = FALSE, 
                title="Variable dependante")$res
    if(length(VD) == 0L||VD=="annuler") return(analyse())
    subset(listes, listes[,1] %in% VD)[,2]->VD
    data[complete.cases(data[,c(VD,X)]),]->data
    
    codage<-dlgList(c("Centre", "Non centre"), multiple = FALSE, title="Type de codage?")$res
    if(length(codage)==0) codage<-"Non centre"
    
    if(codage=="Centre") {Resultats$"En accord avec les recommandations de Schielzeth 2010, les donnees ont ete prealablement centrees"
      X[sapply(data[,X], class)=="factor"]->facteurs
      
      if(length(facteurs)!=0){
        setdiff(X,which(sapply(data, class)=="factor"))->X
        length(data)->n.var
        for(i in 1:length(facteurs)){
          model.matrix(~ data[,facteurs[i]]-1 , data)->dummy
          data.frame(dummy)->dummy
          paste(names(data)[facteurs[i]],levels(data[,facteurs[i]]),sep="")->noms
          c(names(data)[1:length(data)], noms[2:length(noms)])->noms
          data.frame(data, dummy[,2:length(dummy)])->data
          noms->names(data)
        }
        X<-c(X, (n.var+1):length(data))
        
        fun<-function(X){X-mean(X)}
        sapply(X=data[,X], fun)->data[,X]
      }}
    writeLines("le nombre de bootstrap permet de creer un intervalle de confiance en prenant en compte
               la distribution des donnees. Plus il y en a mieux, c est. Il en faut idealement au moins 500
               et plus de 5000 pour obtenir l'intervalle de confiance avec une correction des biais (BCA).
               Dans ce cas, cela peut prendre du temps de calcul. Aucun bootstrap n'est realiser si la valeur vaut 1")
    nboot<-dlgInput("Nombre de bootstrap ? (1  = pas de bootstrap - BCa necessite N>5000)", 1)$res
    if(length(nboot)==0) nboot<-"1"
    strsplit(nboot, ":")->nboot
    tail(nboot[[1]],n=1)->nboot
    as.numeric(nboot)->nboot
    if(nboot<1 ||is.na(nboot)) nboot<-1
    
    desires<- dlgList(c("Donnees completes", 
                        "Identification des outliers", 
                        "Donnees sans valeur influente", "annuler"), 
                      preselect=c("Donnees completes", 
                                  "Identification des outliers", 
                                  "Donnees sans valeur influente"),
                      multiple = TRUE, title="Quels Resultats voulez vous obtenir ?")$res
    if(length(desires)==0) desires<-c("Donnees completes", "Identification des outliers", "Donnees sans valeur influente")
    if(any(desires=="annuler")) return(regressions())
    
    autres.options<- dlgList(c("Methodes de selection", 
                               "Modeles hierarchiques", "Validation croisee",
                               "Aucune", "annuler"), 
                             preselect=c("Aucune"),
                             multiple = FALSE, title="Autres options?")$res 
    if(length(autres.options)==0) autres.options<-"aucune"
    if(autres.options=="annuler") return(regressions())
    if(autres.options=="aucune"){method<-NULL
    type.selection<-NULL
    }
    if(autres.options=="Methodes de selection"){
      
      method<- dlgList(c("F d entree", "Critere d information d Akaike"), 
                       preselect=c("F d entree"),
                       multiple = TRUE, title="Choix de la methode")$res
      if(length(method)==0) method<-"F d entree"
      type.selection<- dlgList(c("Forward - pas-a-pas ascendant","Backward- pas-a-pas descendant", "Bidirectionnel"), 
                               preselect=NULL,
                               multiple = FALSE, title="Choix de la methode")$res
      while(length(type.selection)==0){print("veuillez preciser la methode de selection")
        type.selection<- dlgList(c("Forward - pas-a-pas ascendant","Backward- pas-a-pas descendant", "Bidirectionnel"), 
                                 preselect=NULL, multiple = FALSE, title="Choix de la methode")$res}
      if(method=="F d entree"){
        f.in<- dlgList(c("Valeur du F","Valeur de la probabilite"), 
                       preselect=NULL, multiple = FALSE, title="Quel critere d inclusion?")$res
        if(length(f.in)==0) f.in<-"Valeur de la probabilite"
        if(f.in=="Valeur du F"){f.in <- dlgInput("Quelle valeur du F voulez vous utiliser?", 4)$res
        if(length(f.in)==0) f.in<-"4"
        strsplit(f.in, ":")->f.in
        tail(f.in[[1]],n=1)->f.in
        as.numeric(f.in)->f.in}else{p.in <- dlgInput("Quelle seuil voulez vous utiliser pour les probabilites?", 0.15)$res
        if(length(p.in)==0) p.in<-"0.15"
        strsplit(p.in, ":")->p.in
        tail(p.in[[1]],n=1)->p.in
        as.numeric(p.in)->p.in
        qf(p.in, 1, (length(data[,VD])-1-length(X)), lower.tail = F, log.p = FALSE)->f.in}}
    }
    
    if(autres.options=="Modeles hierarchiques") {
      step.reg2<-list()
      if(choix=="Effets simples ou multiples"){ 
        step.reg2[[1]]<- dlgList(names(data)[X], preselect=NULL, multiple = TRUE, title="Variable(s) de cette etape")$res
        setdiff(names(data)[X],step.reg2[[1]])->reste
        i<-1
        while(length(reste!=0)){i+1->i
          step.reg2[[i]]<-dlgList(reste, multiple = TRUE,title="Variable(s) de cette etape")$res
          setdiff(reste,step.reg2[[i]])->reste
        }
      }
      if(choix=="Modele complet - avec interaction"){ 
        as.formula(paste0(names(data)[VD]," ~ ",names(data)[X[1]]))->modele # on cree le modele le plus simple, donc un seul predicteur (seul le modele est ecrit mais on n en fait encore rien)
        lm(modele,na.action=na.exclude, data=data)->lm.r1 # on cree le modele lineaire general avec notre modele simpliste
        if(length(X)>1) {
          for(i in 1:(length(VI)-1)){
            update(lm.r1, as.formula(paste0(".~.*",names(data)[X[i+1]])))->lm.r1
          }
        }
        
        step.reg2[[1]]<- dlgList(attributes(lm.r1$terms)$term.labels, preselect=NULL, multiple = TRUE, title="Variable(s) de cette etape")$res
        setdiff(attributes(lm.r1$terms)$term.labels,step.reg2[[1]])->reste
        i<-1
        while(length(reste!=0)){i+1->i
          step.reg2[[i]]<-dlgList(reste, multiple = TRUE,title="Variable(s) de cette etape")$res
          setdiff(reste,step.reg2[[i]])->reste
        }
      }
      
    }
    print("si vous desirez realiser la meme regressions pour differents sous-groupes, vous pouvez utiliser l option par groupe",quote=F)
    dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Faut-il des analyses par groupe?")$res->groupe 
    if(length(groupe)==0) groupe<-FALSE
    if(groupe){
      groupe<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = FALSE, 
                      title="Specifier la variable groupe")$res
      if(length(groupe)==0) groupe<-NULL else  subset(listes, listes[,1] %in% groupe)[,2]->groupe
    }else groupe<-NULL
    
    dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="voulez vous sauvegarder?")$res->sauvegarde
    if(length(sauvegarde)==0) sauvegarde<-FALSE
    
    reg.simple<-function(VD, VI, data, autres.options, method, type.selection, f.in){
      
      list()->Resultats
      psych::describe(data[,c(VI, VD)])->Resultats$"Statistiques descriptives"
      as.formula(paste0(names(data)[VD]," ~ ",names(data)[VI[1]]))->modele # on cree le modele le plus simple, donc un seul predicteur (seul le modele est ecrit mais on n en fait encore rien)
      lm(modele,na.action=na.exclude, data=data)->lm.r1 # on cree le modele lineaire general avec notre modele simpliste
      mod <- list() # on cree un liste appelee mod. Celle-ci est vide
      lm.r1->mod[[1]]
      
      if(choix=="Effets simples ou multiples"){ 
        if(length(VI)==1) mod->mod else 
          for(i in 1:(length(VI)-1)){update(modele, as.formula(paste0(".~.+",names(data)[VI[i+1]])))->modele
            update(lm.r1, as.formula(paste0(".~.+",names(data)[VI[i+1]])))->lm.r1
            lm.r1->mod[[i+1]]}}
      
      
      if(choix=="Modele complet - avec interaction"){
        if(length(VI)==1) mod->mod else {
          for(i in 1:(length(VI)-1)){update(modele, as.formula(paste0(".~.*",names(data)[VI[i+1]])))->modele
            update(lm.r1, as.formula(paste0(".~.*",names(data)[VI[i+1]])))->lm.r1
            lm.r1->mod[[i+1]]}} # on stocke a chaque fois le modele cree dans la liste mod, a un nouvel emplacement
        
        mod[[length(VI)]]->modele_complet
        as.formula(paste0(names(data)[VD]," ~ ",names(data)[VI[1]]))->modele1
        lm(modele1,na.action=na.exclude, data=data)->lm.r1
        for(i in 1:length(attributes(modele_complet$terms)$term.labels)-1){update(lm.r1, as.formula(paste0(".~.+",attributes(modele_complet$terms)$term.labels[i+1])))->lm.r1
          lm.r1->mod[[i+1]]}
      }
      
      assign(x="lm.r1b", value=lm.r1, envir=.e)
      resid(lm.r1)->data$residu
      if(length(data$residu)<5000){
        shapiro.test(data$residu)->Shapiro_Wilk # realise le Shapiro-Wilk
        lillie.test(data$residu)->Lilliefors  # realise le Lilliefors
        round(data.frame(Shapiro_Wilk$statistic,Shapiro_Wilk$p.value, Lilliefors$statistic, Lilliefors$p.value),4)->normalite
        names(normalite)<-c("W de Shapiro-Wilk", "valeur.p SW", "D de Lilliefors", "valeur.p Llfrs")
        dimnames(normalite)[1]<-" "
        format(normalite, width = max(sapply(names(normalite), nchar)), justify = "centre")->Resultats$"Tests de normalite" }
      if(length(VI)>1)  {mult.norm(data[,VI[sapply(data[,VI], class)!="factor"]], chicrit = 0.005)$mult.test->Resultats$"Normalite multivariee"
        1/vif(lm.r1)->tolerance # calcul de la tolerance. La fonction n existe pas, on passe par le facteur d inflation de la variance
        vif(lm.r1)->FIV # calcul du facteur d inflation de la variance 
        round(cbind(tolerance, FIV),4)->Resultats$Tolerance$Tests
        "FIV : facteur d inflation de la variance"->Resultats$Tolerance$Information
        dwt(lm.r1, simulate=TRUE, method= "normal", reps=500)->DWT.results
        round(data.frame(DWT.results[1],DWT.results[2],DWT.results[3]),4)->DWT.results
        names(DWT.results)<-c("Autocorrelation","statistique de D-W", "valeur p" )
        Resultats$"Test de Durbin-Watson - autocorrelations"<-DWT.results}
      
      
      c(NA,NA,NA)->r_carre
      summary(mod[[1]])$r.squared->r_carre2 # on recupere les R.deux pour le premier modele afin de quantifier l amelioration en R.deux de chaque variable
      diff<-r_carre2[1]
      summary(mod[[1]])$adj.r.squared->r_carre_adj
      round(c(r_carre2, diff, r_carre_adj), 4)->r_carre2
      rbind(r_carre,r_carre2 )->r_carre
      while(length(r_carre[,1])!= length(summary(mod[[1]])$coefficients[,1])){rbind(r_carre, c(NA,NA,NA))->r_carre}
      
      if(length(VI)>1){
        for (i in 1:(length(mod)-1)){
          summary(mod[[i+1]])$r.squared->r_carre2
          summary(mod[[i+1]])$r.squared-summary(mod[[i]])$r.squared->diff
          summary(mod[[i+1]])$adj.r.squared->r_carre_adj
          round(c(r_carre2, diff, r_carre_adj), 4)->r_carre2
          rbind(r_carre,r_carre2 )->r_carre
          while(length(r_carre[,1])!= length(summary(mod[[i+1]])$coefficients[,1])){rbind(r_carre, c(NA,NA,NA))->r_carre}
        } }
      
      
      dimnames(r_carre)[[1]]<-NULL
      dimnames(r_carre)[[2]]<-c("R.deux", "Delta R.deux", "R.deux adj")
      
      data.frame(summary(lm.r1)$coefficients)->table # fournit le b, le t et la valeur de la probabilite. On le stocke dans table
      round(table[,1:4],3)->table # on arrondit les valeurs a 3 decimales 
      c("",round(lm.beta(lm.r1),3))->table$beta # fournit les betas qu on inclut a la table 
      names(table)<-c("b","se","t.value","valeur.p","beta")
      
      c(summary(lm.r1)$sigma, summary(lm.r1)$r.squared, summary(lm.r1)$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
      pf(summary(lm.r1)$fstatistic[1], summary(lm.r1)$fstatistic[2],summary(lm.r1)$fstatistic[3], lower.tail=F)->p.value #permet de savoir si le F est significatif
      c(significativite_modele , p.value)->modele.F # on combine les precedents 
      round(modele.F,3)->modele.F # on arrondit les nombres a la 3e decimale
      c("Erreur residuelle", "R.deux", "F", "Ddl (num)", "Ddl (dnom)","valeur.p")->names(modele.F)# attribue le nom aux colonnes
      modele.F->Resultats$"Estimation globale du modele"
      data.frame(table,r_carre)->table
      table[is.na(table)]<-""
      table->Resultats$"table des betas"
      
      rlm(formula=modele, data=data)->modele_robuste
      summary(modele_robuste)->res_modele_robuste
      (1-pt(abs(res_modele_robuste$coefficients[,3]), (length(data[,1])-1-length(VI)), lower.tail=TRUE))*2->proba
      round(cbind(res_modele_robuste$coefficients, proba),3)->M_estimator
      data.frame(M_estimator)->M_estimator
      names(M_estimator)<-c("b (M estimator)", "SE", "t.value", "p.valeur")
      M_estimator->Resultats$"Statistiques robustes"
      
      
      
      if(nboot>1){ 
        bootReg<-function(formula, data, i)
        {  d <- data[i,]
        fit <- lm(formula, data = d)
        return(coef(fit))}
        bootResults<-boot(statistic=bootReg, formula= modele , data=data, R=nboot) # cree le bootstrap
        intervalle<-c()
        if(nboot<5000){
          for(i in 1: length(lm.r1$coefficients)){boot.ci(bootResults, type = "perc", index = i)$percent[,4:5]->resultats
            rbind(intervalle, resultats)->intervalle}
          data.frame(M_estimator, round(intervalle,4))->Statistiques_robustes
          names(Statistiques_robustes)<-c("b (M estimator)", "SE", "t.value", "p.valeur", "Percentile lim inf", "Percentile lim sup")
        }else{
          for(i in 1: length(lm.r1$coefficients)){boot.ci(bootResults, type = "bca", index = i)->resultats
            rbind(intervalle, resultats$bca)->intervalle}
          data.frame(M_estimator, round(intervalle[,4:5],4))->Statistiques_robustes
          names(Statistiques_robustes)<-c("b (M estimator)", "SE", "t.value", "p.valeur", "Bca lim inf", "Bca lim sup")}
        Statistiques_robustes->Resultats$"Statistiques robustes"
        Resultats$"Information bootstrap : pour un reechantillonnage inferieur a 5000, la methode est celle du percentile et celle du BCa pour plus de 5000"}
      
      
      if(autres.options=="Methodes de selection"){
        if(any(method=="F d entree")){
          if(type.selection=="Forward - pas-a-pas ascendant")  Resultats$"Methode de selection - F entree"<-mle.stepwise(modele, data, type="Forward", model=T,f.in=f.in, x=T, y=T) # calcul du F
          if(type.selection=="Backward- pas-a-pas descendant") Resultats$"Methode de selection - F entree"<-mle.stepwise(modele, data, type="Backward", model=T,f.in=f.in, x=T, y=T) # calcul du F
          if(type.selection=="Bidirectionnel")  Resultats$"Methode de selection - F entree"<-mle.stepwise(modele, data, type="Stepwise", model=T,f.in=f.in, x=T, y=T)
        }
        
        if(any(method=="Critere d information d Akaike")){ 
          if(type.selection=="Forward - pas-a-pas ascendant"){ step_forward <- stepAIC(lm.r1, direction="forward")  ## ascendant  # calcul du AIC
          step_forward$anova->Resultats$"Methode de selection - criteres d information d Akaike"}
          if(type.selection=="Backward- pas-a-pas descendant"){step_2directions<-stepAIC(lm.r1, direction="backward")
          step_2directions$anova->Resultats$"Methode de selection - criteres d information d Akaike"}
          if(type.selection=="Bidirectionnel")  {step_2directions<-stepAIC(lm.r1, direction="both")
          step_2directions$anova->Resultats$"Methode de selection - criteres d information d Akaike"}
        }
      }
      
      if(autres.options=="Modeles hierarchiques"){
        as.formula(paste0(names(data)[VD]," ~ ",step.reg2[[1]][1]))->modele.H
        list()->modele.H1
        for(i in 1:length(step.reg2)){
          
          for(j in 1:length(step.reg2[[i]])){update(modele.H, as.formula(paste0(".~.+",step.reg2[[i]][j])))->modele.H}
          lm(modele.H, data=data, na.action=na.exclude )->lm.H
          lm.H->modele.H1[[i]]}
        
        anova(modele.H1[[1]],modele.H1[[2]])->Resultats$"Modeles hierarchique - amelioration du modele a chaque etape"[[1]]
        if(length(modele.H1)>2){
          for(i in 1:(length(modele.H1)-2))anova(modele.H1[[i+1]], modele.H1[[i+2]])->Resultats$"Modeles hierarchique - amelioration du modele a chaque etape"[[1+i]]
        }
        paste("Etape", 1:length(Resultats$"Modeles hierarchique - amelioration du modele a chaque etape"), "vs. etape", 
              2:(length(Resultats$"Modeles hierarchique - amelioration du modele a chaque etape")+1))->names(Resultats$"Modeles hierarchique - amelioration du modele a chaque etape")
        
        
        c(summary(modele.H1[[1]])$sigma, summary(modele.H1[[1]])$r.squared, summary(modele.H1[[1]])$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
        pf(summary(modele.H1[[1]])$fstatistic[1], summary(modele.H1[[1]])$fstatistic[2],summary(modele.H1[[1]])$fstatistic[3], lower.tail=F)->p.value #permet de savoir si le F est significatif
        c(significativite_modele , p.value)->modele_avec_outliers  
        
        for(i in 1:(length(modele.H1)-1)){
          c(summary(modele.H1[[i+1]])$sigma, summary(modele.H1[[i+1]])$r.squared, summary(modele.H1[[i+1]])$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
          pf(summary(modele.H1[[i+1]])$fstatistic[1], summary(modele.H1[[i+1]])$fstatistic[2],summary(modele.H1[[i+1]])$fstatistic[3], lower.tail=F)->valeur.p #permet de savoir si le F est significatif
          rbind(modele_avec_outliers, c(significativite_modele , p.value))->modele_avec_outliers  
        }
        round(modele_avec_outliers,3)->modele_avec_outliers 
        c("Erreur residuelle", "R.deux", "F", "Ddl(1)", "Ddl(2)","valeur.p")->dimnames(modele_avec_outliers)[[2]]
        paste("etape", 1:length(modele_avec_outliers[,1]))->dimnames(modele_avec_outliers)[[1]]
        Resultats$"Modeles hierarchique - significativite du modele complet a chaque etape"<-modele_avec_outliers
        
      }
      
      if(autres.options=="Validation croisee") CVlm(data=data, form.lm=formula(modele), m=2,printit=FALSE, plotit="Observed")
      return(Resultats)
    }
    
    
    reg.simple(VD=VD, VI=X, data=data, autres.options=autres.options,type.selection=type.selection, f.in=f.in, method=method)->complete1
    
    if(any(desires=="Donnees completes"))  complete1->Resultats$"Donnees completes"
    if(any(desires=="Identification des outliers")| any(desires=="Donnees sans valeur influente")){   
      influence.measures(lm.r1b)->mesure_influence
      data.frame(mesure_influence$infmat)->mes_inf # on garde uniquement les informations relatives aux valeurs influentes
      cbind(data, mes_inf)->data # on les integres a data2
      rstudent(lm.r1b)->data$residus_studentise # idem avec le residu studentise
      rstandard(lm.r1b)->data$residus_standardise # idem avec le residu standardise 
      4/length(data[,1])->seuil_cook # fixe le seuil pour les valeurs aberrantes 
      data[which(data$cook.d<= seuil_cook), ]->nettoyees 
      
      if(any(desires=="Identification des outliers")){
        length(data[,1])-length(nettoyees[,1])->N_retire # identifier le nombre d observations retirees sur la base de la distance de cook
        paste(N_retire/length(data[,1])*100,"%")->Pourcentage_retire # fournit le pourcentage retire
        data.frame(N_retire, Pourcentage_retire)->Resultats$"Synthese du nombre d observations considerees comme influentes"
        data[which(data$cook.d>= seuil_cook), ]->outliers
        cbind(outliers[,X],outliers$cook.d)->Resultats$"Observations considerees comme influentes sur la base de la distance de Cook - 4/N" }
      
      if(any(desires=="Donnees sans valeur influente"))reg.simple(VD=VD, VI=X, data=nettoyees, autres.options=autres.options,
                                                                  type.selection=type.selection, f.in=f.in,method=method)->Resultats$"Donnees nettoyees"
    }
    
    if(length(groupe)>0){split(data, data[,groupe])->groupe
      list()->analyse.par.groupe
      for(i in 1:length(groupe)){
        names(groupe)[i]->analyse.par.groupe[[1]]
        groupe[[i]]->data
        reg.simple(VD=VD, VI=X, data=data, autres.options=autres.options,type.selection=type.selection, f.in=f.in, method=method)->completes1
        if(any(desires=="Donnees completes"))  completes1->analyse.par.groupe$"Donnees completes"
        if(any(desires=="Identification des outliers")| any(desires=="Donnees sans valeur influente")){   
          influence.measures(lm.r1b)->mesure_influence
          data.frame(mesure_influence$infmat)->mes_inf # on garde uniquement les informations relatives aux valeurs influentes
          cbind(data, mes_inf)->data # on les integres a data2
          rstudent(lm.r1b)->data$residus_studentise # idem avec le residu studentise
          rstandard(lm.r1b)->data$residus_standardise # idem avec le residu standardise 
          4/length(data[,1])->seuil_cook # fixe le seuil pour les valeurs aberrantes 
          data[which(data$cook.d<= seuil_cook), ]->nettoyees 
          
          if(any(desires=="Identification des outliers")){
            length(data[,1])-length(nettoyees[,1])->N_retire # identifier le nombre d observations retirees sur la base de la distance de cook
            paste(N_retire/length(data[,1])*100,"%")->Pourcentage_retire # fournit le pourcentage retire
            data.frame(N_retire, Pourcentage_retire)->Resultats$"Synthese du nombre d observations considerees comme influentes"
            data[which(data$cook.d>= seuil_cook), ]->outliers
            cbind(outliers[,X],outliers$cook.d)->analyse.par.groupe$"Observations considerees comme influentes sur la base de la distance de Cook - 4/N" }
          
          if(any(desires=="Donnees sans valeur influente"))reg.simple(VD=VD, VI=X, data=nettoyees, autres.options=autres.options,
                                                                      type.selection=type.selection, f.in=f.in,method=method)->analyse.par.groupe$"Donnees nettoyees"}
        
        analyse.par.groupe->Resultats[[(length(Resultats)+1)]]
        
        
        
      }
      
    }
    
    
    if(sauvegarde) save(Resultats=Resultats, choix=choix, env=.e)
  }
  return(Resultats)
}


#### RÃ©gressions logistiques ####
# ajouter les modÃ¨les multinomiques
# laisser la possibilitÃ© de faire d'autres distributions 
regressions.log<-function(){
  #. ajouter sauver
  options (warn=-1)
  c("boot","car","psych", "mlogit","svDialogs","rms","MASS")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages)
    require(packages)}
  list()->Resultats
  writeLines("veuillez choisir le type de modele que vous desirez realiser")
  dlgList(c("Effets simples ou multiples", "Modele complet - avec interaction"), preselect="Effets simples ou multiples", multiple = FALSE, title="Quelle type d analyse ?")$res->choix
  if(length(choix)==0) return(analyse())
  X<-"autres donnees"
  while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
  if(length(data)==0) return(regressions.log())
  data<-get(data)
  X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees","annuler"), multiple = TRUE,
             title="Predicteur(s)")$res
  if(length(X)==0) X<-"autres donnees"
  if(any(X=="annuler")) return(regressions.log())}
  listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
  subset(listes, listes[,1] %in% X)[,2]->X
  
  writeLines("L ordre d entree des variables est important pour le calcul du maximum de vraisemblance. Veuillez
             preciser l'ordre d'entree")
  V1<-dlgList(names(data[,X]), multiple = FALSE,title="Quelle variable Ã  cette etape")$res
  setdiff(names(data[,X]),V1)->reste
  which(names(data) %in% V1)->X
  while(length(reste)!=0){
    V1<-dlgList(reste, multiple = FALSE,title="Quelle variable Ã  cette etape")$res
    c(X,which(names(data) %in% V1))->X
    setdiff(reste,V1)->reste
  }
  
  writeLines("Veuillez choisir la variable dependante. Elle doit etre dichotomique")
  Y<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = FALSE,
             title="Variable dependante")$res
  if(length(Y) == 0L) return(regressions.log())
  subset(listes, listes[,1] %in% Y)[,2]->Y
  data[complete.cases(data[,c(Y,X)]),]->data
  
  if(class(data[,Y])!="factor" && (min(data[,Y])!=0|max(data[,Y])!=1)){
    
    if(class(data[,Y])!="factor") {dlgMessage("La variable dependante n est pas dichotomique, voulez vous la convertir ?","yesno")$res->conv
      if(conv=="no") return(regressions.log())  else{
        dlgList(c("Mediane", "Seuil"), preselect="Mediane", multiple = FALSE, title="Quel critere de codage voulez vous ?")$res->codage
        if(length(codage)==0) codage<-"Mediane"
        a<-dlgInput("Etiquette pour les valeurs superieures", "haut")$res
        if(length(a)==0) a<-"haut"
        strsplit(a, ":")->a
        tail(a[[1]],n=1)->a
        b<-dlgInput("Etiquette pour les valeurs inferieures ou egales", "bas")$res
        if(length(b)==0) b<-"bas"
        strsplit(b, ":")->b
        tail(b[[1]],n=1)->b
        if(codage=="Mediane") {data[,Y][data[,Y]>median(data[,Y])]<-a
        data[,Y][data[,Y]!=a]<-b
        } # mediane
        if(codage=="Seuil") {seuil<-dlgInput("Veuillez preciser le seuil de separation", median(data[,Y]))$res
        if(length(seuil)==0) seuil<-as.character(median(data[,Y]))
        strsplit(seuil, ":")->seuil
        tail(seuil[[1]],n=1)->seuil
        as.numeric(seuil)->seuil
        data[,Y][data[,Y]>seuil]<-a
        data[,Y][data[,Y]!=a]<-b
        } # seuil
        factor(data[,Y])->data[,Y]
      } # ce qui se passe quand on n annule pas
    } # conversion de numerique
    
    if(class(data[,Y])=="factor" & nlevels(data[,Y])!=2)  {dlgMessage("La variable dependante n est pas dichotomique, voulez vous faire des regroupements ?","yesno")$res->reg
      if(reg=="no") return(analyse()) else {
        reg<- dlgList(levels(data[,Y]), preselect=NULL, multiple = TRUE, title="Modalites a regrouper")$res
        setdiff(levels(data[,Y]),reg)->reste
        a<-dlgInput("Etiquette pour les modalites regroupees", " ")$res
        if(length(a)==0) a<-"regroupement1"
        strsplit(a, ":")->a
        tail(a[[1]],n=1)->a
        b<-dlgInput("Etiquette pour les autres modalites", " ")$res
        if(length(b)==0) b<-"regroupement2"
        strsplit(b, ":")->b
        tail(b[[1]],n=1)->b
        data[,Y] <- factor(data[,Y], levels=c(levels(data[,Y]), c(a,b)))
        data[which(data[,Y] %in% reg),Y]<-a
        data[which(data[,Y] %in% reste),Y]<-b
        factor(data[,Y])->data[,Y]
      }
    }
    
  } #pas facteur et pas 1 ou 0
  
  
  
  desires<- dlgList(c("Donnees completes",
                      "Identification des outliers",
                      "Donnees sans valeur influente"),
                    preselect=c("Donnees completes",
                                "Identification des outliers",
                                "Donnees sans valeur influente"),
                    multiple = TRUE, title="Quels resultats voulez vous obtenir ?")$res
  if(length(desires)==0) desires<-c("Donnees completes",
                                    "Identification des outliers",
                                    "Donnees sans valeur influente")
  
  autres.options<- dlgList(c("Methodes de selection","Aucune", "annuler"),
                           preselect=c("Aucune"),
                           multiple = FALSE, title="Autres options?")$res
  if(length(autres.options)==0) autres.options<-"Aucune"
  if(autres.options=="annuler") return(regressions.log())
  if(autres.options=="Methodes de selection"){
    type.selection<- dlgList(c("Forward - pas-a-pas ascendant","Backward- pas-a-pas descendant", "Bidirectionnel"),
                             preselect=NULL,
                             multiple = FALSE, title="Choix de la methode")$res
    if(length(type.selection)==0) type.selection<-"Backward- pas-a-pas descendant"
  }else type.selection<-NULL
  
  .e <- environment()
  dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="voulez vous sauvegarder?")$res->sauvegarde
  if(length(sauvegarde)==0) sauvegarde<-FALSE
  data[complete.cases(data[,c(Y,X)]),]->data
  
  dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="voulez vous les probabilites?")$res->proba
  if(length(proba)==0) proba<-FALSE
  
  if(proba) {fichier<- dlgInput("Quel nom voulez vous pour la base de donnees ?", "data1")$res
  if(length(fichier)==0) fichier<-"data1"
  strsplit(fichier, ":")->fichier
  tail(fichier[[1]],n=1)->fichier}
  
  logisticPseudoR2s <- function(LogModel) {
    dev <- LogModel$deviance
    nullDev <- LogModel$null.deviance
    modelN <-  length(LogModel$fitted.values)
    R.l <-  1 -  dev / nullDev
    R.cs <- 1- exp ( -(nullDev - dev) / modelN)
    R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
    return(c(round(R.l, 3),round(R.cs, 3),round(R.n, 3)))
  }
  
  reg.log2<-function(X, Y, data,autres.options,type.selection, proba, choix){
    options (warn=-1)
    list()->Resultats
    summary(data[,c(X,Y)])->Resultats$"Statistiques descriptives"
    
    as.formula(paste0(names(data)[Y]," ~ ",names(data)[X[1]]))->modele
    glm(modele, data=data, family="binomial")->glm.r1
    mod <- list() # creation d une liste vide appelee "mod"
    glm.r1->mod[[1]] #on stocke le premier modele dans la liste
    if(choix=="Effets simples ou multiples"){
      for(i in 1:(length(X)-1)){update(glm.r1, as.formula(paste0(".~.+",names(data)[X[i+1]])))->glm.r1
        glm.r1->mod[[i+1]]}
    }
    if(choix=="Modele complet - avec interaction"){for(i in 1:(length(X)-1)){update(glm.r1, as.formula(paste0(".~.*",names(data)[X[i+1]])))->glm.r1
      glm.r1->mod[[i+1]]}
    }
    assign(x="glm.r1b", value=glm.r1, envir=.e)
    
    anova(mod[[length(mod)]])->Amelioration_du_MV
    
    summary(mod[[length(mod)]])->resultats
    as(resultats$call,"character")->texte
    paste("le modele teste est", texte[1],"(",texte[2],",",texte[3],",", texte[4],")")->Resultats$"Modele teste"
    
    rbind(rms::vif(mod[[length(mod)]]), 1/rms::vif(mod[[length(mod)]]))->MC
    dimnames(MC)[[1]]<-c("Facteur d inflation de la variance", "Tolerance")
    t(round(MC,4))->Resultats$"Test de multicolineratite"
    
    sum(Amelioration_du_MV$Df[2:length(Amelioration_du_MV$Df)])->ddl
    Amelioration_du_MV$`Resid. Dev`[1]-Amelioration_du_MV$`Resid. Dev`[length(Amelioration_du_MV$`Resid. Dev`)]->chi.carre.modele
    round(1-pchisq(chi.carre.modele,ddl),4)->valeur.p
    logisticPseudoR2s(mod[[length(mod)]])->Pseudo.R.carre
    data.frame(chi.carre.modele, ddl, valeur.p,Pseudo.R.carre[1],Pseudo.R.carre[2],Pseudo.R.carre[3])->mod.glob
    names(mod.glob)<-c("chi.carre.modele", "ddl", "valeur.p","Hosmer and Lemeshow R^2","Cox and Snell R^2","Nagelkerke R^2")
    mod.glob->Resultats$"Significativite du modele global"
    
    
    Amelioration_du_MV$chi.deux.prob<-1-pchisq(Amelioration_du_MV$Deviance, Amelioration_du_MV$Df)
    round(Amelioration_du_MV,4)->Amelioration_du_MV
    names(Amelioration_du_MV)<-c("ddl predicteur", "MV","ddl.residuels","MV residuel","valeur.p")
    data.frame(Amelioration_du_MV)->Resultats$"Amelioration de la vraisemblance pour chaque variable"
    
    
    data.frame(resultats$coefficients)->table
    (table$z.value)^2->table$Wald.statistic
    exp(table$Estimate)->table$Odd.Ratio
    round(table,4)->table
    names(table)<-c("b","Erreur.standard","valeur.Z","p.Wald", "Wald","Odd.ratio")
    cbind(table, round(exp(confint(mod[[length(mod)]])),4))->table
    table$interpretation<-c(rep("", times=length(table[,1])))
    table$interpretation[which(table$Odd.ratio>=1)]<-paste(table$Odd.ratio[which(table$Odd.ratio>=1)], "fois plus")
    table$interpretation[which(table$Odd.ratio<1)]<-paste(round(1/table$Odd.ratio[which(table$Odd.ratio<1)],4), "fois moins")
    table->Resultats$"Table des coefficients"
    
    R_sq<-NULL
    for(i in 1:length(mod)){logisticPseudoR2s(mod[[i]])->R_squared
      rbind(R_sq, R_squared)->R_sq}
    diff(R_sq,lag=1)->R_sq[2.]
    dimnames(R_sq)[[1]]<-names(data)[X]
    dimnames(R_sq)[[2]]<-c("Hosmer and Lemeshow R^2","Cox and Snell R^2","Nagelkerke R^2")
    R_sq->Resultats$"Delta du pseudo R carre"
    
    if(proba)	{round(fitted(mod[[length(mod)]]),4)->data$"Probabilites predites"
      assign(x=fichier, value=data, envir=.GlobalEnv)}
    
    if(autres.options=="Methodes de selection"){
      if(type.selection=="Forward - pas-a-pas ascendant"){ step_forward <- stepAIC(glm.r1, direction="forward")  ## ascendant  # calcul du AIC
      step_forward$anova->Resultats$"Methode de selection - critere d information d Akaike"}
      if(type.selection=="Backward- pas-a-pas descendant"){step_2directions<-stepAIC(glm.r1, direction="backward")
      step_2directions$anova->Resultats$"Methode de selection - critere d information d Akaike"}
      if(type.selection=="Bidirectionnel")  {step_2directions<-stepAIC(glm.r1, direction="both")
      step_2directions$anova->Resultats$"Methode de selection - critere d information d Akaike"}
      
    }
    return(Resultats)
  }
  reg.log2(X=X, Y=Y, data=data, autres.options= autres.options,type.selection=type.selection, proba=proba,choix=choix)->complete1
  
  if(any(desires=="Donnees completes"))  complete1->Resultats$"Donnees completes"
  if(any(desires=="Identification des outliers")| any(desires=="Donnees sans valeur influente")){
    influence.measures(glm.r1b)->mesure_influence
    data.frame(mesure_influence$infmat)->mes_inf # on garde uniquement les informations relatives aux valeurs influentes
    cbind(data, mes_inf)->data # on les integres e data2
    rstudent(glm.r1b)->data$residus_studentise # idem avec le residu studentise
    rstandard(glm.r1b)->data$residus_standardise # idem avec le residu standardise
    4/length(data[,1])->seuil_cook # fixe le seuil pour les valeurs aberrantes
    data[which(data$cook.d<= seuil_cook), ]->nettoyees
    
    if(any(desires=="Identification des outliers")){
      length(data[,1])-length(nettoyees[,1])->N_retire # identifier le nombre d observations retirees sur la base de la distance de cook
      paste(N_retire/length(data[,1])*100,"%")->Pourcentage_retire # fournit le pourcentage retire
      data.frame(N_retire, Pourcentage_retire)->Resultats$"Synthese du nombre d observations considerees comme influentes"
      data[which(data$cook.d>= seuil_cook), ]->outliers
      cbind(outliers[,X],outliers$cook.d)->Resultats$"Observations considerees comme influentes sur la base de la distance de Cook - 4/N" }
    
    if(any(desires=="Donnees sans valeur influente")) reg.log2(X=X, Y=Y, data=nettoyees, autres.options=autres.options,type.selection=type.selection, proba=proba,choix=choix)->Resultats$"Donnees nettoyees"
    
    
  }
  
  if(sauvegarde) save(Resultats=Resultats, choix="regressions.logistiques", env=.e)
  return(Resultats)
}


#### statistiques descriptives ####
# un peu rudimentaire, mÃ©riterait d'Ãªtre dÃ©veloppÃ©e un peu 
stat.desc<-function(X=NULL, groupes=NULL, data=NULL, tr=.1, type=3){options (warn=-1)
  c( "psych", "svDialogs")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  if(!is.null(X) && class(X)!="character") data.frame(X)->data
  if(class(X)=="character") match(X,names(data))->X
  if(class(groupes)=="character") match(groupes,names(data))->groupes
  if(!is.null(groupes)) length(data.frame(groupes))->n.gr
  
  if(is.null(X) |any(c(X,groupes)>ncol(data))) { 
    print("veuillez choisir les donnees")
    X<-"autres donnees"
    while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
    if(length(data)==0) return(analyse())
    data<-get(data)
    print("veuillez choisir les variables dont il faut faire les statistiques descriptives")
    X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees"), multiple = TRUE, 
               title="Selectionnez variables")$res
    if(length(X)==0) X<-"autres donnees"
    }
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% X)[,2]->X
    writeLines("Vous pouvez decomposer les statistiques descriptives par sous-groupe en choisissant une 
               ou plusieurs variables categorielles")
    groupes<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 
                       "aucun"), preselect="aucun", multiple = TRUE, title="Variable(s)  groupes ")$res
    if(length(groupes) == 0L) groupes<-"aucun"
    print("Vous pouvez specifier la troncature et les parametre pour l'aplatissement et l'asymetrie en choisissant autres options")
    autres<-dlgList(c("TRUE", "FALSE"), multiple = FALSE, title="Specifier autres options?")$res
    
    if(autres) dlgForm(list("Troncature:NUM"=0.1, "Type de skew et kurtosis:NUM"=3),  "Veuillez fixer le seuil de la troncature")$res->opts2
    if(exists("opts2"))  {tr=opts2[[1]]
    type=opts2[[2]]} else {tr<-0.1
    type<-3}
    if(!any(1:3 %in% type)) type<-3
    if(!any(0:0.5 %in% tr)) tr<-0.1
    
    if(groupes!="aucun") {length(groupes)->n.gr
      subset(listes, listes[,1] %in% groupes)[,2]->groupes 
      data[,groupes]->groupes} else groupes<-NULL}
  if(!is.null(groupes) && any(sapply(groupes, class)!="factor")) {sapply(groupes, factor)->groupes
    data.frame(groupes)->groupes
    print("certaines variables ont ete converties en facteur", quote=FALSE)}
  length(X)->n.var
  data<-data[,X]
  if(n.var==1 & class(data)=="factor") {data->categ 
    NULL->data} else { 
      if(any(sapply(data, class)=="factor")) {data[,which(sapply(data, class)=="factor")]->categ 
        data[,-which(sapply(data, class)=="factor")]->data }  else categ<-NULL}
  
  
  list()->Resultats
  if(!is.null(data)) describeBy(data, group=groupes,mat=(!is.null(groupes)),type=type,digits=15, check=FALSE,skew = TRUE, 
                                ranges = TRUE,trim=tr)->Resultats$'Variables numeriques'
  if(!is.null(categ)) {
    if(is.null(groupes)) summary(categ, maxsum=5)->Resultats$'Variables categorielles'else {
      
      if(n.gr==1) {
        if(n.var==1)tapply(categ, groupes, summary)->Resultats$'Variables categorielles'else { 
          for(i in 1: length(categ)){
            tapply(categ[[i]], groupes, summary)->Resultats$'Variables categorielles'[[i]]}}} else {
              list()->niveaux
              for(i in 1:n.gr) levels(groupes[,i])->niveaux[[i]]
              expand.grid(niveaux)->combin
              if(n.var==1){tapply(categ, groupes, summary)->rr
                matrix(unlist(rr), ncol=nlevels(categ))->rr
                data.frame(combin, rr)->rr
                names(rr)<-c(names(groupes), levels(categ))
                rr->Resultats$'Variables categorielles'} else { 
                  for(i in 1:length(categ)){tapply(categ[,i], groupes, summary)->rr
                    matrix(unlist(rr), ncol=nlevels(categ[,i]))->rr
                    data.frame(combin, rr)->rr
                    names(rr)<-c(names(groupes), levels(categ[,i]))
                    rr->Resultats$'Variables categorielles'[[i]]}} }
    }}  
  
  return(Resultats)
  
}
#### toutes les formes de t de Student ####

test.t<-function(X=NULL, Y=NULL, group=NULL, 
                 sauvegarde=F, desires=c("Donnees completes", 
                                         "Identification des outliers", 
                                         "Donnees sans valeur influente"), critere="Grubbs", z=3.26, data=NULL,
                 alternative="two.sided", mu=NULL, paired = FALSE, conf.level=.95, 
                 formula=NULL, subset=NULL,nboot=1000, param=c("param", "non param", "robustes"), info=TRUE){
  options (warn=-1) 
  # chargement des packages
  packages<-c("svDialogs", "outliers", "nortest","psych", "lsr","ggplot2", "reshape2", "car", "plyr")
  # corriger pour l installation de WRS
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  if(require(WRS)==FALSE) {
    install.packages(c("MASS", "akima", "robustbase"))
    install.packages(c("cobs", "robust", "mgcv", "scatterplot3d", "quantreg", "rrcov", "lars", "pwr", "trimcluster", "parallel", "mc2d", "psych", "Rfit"))
    install.packages("WRS", repos="http://R-Forge.R-project.org", type="source")}
  # choix de l analyse
  .e <- environment()
  Resultats<-list()
  if(info==TRUE) print("quelle analyse voulez-vous realiser ?")
  dlgList(c("Comparaison a une norme", 
            "Deux echantillons apparies",
            "Deux echantillons independants"), preselect=NULL, multiple = FALSE, title="Comparaison de moyennes")$res->choix
  if(length(choix)==0) return(analyse())
  if(info==TRUE) writeLines("param est le modele paramatrique qui renvoie le test t classique, 
                            \n non param calcule le test de Wilcoxon (ou Mann-Whitney), 
                            \n robustes permet de calculer des test t sur des medianes ou des moyennes tronquÃ©es avec ou sans bootstrap")  
  param<- dlgList(c("param", "non param", "robustes"), 
                  preselect=c("param", "non param", "robustes"),
                  multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res 
  if(info==TRUE) writeLines("les donnees completes representent l analyse classique sur toutes les donnes utilisables, L identification des outliers
                            \n permet d'identifier les observations qui sont considerees statistiquement comme influenÃ§ant les donnees. Le critere utilise est le test de Grubbs.
                            \n Il est preferable d eviter de choisir cette option si vous avez beaucoup de variables. La lecture du tableau peut Ãªtre genee. 
                            \nles analyses sur les donnees sans les valeurs influentes realise l analyse apres suppression des valeurs influentes sur la base du test 
                            \n de Grubbs. Cette option stocke vos donnees sans les valeurs influentes dans un objet appele nettoyees")  
  
  desires<- dlgList(c("Donnees completes", 
                      "Identification des outliers", 
                      "Donnees sans valeur influente"), 
                    preselect=c("Donnees completes", 
                                "Identification des outliers", 
                                "Donnees sans valeur influente"),
                    multiple = TRUE, title="Quels Resultats voulez vous obtenir ?")$res
  if(length(desires)==0) desires<-c("Donnees completes", "Identification des outliers", "Donnees sans valeur influente")
  
  
  if(choix=="Comparaison a une norme"){
    # choix des variables
    if(info==TRUE) writeLines("Veuillez choisir la base de donnees")
    X<-"autres donnees"
    while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
    if(length(data)==0) return(test.t())
    data<-get(data)
    if(info==TRUE) writeLines("Veuillez choisir la ou les variables sur lesquelles realiser les analyses")
    X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees", "annuler"), multiple = TRUE, 
               title="Variable dependante")$res
    if(length(X)==0) X<-"autres donnees"
    if(any(X=="annuler")) return(test.t())}
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% X)[,2]->X
    
    # Determiner la norme
    mu <- dlgInput("Quelle est la valeur de la norme ?", 0)$res
    while(length(mu)==0){print("vous devez specifier la norme")
      mu <- dlgInput("Quelle est la valeur de la norme ?", 0)$res }
    strsplit(mu, ":")->mu
    tail(mu[[1]],n=1)->mu
    as.numeric(mu)->mu
    
    if(info==TRUE) writeLines("Une analyse bilaterale teste l existence d une difference. Le choix superieur teste si la moyenne est strictement superieure
                              \n Le choix infereieur teste l existence d une difference strictement inferieure")
    dlgList(c("Bilateral", 
              "Superieur",
              "Inferieur"), preselect=NULL, multiple = FALSE, title="Comparaison de moyennes")$res->alternative
    if(length(alternative)==0) alternative<-"Bilateral"
    recode(alternative, "'Bilateral'= 'two.sided';'Superieur'='greater'; 'Inferieur'='less'")->alternative
    if(info==TRUE) writeLines("Si vous souhaitez realiser l analyse pour differents sous-echantillons en fonction d un critere categorie (donc des groupes)
                              \n choisissez oui. Dans ce cas, l analyse est realisee sur l echantillon complet et sur les sous echantillons.
                              \n Si vous desirez l analyse pour l echantillon complet uniquement, chosissez non.
                              \n l analyse par groupe ne s applique pas aux statistiques robustes")
    dlgList(c("oui", 
              "non"), preselect=NULL, multiple = FALSE, title="Analyse par groupe?")$res->par.groupe
    if(length(par.groupe)==0) par.groupe<-"non"
    if(par.groupe=="oui"){group<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 
                                           "autres donnees"), multiple = FALSE, title="Variable de classement")$res
    if(length(group)==0) {par.groupe<-"non"
    group<-NULL}
    subset(listes, listes[,1] %in% group)[,2]->group} else group<-NULL
    
    # creation de la fonction "norme"
    norme<-function(X, mu, data, param=c("param", "non param", "robustes"), group=NULL, alternative="two.sided"){
      list()->Resultats
      psych::describe(data[,X], type=3)->Resultats$"statistiques descriptives"
      if(!is.null(group)) describeBy(data[,X],  group=group,mat=TRUE,digits=15, check=FALSE,skew = TRUE, 
                                     ranges = TRUE,trim=tr, type=3)->Resultats$"statistiques descriptives par groupe"
      
      if(any(param=="param")){
        if(length(data[,X])<5000){
          shapiro.test(data[,X])->Shapiro_Wilk # realise le Shapiro-Wilk
          lillie.test(data[,X])->Lilliefors  # realise le Lilliefors
          round(data.frame(Shapiro_Wilk$statistic,Shapiro_Wilk$p.value, Lilliefors$statistic, Lilliefors$p.value),4)->normalite
          names(normalite)<-c("W de Shapiro-Wilk", "valeur.p SW", "D de Lilliefors", "valeur.p Llfrs")
          dimnames(normalite)[1]<-" "
          format(normalite, width = max(sapply(names(normalite), nchar)), justify = "centre")->Resultats$"Tests de normalite"}
        x11()
        h<-hist(data[,X], breaks=10, density=10, col="black", xlab=names(data)[X], main="Distribution d echantillonnage") 
        xfit<-seq(min(data[,X]),max(data[,X]),length=40) 
        yfit<-dnorm(xfit,mean=mean(data[,X]),sd=sd(data[,X])) 
        yfit <- yfit*diff(h$mids[1:2])*length(data[,X]) 
        lines(xfit, yfit, col="darkblue", lwd=2) 
        t.test(data[,X], mu = mu, paired = FALSE, conf.level = 0.95, alternative=alternative)->ttest
        ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R_carre
        cohensD(data[,X], mu=mu)->dc
        data.frame(round(ttest$statistic,3), ttest$parameter, round(ttest$p.value,4), ttest$conf.int[[1]], ttest$conf.int[[2]], 
                   round(R_carre,4), round(dc,3))->ttest
        names(ttest)<-c("t test", "ddl", "valeur.p", "Lim.inf.IC", "Lim.sup.IC", "R.carre", "D Cohen")
        dimnames(ttest)[1]<-" "
        ttest->Resultats$"Test de Student - comparaison a une norme"
        
        if(!is.null(group)){
          func <- function(data, moy=mu){ 
            t.test(data, mu = moy)->ttest
            ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R_carre
            cohensD(data[,1], mu=moy)->dc
            return(data.frame(test.t=round(ttest$statistic,3), 
                              ddl=ttest$parameter, 
                              valeur.p=round(ttest$p.value,4), 
                              IC.inf=ttest$conf.int[[1]], 
                              IC.sup=ttest$conf.int[[2]], 
                              R.carre=round(R_carre,4), 
                              D.Cohen=round(dc,3)))}
          data.frame(data[,X])->Y
          ddply(.data=Y, .(group), func)->t.groupes
          t.groupes->Resultats$"Analyse par groupe"$"t de Student"}}
      
      if(any(param=="non param")){
        data.frame(wilcox.test(data[,X],mu, alternative)$statistic, round(wilcox.test(data[,X],mu, alternative)$p.value,4))->wilcox
        names(wilcox)<-c("Wilcoxon W","valeur.p")
        wilcox->Resultats$Wilcoxon
        
        if(!is.null(group)){
          func <- function(data, moy=mu){ 
            return(data.frame(Wilcoxon.W=wilcox.test(data[,1],mu, alternative)$statistic, 
                              valeur.p=round(wilcox.test(data[,1],mu, alternative)$p.value,4)))}
          ddply(.data=Y, .(group), func)->Wilcox.groupes
          Wilcox.groupes->Resultats$"Analyse par groupe"$"Wilcoxon"}}
      
      if(any(param=="robustes")){
        round(unlist(WRS::trimci(data[,X],tr=.2,alpha=.05, null.value=mu)),4)->m.tr
        names(m.tr)<-c("lim.inf.IC","lim.sup.IC", "M.tronquee","test.t", "se","valeur.p","n")
        m.tr->Resultats$'Test sur la moyenne tronquee'
        data[,X]->x
        if(length(x)>200 | max(table(x)>=length(x)/2)){
          WRS::trimcibt(x, tr=.2,alpha=.05,nboot=nboot,plotit=T,op=3)$ci->trimci
          WRS::mestci(x,alpha=.05,nboot=nboot,bend=1.28,os=F)->M.estimator
          WRS:: momci(x,alpha=.05,nboot=nboot)->MoM
          rbind(trimci, M.estimator$ci,MoM$ci)->IC.robustes
          dimnames(IC.robustes)[[2]]<-c("lim.inf.IC", "lim.sup.IC")
          dimnames(IC.robustes)[[1]]<-c("bootstrap-t method", "M-estimator", "M-estimator modifie")} else{
            WRS::trimcibt(x, tr=.2,alpha=.05,nboot=nboot,plotit=T,op=3)$ci->trimci
            WRS::momci(x,alpha=.05,nboot=nboot)->MoM
            rbind(trimci, MoM$ci)->IC.robustes
            dimnames(IC.robustes)[[2]]<-c("lim.inf.IC", "lim.sup.IC")
            dimnames(IC.robustes)[[1]]<-c("bootstrap-t method", "M-estimator modifie")}
        IC.robustes->Resultats$Robustes
        c("Le bootstrap-t method est un bootstrap adapte au calcul de la moyenne tronquee", 
          " Cet indice est adapte dans la plupart des situations. Le M-estimator modifie doit etre prefere pour N<20",
          "La troncature sur le M-estimator s adapte en fonction des caracteristiques de votre echantillon.")->Resultats$infos}
      
      return(Resultats)
    }
    # execution de la fonction
    
    length(X)->Y
    for(i in 1:Y){
      if(any(desires=="Donnees completes")){norme(X=X[i], mu=mu, data=data, param=param)->Resultats$"Donnees completes"} 
      # identification de valeurs aberrantes a l aide du test de Grubbs et stockage de l info dans l objet outliers
      if(any(desires=="Identification des outliers")|any(desires=="Donnees sans valeur influente")) valeurs.influentes(X=X[i], critere=critere,z=3.26, data=data)->influentes
      if(any(desires=="Identification des outliers")) influentes->Resultats$"Valeurs influentes"
      if(any(desires=="Donnees sans valeur influente")) norme(X=X[i], mu=mu, data=nettoyees,param=param)->Resultats$"Donnees nettoyees"
      #   if(sauvegarde) save(Resultats=Resultats, choix=choix)
      #    print(Resultats)
    }}
  
  
  if(choix=="Deux echantillons apparies"){
    if(info==TRUE){
      temps1<-1:3
      temps2<-4:6
      cbind(temps1,temps2)->large
      data.frame(c(rep("temps1",3),rep("temps2", 3)), 1:6)->long
      names(long)<-c("moment","mesure")
      print("ceci est le format large")
      print(large)
      print("ceci est le format long")
      print(long)}
    
    format<-dlgList(c("large", "long"), preselect="large", multiple = FALSE, title="Quel est le format de donnees?")$res
    if(length(format)==0) format<-"large"
    
    if(format=="large"){
      if(info==TRUE) writeLines("Veuillez choisir la base de donnees")
      X<-"autres donnees"
      while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
      data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
      if(length(data)==0) return(test.t())
      data<-get(data)
      if(info==TRUE)print("veuillez choisir la colonne qui correspond Ã  la premiere modalite de votre variable independante - Le moment 1")
      X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees", "annuler"), multiple = FALSE, 
                 title="Temps 1")$res
      if(length(X)==0) X<-"autres donnees"
      if(any(X=="annuler")) return(test.t())}
      if(info==TRUE)print("veuillez choisir la colonne qui correspond Ã  la seconde modalite de votre variable independante - Le moment 2")
      Y<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = FALSE, title="Temps 2")$res
      if(length(Y) == 0L) return(test.t())
      listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
      subset(listes, listes[,1] %in% X)[,2]->X 
      subset(listes, listes[,1] %in% Y)[,2]->Y
      noms<-c(names(data)[X], names(data)[Y])
      
      if(class(data[,X])!="integer" & class(data[,X])!="numeric") return("une variable n est pas numerique")
      if(class(data[,Y])!="integer" & class(data[,Y])!="numeric") return("une variable n est pas numerique")}else {
        VI<-"autres donnees"
        while(any(VI=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
        data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
        if(length(data)==0) return(test.t())
        data<-get(data)
        VI<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees", "annuler"), multiple = TRUE, 
                    title="Variable independante")$res
        if(length(VI)==0) VI<-"autres donnees"
        if(any(VI=="annuler")) return(test.t())
        }
        
        VD<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = FALSE, title="Variable dependante")$res
        if(length(VD) == 0L) return(test.t())
        listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
        subset(listes, listes[,1] %in% VI)[,2]->VI 
        subset(listes, listes[,1] %in% VD)[,2]->VD
        
        if(class(data[,VI])=="integer" |class(data[,VI])=="numeric"){factor(data[,VI])->data[,VI]}
        if(nlevels(data[,VI]!=2)) return("la variable independante n est pas une variable categorielle a 2 modalites")
        levels(data[,VI])->noms
        if(class(data[,VD])!="integer" && class(data[,VD])!="numeric") return("la variable dependante n est pas numerique") 
        ptcpt<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = TRUE, title="Identifiant des participants?")$res
        if(length(ptcpt) == 0L) return("il est indispensable d avoir un identifiant du participant dans ce format de donnees")      
        subset(listes, listes[,1] %in% ptcpt)[,2]->ptcpt
        data[,c(ptcpt,VI,VD)]->data
        dcast(data,data[,1]~data[,2])->data
        names(data)<-c(ptcpt, noms)
        X<-noms[1]
        Y<-noms[2]
      }
    if(info==TRUE)print("donner un nom explicite Ã  la variable independante rendra la lecture des resultats plus lisible")
    nomVI <- dlgInput("Quel est le nom de la variable independante?", "Moment")$res
    if(length(nomVI)==0) nomVI<-"Moment"
    strsplit(nomVI, ":")->nomVI
    tail(nomVI[[1]],n=1)->nomVI
    if(info==TRUE)print("donner un nom explicite Ã  la variable dependante rendra la lecture des resultats plus lisible")
    nomVD <- dlgInput("Quel est le nom de la variable dependante?", "Resultat")$res
    if(length(nomVD)==0) nomVD<-"Resultat"
    strsplit(nomVD, ":")->nomVD
    tail(nomVD[[1]],n=1)->nomVD
    # creation de la fonction "norme"
    apparies<-function(X, Y, data=data, param=param,alternative=alternative){
      list()->Resultats
      psych::describe(data[,c(X,Y)], type=3)->Resultats$"statistiques descriptives"
      if(any(param=="param")){
        if(length(data[,X])<5000){
          shapiro.test(data[,X]-data[,Y])->Shapiro_Wilk # realise le Shapiro-Wilk
          lillie.test(data[,X]-data[,Y])->Lilliefors  # realise le Lilliefors
          round(data.frame(Shapiro_Wilk$statistic,Shapiro_Wilk$p.value, Lilliefors$statistic, Lilliefors$p.value),4)->normalite
          names(normalite)<-c("W de Shapiro-Wilk", "valeur.p SW", "D de Lilliefors", "valeur.p Llfrs")
          dimnames(normalite)[1]<-" "
          format(normalite, width = max(sapply(names(normalite), nchar)), justify = "centre")->Resultats$"Tests de normalite"}
        data[,X]-data[,Y]->diffs
        x11()
        h<-hist(diffs, breaks=10, density=10, col="black", xlab="differences", main="Distribution des differences") 
        xfit<-seq(min(diffs),max(diffs),length=40) 
        yfit<-dnorm(xfit,mean=mean(diffs),sd=sd(diffs)) 
        yfit <- yfit*diff(h$mids[1:2])*length(diffs) 
        lines(xfit, yfit, col="darkblue", lwd=2) 
        
        t.test(data[,X], data[,Y], paired = TRUE, conf.level = 0.95, alternative=alternative)->ttest
        ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R_carre
        cohensD(data[,X], data[,Y], method="paired")->dc
        data.frame(round(ttest$statistic,3), ttest$parameter, round(ttest$p.value,4), ttest$conf.int[[1]], ttest$conf.int[[2]], 
                   round(R_carre,4), round(dc,3))->ttest
        names(ttest)<-c("t test", "ddl", "valeur.p", "Lim.inf.IC", "Lim.sup.IC", "R.carre", "D Cohen")
        dimnames(ttest)[1]<-" "
        ttest->Resultats$"Test de Student - comparaison de deux echantillons apparies"}
      
      if(any(param=="non param")) {data.frame(wilcox.test(data[,X],data[,Y], paired=TRUE, correct=TRUE, alternative=alternative)$statistic, 
                                              round(wilcox.test(data[,X],data[,Y], paired=TRUE, correct=TRUE, alternative=alternative)$p.value,4))->wilcox
        names(wilcox)<-c("Wilcoxon W","valeur.p")
        wilcox->Resultats$Wilcoxon}
      if(any(param=="robustes")){WRS::yuend(data[ ,X], data[ ,Y], tr=.2)->moy.tr
        round(unlist(moy.tr),3)->moy.tr
        names(moy.tr)<-c("IC Inf","IC Sup", "valeur.p", "Moyenne1", "Moyenne2", "Difference","se", "Stat", "n", "ddl") 
        WRS::ydbt(data[ ,X], data[ ,Y], tr=0.2, nboot=nboot)->moy.tr.bt
        moy.tr->Resultats$Robustes$"Comparaison basee sur les moyennes tronquees"
        round(unlist(moy.tr.bt,3))->Resultats$Robustes$"bootstrap studentise sur les moyennes tronquees"
        if(length(data[,1])>20) {WRS::bootdpci(data[ ,X], data[ ,Y], nboot=nboot, BA=T)$output[,2:6]->Mest
          names(Mest)<-c("statistique", "valeur.p", "p.crit", "CI inf", "CI sup")
          Mest->Resultats$Robustes$"Bootstrap de type BCa sur le M-estimator"}}
      
      return(Resultats)                                                                               
    } 
    
    if(any(desires=="Donnees completes")){apparies(X, Y, data=data, param=param,alternative=alternative)->Resultats$"Donnees completes"
      # realisation du graphique
      cbind(1:length(data[,1]),data[,c(X,Y)])->data1
      longdata<-melt(data1, id=1, measured=c(2,3))
      nonaj<-ggplot(longdata, aes(variable, value))+labs(x=nomVI, y=nomVD)+stat_summary(fun.y=mean, geom="bar",
                                                                                        fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
      # realisation du graphique ajuste propose par Loftus et Masson 1994 (pour plus d informations voir l article)
      data1$meanD2<-(data1[ ,2]+data1[ ,3])/2
      mean(data1$meanD2)->GMean
      GMean-data1$meanD2->data1$adj
      data1$adjM1<-data1[ ,2]+data1$adj
      data1$adjM2<-data1[ ,3]+data1$adj
      data.frame(data1[,1], data1$adjM1, data1$adjM2)->data1
      names(data1)<-c("participants", noms)
      longdata<-melt(data1, id=1, measured=c(2,3))
      aj<-ggplot(longdata, aes(variable, value))+labs(x=nomVI, y=nomVD)+stat_summary(fun.y=mean, geom="bar", 
                                                                                     fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
      
      x11()
      print(nonaj)
      x11()
      print(aj)} 
    # identification de valeurs aberrantes a l aide du test de Grubbs et stockage de l info dans l objet outliers
    if(any(desires=="Identification des outliers")|any(desires=="Donnees sans valeur influente")) {data[,X]-data[,Y]->data$diff
      valeurs.influentes(X="diff", critere=critere,z=3.26, data=data)->influentes}
    
    if(any(desires=="Identification des outliers")) influentes->Resultats$"Valeurs influentes"
    
    if(any(desires=="Donnees sans valeur influente")) {apparies(X, Y, data=nettoyees, param=param,alternative=alternative)->Resultats$"Donnees nettoyees"
      # realisation du graphique
      cbind(1:length(data[,1]),data[,c(X,Y)])->data1
      longdata<-melt(data1, id=1, measured=c(2,3))
      nonaj<-ggplot(longdata, aes(variable, value))+labs(x=nomVI, y=nomVD)+stat_summary(fun.y=mean, geom="bar",
                                                                                        fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
      # realisation du graphique ajuste propose par Loftus et Masson 1994 (pour plus d informations voir l article)
      data1$meanD2<-(data1[ ,2]+data1[ ,3])/2
      mean(data1$meanD2)->GMean
      GMean-data1$meanD2->data1$adj
      data1$adjM1<-data1[ ,2]+data1$adj
      data1$adjM2<-data1[ ,3]+data1$adj
      data.frame(data1[,1], data1$adjM1, data1$adjM2)->data1
      names(data1)<-c("participants", noms)
      longdata<-melt(data1, id=1, measured=c(2,3))
      aj<-ggplot(longdata, aes(variable, value))+labs(x=nomVI, y=nomVD)+stat_summary(fun.y=mean, geom="bar", 
                                                                                     fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
      x11()
      print(nonaj)
      x11()
      print(aj)}
    #   if(sauvegarde) save(Resultats=Resultats, choix=choix)
    #    print(Resultats)
  }
  
  
  
  
  if(choix=="Deux echantillons independants"){
    if(info==TRUE) writeLines("Veuillez choisir la base de donnees")
    VI<-"autres donnees"
    while(any(VI=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
    if(length(data)==0) return(analyse())
    data<-get(data)
    if(info==TRUE) writeLines("Veuillez choisir la variable independante. Cela doit etre une variable categorielle a deux modalites")
    VI<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees","annuler"), multiple = TRUE, 
                title="Variable independante")$res
    if(length(VI)==0) VI<-"autres donnees"
    if(any(VI=="annuler")) return(test.t())}
    if(info==TRUE) writeLines("Veuillez choisir la variable dependante. Cela doit etre une variable numerique")
    VD<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = TRUE, title="Variable dependante")$res
    if(length(VD) == 0L) return(test.t())
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% VI)[,2]->VI 
    subset(listes, listes[,1] %in% VD)[,2]->VD
    if(class(data[,VI])=="integer" |class(data[,VI])=="numeric"){factor(data[,VI])->data[,VI]}
    if(nlevels(data[,VI]!=2)) return("la variable independante n est pas une variable categorielle a 2 modalites")
    levels(data[,VI])->noms
    
    indpdts<-function(VD, VI, data, param=c("param", "non param","robustes"),alternative="two.sided"){
      list()->Resultats
      psych::describeBy(data[ ,VD], data[ ,VI] ,mat=TRUE,type=3)->Resultats$"statistiques descriptives"
      as.formula(paste0(names(data)[VD]," ~ ",names(data)[VI]))->modele
      if(any(param=="param")){
        lm(modele,na.action=na.exclude, data=data)->lm.r1
        resid(lm.r1)->data$residu
        if(length(data$residu)<5000){
          shapiro.test(data$residu)->Shapiro_Wilk # realise le Shapiro-Wilk
          lillie.test(data$residu)->Lilliefors  # realise le Lilliefors
          round(data.frame(Shapiro_Wilk$statistic,Shapiro_Wilk$p.value, Lilliefors$statistic, Lilliefors$p.value),4)->normalite
          names(normalite)<-c("W de Shapiro-Wilk", "valeur.p SW", "D de Lilliefors", "valeur.p Llfrs")
          dimnames(normalite)[1]<-" "
          format(normalite, width = max(sapply(names(normalite), nchar)), justify = "centre")->Resultats$"Tests de normalite"}
        x11()
        h<-hist(data$residu, breaks=10, density=10, col="black", xlab="residus", main="Distribution des residus") 
        xfit<-seq(min(data$residu),max(data$residu),length=40) 
        yfit<-dnorm(xfit,mean=mean(data$residu),sd=sd(data$residu)) 
        yfit <- yfit*diff(h$mids[1:2])*length(data$residu) 
        lines(xfit, yfit, col="darkblue", lwd=2) 
        car::leveneTest(data[ ,VD], data[ ,VI])->Levene # test de Levene pour homogeneite des variances
        round(unlist(Levene)[c(1,2,3,5)],3)->Levene
        names(Levene)<-c("ddl1","ddl2","F","valeur.p")
        Levene->Resultats$"Test de Levene verifiant l homogeneite des variances"
        t.test(modele, data=data, alternative=alternative,  var.equal=TRUE, conf.level=0.95)->student
        round(student$statistic^2/(student$statistic^2+student$parameter),3)->R.deux
        d_cohen<-round(cohensD(modele , data=data, method = "pooled"),3)
        data.frame(student[9], round(student$statistic,3), student$parameter, round(student$p.value,3), round(student$conf.int[1],4),
                   round(student$conf.int[2],4),  R.deux, d_cohen)->student
        names(student)<-c("modele", "test t", "ddl", "valeur.p", "lim.inf.IC", "lim.sup.IC","R.carre","d de Cohen")
        student->Resultats$"t de student pour echantillons independants sans la correction de Welch"
        t.test(modele, data=data, alternative=alternative,  var.equal=FALSE, conf.level=0.95)->corrige
        corrige$statistic^2/(corrige$statistic^2+corrige$parameter)->R.deux.corr
        d_cohen.corr<-cohensD(modele , data=data, method = "unequal")
        data.frame(corrige[9], round(corrige$statistic,3), round(corrige$parameter,3), round(corrige$p.value,3), round(corrige$conf.int[1],4),
                   round(corrige$conf.int[2],4),  R.deux, d_cohen)->corrige
        names(corrige)<-c("modele", "test t", "ddl", "valeur.p", "lim.inf.IC", "lim.sup.IC","R.carre","d de Cohen")
        corrige->Resultats$"t de student pour echantillons independants avec la correction de Welch"}
      
      if(any(param=="non param")) {data.frame(wilcox.test(modele, data=data, paired=FALSE,exact = NULL, correct=TRUE, alternative=alternative)$statistic, 
                                              round(wilcox.test(modele,data=data, paired=FALSE, correct=TRUE, alternative=alternative)$p.value,4))->wilcox
        names(wilcox)<-c("W de Mann Whitney","valeur.p")
        wilcox->Resultats$"test de Mann-Whitney"}
      
      if(any(param=="robustes")){
        data[which(data[,VI]==levels(data[,VI])[1]),]->g1 # on cree une base de donnees avec le groupe 1 uniquement (sans valeur aberrantes)
        data[which(data[,VI]==levels(data[,VI])[2]),]->g2 # on cree une base de donnees avec le groupe 2 uniquement (sans valeur aberrantes)
        WRS::yuen(g1[,VD],g2[,VD])->yuen.modele### fournit la probabilite associee a des moyennes tronquees.Par defaut, la troncature est de 0.20
        round(unlist(yuen.modele),4)->yuen.modele
        cbind(yuen.modele[1:2], yuen.modele[3:4])->yuen.desc
        dimnames(yuen.desc)[[1]]<-levels(data[,VI])
        dimnames(yuen.desc)[[2]]<-c("n", "moyennes tronquees")
        yuen.desc->Resultats$Robustes$"statistiques descriptives"
        
        yuen.modele[c(5,6,8,9,10,11,12,7)]->yuen.modele
        names(yuen.modele)<-c("lim.inf.IC", "lim.sup.IC", 
                              "Difference","Err-type","Stat", "Seuil", "ddl","valeur.p")
        yuen.modele->Resultats$Robustes$"Analyse sur les moyennes tronquees"
        WRS::yuenbt(g1[,VD],g2[,VD], nboot=nboot, side=T)->yuen.bt.modele ### fournit la probabilite associee a des moyennes tronquees apres un bootstrap.
        round(unlist(yuen.bt.modele)[1:4],4)->yuen.bt.modele
        names(yuen.bt.modele)<-c("lim.inf.IC", "lim.sup.IC", "Stat", "valeur.p")
        yuen.bt.modele->Resultats$Robustes$"Bootstrap utilisant la methode t sur les moyennes tronquees"
        WRS::pb2gen(g1[,VD],g2[,VD], nboot=nboot)->pb2gen.modele### calcule le bootstrap sur le M-estimateur et fournit l intervalle de confiance. 
        round(unlist(pb2gen.modele)[1:6],4)->pb2gen.modele
        names(pb2gen.modele)<-c("M.estimaror.G1", "M.estimator.G2", "diff", "lim.inf.IC", "lim.sup.IC", "valeur.p")
        pb2gen.modele->Resultats$Robustes$"Percentile bootstrap sur les M-estimator"
        Resultats$Robustes$Informations<-c("la methode du percentile bootstrap doit etre preferee pour les petits echantillons",
                                           "Pour des echantillons plus importants, les boostrap utilisant la methode t doit etre preferee.")
        WRS::ks(g1[,VD],g2[,VD],w=F,sig=T)->KS
        round(unlist(KS),4)->KS
        names(KS)<-c("KS", "Seuil.critique","valeur.p")
        KS->Resultats$Robustes$"Test de Kolmogorov-Smirnov comparant deux distributions"
        x11()
        g2plot(g1[,VD],g2[,VD],op=4,rval=15,fr=0.8,aval=0.5)
        
      }
      
      return(Resultats)
    }
    R2<-list()
    for(i in 1:length(VD)){
      data[complete.cases(data[,c(VI,VD[i])]),]->data2
      
      if(any(desires=="Donnees completes")){indpdts(VI=VI, VD=VD[i], data=data2, param=param)->R2$"Donnees completes"} 
      if(length(VD)<3) x11()
      print(ggplot(data2, aes(data2[ ,VI], data2[ ,VD[i]]), environment = .e)+labs(x=names(data2[,VI]), y=names(data2[,VD[i]]))+stat_summary(fun.y=mean, geom="bar", fill="Black", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2))
      # identification de valeurs aberrantes a l aide du test de Grubbs et stockage de l info dans l objet outliers
      
      if(any(desires=="Identification des outliers")|any(desires=="Donnees sans valeur influente")) { 
        lm(data2[,VD[i]]~data2[,VI],na.action=na.exclude, data=data2)->lm.r1
        resid(lm.r1)->data2$residu
        valeurs.influentes(X="residu", critere=critere,z=3.26, data=data2)->influentes}
      
      if(any(desires=="Identification des outliers")) influentes->R2$"Valeurs influentes"
      if(any(desires=="Donnees sans valeur influente")){ get("nettoyees", envir=.GlobalEnv)->nettoyees
        indpdts(VI=VI, VD=VD[i], data=nettoyees, param=param)->R2$"Donnees nettoyees"
        if(length(VD)<3) x11()
        print(ggplot(nettoyees, aes(nettoyees[ ,VI], nettoyees[ ,VD[i]]),environment = .e)+labs(x=names(nettoyees[,VI]), y=names(nettoyees[,VD[i]]))+stat_summary(fun.y=mean, geom="bar", fill="Black", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2))
      }
      R2->Resultats[[i]]
    }
    names(Resultats)<-names(data)[VD]
  }
  
  dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="voulez vous sauvegarder?")$res->sauvegarde
  if(length(sauvegarde)==0) sauvegarde<-FALSE
  
  if(sauvegarde) save(Resultats=Resultats, choix=choix, env=.e)
  return(Resultats)
}

#### corrÃ©lations tÃ©trachoriques et polychoriques (+ mixtes)
# devrait bugger ... Ã  vÃ©rifier
tetrapoly<-function(data=NULL, save=F){
  options (warn=-1) 
  c("psych", "svDialogs")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  .e<- environment()
  dlgList(c("correlations polychoriques", "correlations tetrachoriques", "correlations mixtes"), preselect=NULL, multiple = FALSE, title="Type de correlations ?")$res->method
  if(length(method)==0) return(choix.corr())
  
  X<-"autres donnees"
  while(any(X=="autres donnees")){data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
  if(length(data)==0) return(tetrapoly())
  data<-get(data)
  X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees","annuler"), multiple = TRUE, 
             title="Entetes de colonnes")$res
  if(length(X)==0) X<-"autres donnees"
  if(X==annuler) return(tetrapoly())}
  listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
  subset(listes, listes[,1] %in% X)[,2]->X
  
  save<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = TRUE, title="Enregistrer les Resultats ?")$res
  if(length(save)==0) save<-FALSE
  list()->Resultats # cree une liste appelee Resultats dans laquelle on va stocker les Resultats
  
  for(i in 1:(length(data[,X]))){
    if(is.factor(data[,c(X)[i]])){           
      if(dlgMessage(paste("la variable", names(data)[c(X)[i]], "n est pas numerique. \n
                          si c est une variable ordinale, vous pouvez preciser l ordre des modalites dans la console  \n
                          voulez vous continuez ?"), "okcancel")$res=="cancel") return(choix.corr()) else {
                            dlgForm(setNames(as.list(rank(levels(data[,i]))), levels(data[,i])), "Ordre des modalites")$res->niveaux
                            stack(niveaux)->niveaux                                                
                            factor(data[,i], levels(data[,i])[niveaux[,1]])->data[,i] 
                            as.numeric(data[,i])->data[,i]
                            Resultats$Avertissement[[i]]<-paste("la variable", names(data)[i], "a ete convertie en variable numerique")
    }}}  
  
  if(method=="correlations tetrachoriques"){tetrachoric(data[,X],correct=TRUE,smooth=TRUE,global=FALSE,weight=NULL,na.rm=TRUE, delete=TRUE)->poly
    poly$rho->Resultats$"Matrice de correlation tetrachorique"
    poly$tau->Resultats$"Matrice des tau"}   
  
  
  if(method=="correlations polychoriques"){polychoric(data[,X],smooth=TRUE,global=FALSE,polycor=FALSE, ML = FALSE,
                                                      std.err=FALSE,weight=NULL,progress=TRUE,na.rm=TRUE, delete=TRUE)->poly
    poly$rho->Resultats$"Matrice de correlation polychorique"
    poly$tau->Resultats$"Matrice des tau"}   
  if(method=="correlations mixtes") {mixed.cor(x = data[,X],smooth=TRUE, correct=.5,global=FALSE,
                                               ncat=8,use="pairwise",method="pearson",weight=NULL)$rho->Resultats$"Matrice mixte"
    Resultats$Information<-"correlations tetrachoriques pour les variables a 2 modalites, polychoriques pour 3 a 8 modalites, Bravais-Pearson pour plus de 8 modalites "}
  
  if(save) save(Resultats=Resultats, choix=method, env=.e)
  print(Resultats,short=FALSE) }


#### permet d'identifier et enlever les valeurs influentes ####
# pas encore intÃ©grÃ© Ã  l'interface graphique mais dans les fonctions. Il faut rajouter l'interace graphique pour la faire fonctionnerdirectement de easier
valeurs.influentes<-function(X, critere="Grubbs", z=3.26, data=data){options (warn=-1)
  c("outliers")->packages
  .inf <- environment()
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  if(class(data[,X])=="integer") as.numeric(data[,X])->data[,X]
  if(class(data[,X])!="numeric") return("la variable n est pas numerique")
  if(class(z)!="numeric") return("z doit etre un nombre")
  if(any(match(c("Grubbs","z"), critere))==FALSE) return("Les valeurs admises pour critere sont  z  et  Grubbs ")
  length(data[,1])->i
  if(critere=="Grubbs"){
    grubbs.test(data[,X], type = 10, opposite = FALSE, two.sided = FALSE)->outliers # test de Grubbs permettant de savoir s il y a des valeurs aberrantes
    names(data[X])->outliers$data.name
    # on realise un boucle du type: tant y est inferieure a 0.05, continue. 
    data.frame()->valeur.influentes
    while(grubbs.test(data[,X], type = 10, opposite = FALSE, two.sided = FALSE)$p.value <0.05)  { 
      which.max(abs(data[,X]))->max #cherche la valeur maximale qu on stocke dans l objet max                                                                                                
      rbind(valeur.influentes,data[max, ])->valeur.influentes
      data<-data[ -max, ] # supprime la valeur maximmal de data
    }}
  
  
  if(critere=="z"){
    # on centre reduit les residus et on stocke la valeur absolue du z dans la variable "Var_centree_abs" dans l objet data2
    abs(scale(data[,X], center = TRUE, scale = TRUE))->data$Var_centree_abs 
    valeur.influentes<-data[which(data$Var_centree_abs>z),]
    data<-data[which(data$Var_centree_abs<=z),]  
  }
  length(data[,1])->iso
  i-iso->n # nombre d observations supprimees
  round((n/i)*100,2)-> pourcentage_N # proportions d observations supprimees (nombre / taille de l echantillon)
  rbind(n, paste(pourcentage_N, "%"))->synthese_aberrant # on combine le nombre et le pourcentage. 
  data.frame(information=c("Nombre d observations retirees", "% d observations considerees comme influentes"), Synthese=synthese_aberrant)->synthese_aberrant # on cree un data.frame 
  list()->Resultats.valeurs.influentes
  data.frame(G=outliers$statistic[1], U=outliers$statistic[2], valeur.p=round(outliers$p.value,4))->Resultats.valeurs.influentes$"Test de Grubbs"
  Resultats.valeurs.influentes$"Valeur la plus elevee"<-outliers$alternative
  Resultats.valeurs.influentes$"observations influentes"<-valeur.influentes
  Resultats.valeurs.influentes$"Synthese des observations influentes" <-synthese_aberrant
  data->>nettoyees
  return(Resultats.valeurs.influentes[1:4]) 
}


# fonction corrigÃ©e du package mbess permettant de faire des barres d'effets sur les mÃ©diations. Ne pas modifier. 
mediation.effect.bar.plot2<-function (x, mediator, dv, main = "Mediation Effect Bar Plot", 
                                      width = 1, left.text.adj = 0, right.text.adj = 0, rounding = 3, 
                                      file = "", save.pdf = FALSE, save.eps = FALSE, save.jpg = FALSE, 
                                      ...) 
{
  Mediation.Results <- mediation(x = x, mediator = mediator, 
                                 dv = dv, conf.level = 0.95,complete.set=TRUE)
  observed.c <- Mediation.Results$Y.on.X$Regression.Table[2, 
                                                          1]
  observed.c.prime <- Mediation.Results$Y.on.X.and.M$Regression.Table[2, 
                                                                      1]
  max.possible.c <- sqrt(var(dv))/sqrt(var(x))
  if (observed.c < 0) 
    max.possible.c <- -max.possible.c
  if (width < 1) {
    width <- 0.5 * (1 - width)
  }
  if (width > 1) {
    width <- 0.5 * (1 + width)
  }
  if (save.pdf == TRUE) {
    if (save.eps == TRUE) 
      stop("Only one file format for saving figure may be used at a time (you have both PDF and EPS specified).")
    if (save.jpg == TRUE) 
      stop("Only one file format for saving figure may be used at a time (you have both PDF and JPG specified).")
  }
  if (save.eps == TRUE) {
    if (save.jpg == TRUE) 
      stop("Only one file format for saving figure may be used at a time (you have both EPS and JPG specified).")
  }
  if (save.pdf == TRUE | save.eps == TRUE | save.jpg == TRUE) {
    no.file.name <- FALSE
    if (file == "") {
      file <- "mediation.effect.bar.plot"
      no.file.name <- TRUE
    }
  }
  if (save.pdf == TRUE) 
    pdf(file = paste(file, ".pdf", sep = ""), ...)
  if (save.eps == TRUE) 
    jpeg(filename = paste(file, ".eps", sep = ""), ...)
  if (save.jpg == TRUE) 
    jpeg(filename = paste(file, ".jpg", sep = ""), ...)
  plot(c(-2, 2), seq(0, 1), ylab = "", xlab = "", xaxt = "n", 
       yaxt = "n", bty = "n", type = "n", main = main, ...)
  segments(x0 = -0.5 * width, y0 = 0, x1 = -0.5 * width, y1 = 1)
  segments(x0 = 0.5 * width, y0 = 0, x1 = 0.5 * width, y1 = 1)
  segments(x0 = 0.5 * width, y0 = 0, x1 = -0.5 * width, y1 = 0)
  segments(x0 = 0.5 * width, y0 = 1, x1 = -0.5 * width, y1 = 1)
  segments(x0 = 0.5 * width, y0 = observed.c/max.possible.c, 
           x1 = -0.5 * width, y1 = observed.c/max.possible.c)
  segments(x0 = 0.5 * width, y0 = observed.c.prime/max.possible.c, 
           x1 = -0.5 * width, y1 = observed.c.prime/max.possible.c)
  rect(xleft = -0.5 * width, ybottom = 0, xright = 0.5 * width, 
       ytop = observed.c.prime/max.possible.c, density = 10, 
       angle = 45, border = NA)
  rect(xleft = -0.5 * width, ybottom = observed.c.prime/max.possible.c, 
       xright = 0.5 * width, ytop = observed.c/max.possible.c, 
       density = 10, angle = 135, border = NA)
  if (left.text.adj == 0) {
    left.text.adj <- -0.5 * width - (0.5 * width/3)
  }
  if (left.text.adj != 0) {
    left.text.adj <- -0.5 * width - (0.5 * width/3) + left.text.adj
  }
  if (right.text.adj == 0) {
    right.text.adj <- 0.5 * width + (0.5 * width/20)
  }
  if (right.text.adj != 0) {
    right.text.adj <- 0.5 * width + (0.5 * width/20) + right.text.adj
  }
  use.this <- round(max.possible.c, rounding)
  text(x = right.text.adj * 1.3, y = 1, bquote(paste(plain("max possible"), 
                                                     phantom(x), italic(c) == .(use.this))))
  use.this <- round(observed.c, rounding)
  text(x = left.text.adj, y = observed.c/max.possible.c, bquote(paste(plain(observed), 
                                                                      phantom(x), italic(c) == .(use.this))))
  use.this <- round(observed.c.prime, rounding)
  text(x = left.text.adj, y = observed.c.prime/max.possible.c, 
       bquote(paste(plain(observed), phantom(x), italic(c), 
                    phantom(x), plain(prime) == .(use.this))))
  use.this <- round(observed.c - observed.c.prime, rounding)
  text(x = right.text.adj, y = observed.c/max.possible.c - 
         observed.c.prime/max.possible.c, bquote(italic(ab) == 
                                                   .(use.this)))
  segments(x0 = right.text.adj * 0.6, y0 = observed.c/max.possible.c, 
           x1 = right.text.adj * 0.6, y1 = observed.c.prime/max.possible.c)
  segments(x0 = right.text.adj * 0.6, y0 = observed.c/max.possible.c, 
           x1 = right.text.adj * 0.55, y1 = observed.c/max.possible.c)
  segments(x0 = right.text.adj * 0.6, y0 = observed.c.prime/max.possible.c, 
           x1 = right.text.adj * 0.55, y1 = observed.c.prime/max.possible.c)
  text(x = right.text.adj * 0.8, y = 0, "zero")
  if (save.pdf == TRUE) {
    dev.off()
    if (no.file.name == TRUE) 
      print(paste("'mediation.effect.bar.plot.pdf' file saved at the directory", 
                  getwd()))
  }
  if (save.eps == TRUE) {
    dev.off()
    if (no.file.name == TRUE) 
      print(paste("'mediation.effect.bar.plot.eps' file saved at the directory", 
                  getwd()))
  }
  if (save.jpg == TRUE) {
    dev.off()
    if (no.file.name == TRUE) 
      print(paste("'mediation.effect.bar.plot.jpg' file saved at the directory", 
                  getwd()))
  }
}



################################
####                        ####
####       Graphique        ####
####                        ####
################################





########################################
####                                ####
####     Manipulation dataframe     ####
####                                ####
########################################
