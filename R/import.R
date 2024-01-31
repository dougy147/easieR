import <-
  function(file=NULL, dir=NULL, type=NULL, header=T, info=TRUE, dec=".", sep=";",na.strings="NA",sheet=NULL, name="dataset"){
    # file : Character. Name of the file to import
    # dir : Character. Directory in which the file is stored
    # type : One among "csv", "txt", "excel", "SPSS"
    # header : Logical. Is the name of the variables on the first row ? 
    # info : logical
    # dec : Character. The character used in the file for decimal points.
    # sep : The field separator character. Values on each line of the file are separated by this character. 
    # na.strings :a character vector of strings which are to be interpreted as NA values. Blank fields are also considered to be missing values in logical, integer, numeric and complex fields.    
    # sheet:  Character. The name of the sheet for excel files.   
    # name : Character. Name of the R object in which to store the data. 
    
    
    options (warn=-1)
    c("svDialogs",  "readxl","foreign", "textclean")->packages
    lapply(packages, require,character.only=T)
    Resultats <- list()
    if(info==TRUE) print(ASK_file_format_to_import)
    if(!is.null(type)){
      type<-switch(type, 
                   "csv"=TXT_csv_file,TXT_csv_file=TXT_csv_file ,
                   "txt"=TXT_txt_file,TXT_txt_file=TXT_txt_file ,
                   "excel"=TXT_excel_file,TXT_excel_file=TXT_excel_file, 
                   "SPSS" =  TXT_spss_file,TXT_spss_file=TXT_spss_file )
    }
    if(is.null(type)) type <- dlgList(c(TXT_csv_file, TXT_txt_file, TXT_excel_file, TXT_spss_file), preselect=TXT_excel_file, multiple = FALSE, title=ASK_file_format)$res
    if(length(type)==0) return(donnees())
    
    if(!is.null(dir)) try(setwd(dir), silent=T)
    if(!is.null(file) && file.exists( file)) {file<-file 
    dial<-F}else {dial<-T
    file <- try(file.choose(), silent=TRUE)
    if(class(file)=="try-error") return(import())
    setwd(dirname(file))
    basename(file)->file
    }
    
    
    
    if(type!=TXT_spss_file){
      if(dial | (dec %in% c(".",","))==FALSE | (sep %in% c(" ", "\t",";",","))==FALSE |!is.logical(header)){
        if(info==TRUE) print(ASK_headers_in_database)
        header <- dlgList(c(TRUE, FALSE), preselect=TRUE, multiple = FALSE, title=ASK_variables_names)$res
        if(length(header)==0) return(import())
        
        if(info==TRUE) print(ASK_missing_values_value_na_on_empty)
        na.strings <- dlgInput(ASK_value_for_missing_values, "NA")$res
        if(length(na.strings)==0) na.strings <- "NA"
        na.strings <- strsplit(na.strings, ":")
        na.strings <- tail(na.strings[[1]],n=1)
        
        
        if(type==TXT_csv_file|type==TXT_txt_file){
          if(info==TRUE) print(ASK_col_separation_index)
          sep <- dlgList(c("espace","tab",TXT_semicolon,TXT_comma), preselect=TXT_semicolon, multiple = FALSE, title=TXT_col_separator)$res
          if(length(sep)==0) return(import())
          m1 <- matrix(c("espace","tab",TXT_semicolon,TXT_comma," ","\t",";",","),nrow=4)
          sep <- subset(m1, m1[,1] %in% sep)[,2]
          
          if(info==TRUE) print(ASK_decimal_symbol)
          dec <- dlgList(c(TXT_dot, TXT_comma), preselect=NULL, multiple = FALSE, title=TXT_decimal_separator)$res
          if(length(dec)==0) return(import())
          m1 <- matrix(c(TXT_dot, TXT_comma,".",","),nrow=2)
          dec <- subset(m1, m1[,1] %in% dec)[,2]  
        }
      }
    }
    if(type==TXT_spss_file) {
      #basename(file)->file
      data1<-read.spss(file, to.data.frame=TRUE)
      col.char <-sapply(data1, is.factor)
      if(any(col.char)) data1[col.char] <- lapply(data1[which(col.char)], factor)
    }
    if(type==TXT_csv_file) data1 <- read.csv2(file, header=as.logical(header), sep=sep, dec=dec, na.strings=na.strings)
    if(type==TXT_txt_file) data1 <- read.table(file, header=as.logical(header), sep=sep, dec=dec, na.strings=na.strings)
    if(type==TXT_excel_file){
      basename(file)->file
      writeLines(ASK_specify_datasheet_to_import)
      if(is.null(sheet) || (sheet %in%  excel_sheets(file))==FALSE){
        eval(parse(text=paste0("  dlgList( excel_sheets('",file,
                               "'), preselect=FALSE, multiple = FALSE, title='Quelle feuille ?')$res->sheet")))
        if(length(sheet)==0) return(import())    
      }
      
      eval(parse(text=paste0("data1 <- read_excel(path='", file, "', sheet='", sheet, "', col_names=as.logical(", header, "), na='", na.strings, "')")))
      col.char <-sapply(data1, is.character)
      if(any(col.char)) data1[col.char] <- lapply(data1[which(col.char)], factor)
    }
    if(dial)  { 
      if(type==TXT_excel_file) name<-sheet else name<-file
      name <- dlgInput(ASK_name_for_dataset, name)$res
    if(length(name)==0) name <- "data1"
    name <- strsplit(name, ":")
    name <- tail(name[[1]],n=1)
    }
    if(grepl("[^[:alnum:]]", name)) {
      writeLines(INFO_unauthorized_char_replaced)
      gsub("[^[:alnum:]]", ".", name)->name
    }
     nameV<-replace_non_ascii(names(data1))
     names(data1)<-nameV
     data1<-data.frame(data1)
    
    if(any(nchar(names(data1))>30)) {
      dlgMessage(ASK_shorten_long_variables_names, "yesno")$res->rn
      if(rn=="yes"){
        which(nchar(names(data1))>30)->rn
        for(i in 1:length(rn)) {
          rn2<- dlgInput(ASK_name_to_attribute_to, colnames(data1)[rn[i]])$res 
          if(length(rn2)!=0){
            strsplit(rn2, ":")->rn2
            tail(rn2[[1]],n=1)->colnames(data1)[rn[i]]
          }
          
        }
      }
    }
    
    if(any( grepl("[^[:alnum:][:space:]_.]", names(data1)))) {
      writeLines(INFO_avoid_spaces_and_punctuations)
      dlgMessage(ASK_rename_variables_with_special_char, "yesno")$res->rn
      if(rn=="yes"){
        grep("[^[:alnum:][:space:]_.]", names(data1))->rn
        for(i in 1:length(rn)) {
          rn2<- dlgInput(ASK_name_to_attribute_to, colnames(data1)[rn[i]])$res 
          strsplit(rn2, ":")->rn2
          tail(rn2[[1]],n=1)->colnames(data1)[rn[i]]
        }
      }
    }
    
    if(any(is.na(data1))){
      writeLines(INFO_number_of_missing_values)
      print(sapply(data1, function(x) sum(length(which(is.na(x))))) )
    }
    
    
    assign(x=name, value=data1, envir=.GlobalEnv)
    try(View(data1, TXT_your_data), silent=T)
    str(data1)
    Resultats <- INFO_succesfully_imported
    call.txt<-paste0("import(file='", file, "',dir='",getwd(),"',type='",type,"',dec='",dec, 
                     "',sep='",sep,"',na.strings='", na.strings,"',sheet=" ,
                     ifelse(is.null(sheet), "NULL",paste0("'", sheet,"'")),",name='",name,"')")
    Resultats$call<-call.txt
    .add.history(data=data1, command=Resultats$Call, nom=name)
    return(Resultats)
    
  }
