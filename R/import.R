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
    Results <- list()
    if(info==TRUE) print("What format is your file saved in?")
    if(!is.null(type)){
      type<-switch(type,
                   "csv"="CSV file","CSV file"="CSV file" ,
                   "txt"="Txt file","Txt file"="Txt file" ,
                   "excel"="Excel file","Excel file"="Excel file",
                   "SPSS" =  "SPSS file","SPSS file"="SPSS file" )
    }
    if(is.null(type)) type <- dlgList(c("CSV file", "Txt file", "Excel file", "SPSS file"), preselect="Excel file", multiple = FALSE, title="File format?")$res
    if(length(type)==0) return(donnees())

    if(!is.null(dir)) try(setwd(dir), silent=T)
    if(!is.null(file) && file.exists( file)) {file<-file
    dial<-F}else {dial<-T
    if(grepl("Linux", Sys.info()[[1]])){
	       file <- try(tk_choose.files(), silent=TRUE)
		   }
    else{
	    file <- try(file.choose(), silent=TRUE)
    }
    if(class(file)=="try-error") return(import())
    setwd(dirname(file))
    basename(file)->file
    }



    if(type!="SPSS file"){
      if(dial | (dec %in% c(".",","))==FALSE | (sep %in% c(" ", "\t",";",","))==FALSE |!is.logical(header)){
        if(info==TRUE) print("Are the names of the variables on the first line of your database? Choose TRUE if this is the case")
        header <- dlgList(c(TRUE, FALSE), preselect=TRUE, multiple = FALSE, title="Name of variables?")$res
        if(length(header)==0) return(import())

        if(info==TRUE) print("If some data is missing, how is it defined? You can leave NA if the cells are empty")
        na.strings <- dlgInput("By what value are the missing values defined?", "NA")$res
        if(length(na.strings)==0) na.strings <- "NA"
        na.strings <- strsplit(na.strings, ":")
        na.strings <- tail(na.strings[[1]],n=1)


        if(type=="CSV file"|type=="Txt file"){
          if(info==TRUE) print("When saving your file, what is the column separation index?")
          sep <- dlgList(c("space","tab","semicolon","comma"), preselect="semicolon", multiple = FALSE, title="Column separator")$res
          if(length(sep)==0) return(import())
          m1 <- matrix(c("space","tab","semicolon","comma"," ","\t",";",","),nrow=4)
          sep <- subset(m1, m1[,1] %in% sep)[,2]

          if(info==TRUE) print("If some data contains decimals, what is the symbol indicating the decimal?")
          dec <- dlgList(c("point", "comma"), preselect=NULL, multiple = FALSE, title="Decimal separator")$res
          if(length(dec)==0) return(import())
          m1 <- matrix(c("point", "comma",".",","),nrow=2)
          dec <- subset(m1, m1[,1] %in% dec)[,2]
        }
      }
    }
    if(type=="SPSS file") {
      #basename(file)->file
      data1<-read.spss(file, to.data.frame=TRUE)
      col.char <-sapply(data1, is.factor)
      if(any(col.char)) data1[col.char] <- lapply(data1[which(col.char)], factor)
    }
    if(type=="CSV file") data1 <- read.csv2(file, header=as.logical(header), sep=sep, dec=dec, na.strings=na.strings)
    if(type=="Txt file") data1 <- read.table(file, header=as.logical(header), sep=sep, dec=dec, na.strings=na.strings)
    if(type=="Excel file"){
      basename(file)->file
      writeLines("Please specify the spreadsheet you want to import")
      if(is.null(sheet) || (sheet %in%  excel_sheets(file))==FALSE){
        eval(parse(text=paste0("  dlgList( excel_sheets('",file,
                               "'), preselect = FALSE, multiple = FALSE, title =' Which sheet? ') $ res-> sheet")))
        if(length(sheet)==0) return(import())
      }

      eval(parse(text=paste0("data1 <- read_excel(path='", file, "', sheet='", sheet, "', col_names=as.logical(", header, "), na='", na.strings, "')")))
      col.char <-sapply(data1, is.character)
      if(any(col.char)) data1[col.char] <- lapply(data1[which(col.char)], factor)
    }
    if(dial)  {
      if(type=="Excel file") name<-sheet else name<-file
      name <- dlgInput("What name do you want to give to the data?", name)$res
    if(length(name)==0) name <- "data1"
    name <- strsplit(name, ":")
    name <- tail(name[[1]],n=1)
    }
    if(grepl("[^[:alnum:]]", name)) {
      writeLines("Unauthorized characters were used for the name. These characters have been replaced by periods")
      gsub("[^[:alnum:]]", ".", name)->name
    }
     nameV<-replace_non_ascii(names(data1))
     names(data1)<-nameV
     data1<-data.frame(data1)

    if(any(nchar(names(data1))>30)) {
      dlgMessage("Some variables have particularly long names that can interfere with reading. Do you want to shorten them?", "yesno")$res->rn
      if(rn=="yes"){
        which(nchar(names(data1))>30)->rn
        for(i in 1:length(rn)) {
          rn2<- dlgInput("What name do you want to give to", colnames(data1)[rn[i]])$res
          if(length(rn2)!=0){
            strsplit(rn2, ":")->rn2
            tail(rn2[[1]],n=1)->colnames(data1)[rn[i]]
          }

        }
      }
    }

    if(any( grepl("[^[:alnum:][:space:]_.]", names(data1)))) {
      writeLines("Avoid spaces and punctuation marks, except. and _ ")
      dlgMessage("Some variable names contain special characters that can create bugs. Do you want to rename these variables?", "yesno")$res->rn
      if(rn=="yes"){
        grep("[^[:alnum:][:space:]_.]", names(data1))->rn
        for(i in 1:length(rn)) {
          rn2<- dlgInput("What name do you want to give to", colnames(data1)[rn[i]])$res
          strsplit(rn2, ":")->rn2
          tail(rn2[[1]],n=1)->colnames(data1)[rn[i]]
        }
      }
    }

    if(any(is.na(data1))){
      writeLines("Number of missing values per variable")
      print(sapply(data1, function(x) sum(length(which(is.na(x))))) )
    }


    assign(x=name, value=data1, envir=.GlobalEnv)
    try(View(data1, "Your data"), silent=T)
    str(data1)
    Results <- "the data has been imported correctly"
    call.txt<-paste0("import(file='", file, "',dir='",getwd(),"',type='",type,"',dec='",dec,
                     "',sep='",sep,"',na.strings='", na.strings,"',sheet=" ,
                     ifelse(is.null(sheet), "NULL",paste0("'", sheet,"'")),",name='",name,"')")
    Results$call<-call.txt
    .add.history(data=data1, command=Results$Call, nom=name)
    return(Results)

  }
