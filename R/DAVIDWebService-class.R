#' Main class to connect to DAVID Web Service 
#'
#' A reference class to manage DAVID's Web Service connectivity, to run Set 
#' Enrichment Analysis (SEA) or Modular Enrichment Analysis (MEA) on a
#' candidate list of gene/protein(s) with respect to a background list (the
#' genome of the organism by default).
#'
#' \code{DAVIDWebService} class is implemented as a reference class, to manage a
#' single instance connection to DAVIS's server by means of web services using
#' a registered e-mail. For user registration, go to
#' \url{http://david.abcc.ncifcrf.gov/webservice/register.html}. The
#' implementation uses Java Remote Method Implementation (RMI) to connect the
#' client and server side of DAVID. The main functionalities include:
#' \enumerate{
#' \item Connectivity: upload gene/background list/s, change gene/background 
#'  position, select current specie/s, select annotations, etc. from R.
#' \item Reports: Submitted Gene List, Annotation Category Summary, Gene/Term
#' Clusters, Functional Annotation Chart and Functional Annotation Table as
#' native R objects.
#' }
#'
#' @name DAVIDWebService-class
#' @aliases DAVIDWebService
#' @param ... additional parameters. See Methods section.
#' @docType class
#'
#' @section Fields:
#' \describe{
#'  \item{\code{stub}:}{Java jobjRef which corresponds to a 
#'  sample/session/client/stub/DAVIDWebServiceStub object for the client side of
#'  DAVID.} 
#' \item{\code{email}:}{character.} 
#' }
#'
#' @section Methods:
#' \describe{
#' \item{\code{show()}: }{prints DAVIDWebService object.}
#' \item{\code{summary()}: }{return a data.frame with a summary of all available
#'  annotations in DAVID in terms of percentage of gene list ids present in the
#'  category and numbers of terms where they can be found (see
#' getAnnotationSummary)}
#' \item{\code{initialize(email="", ..., url)}:}{constructor for
#' DAVIDWebService object, which includes: Java Virtual Machine initialization
#' (... if required), and stub initialization with the provided email (if
#' present) and using the url parameter for the API website.} 
#' \item{\code{setEmail(mail)}: }{Set the email field with the given
#' registered user email parameter for authentication purposes.}
#' \item{\code{getEmail()}: }{Returns the current authentication email in use.}
#' \item{\code{getStub}: }{Returns jobjRef object with the current stub field
#' in use.}
#' \item{\code{is.connected()}: }{Check if connected to the DAVID server.}
#' \item{\code{connect()}: }{Try to establish a connection with the DAVID server
#' using the provided email.}
#' \item{\code{getIdTypes()}: }{Returns all acceptable DAVID idTypes.}
#' \item{\code{getAllAnnotationCategoryNames()}: }{Returns all available
#' annotation category names.}
#' \item{\code{getDefaultCategoryNames()}: }{Returns all default category
#' names.}
#' \item{\code{getGeneListNames()}: }{Returns submitted gene list names.}
#' \item{\code{getBackgroundListNames()}: }{Returns submitted background names.}
#' \item{\code{getListName(listType=c("Gene", "Background"), position=1L)}:}{Get
#' the name of the selected list type at a given position.}
#' \item{\code{getSpecieNames()}: }{Return specie/s of the current gene list.}
#' \item{\code{getCurrentGeneListPosition()}: }{Return the position of current
#' gene list.}
#' \item{\code{getCurrentBackgroundListPosition()}: }{Return the position of
#' current background list.}
#' \item{\code{getCurrentSpeciesPosition()}: }{Return current specie/s used
#' positions for the uploaded gene list.}
#' \item{\code{setCurrentGeneListPosition(position)}: }{Use the gene list of the
#' given position.}
#' \item{\code{setCurrentBackgroundPosition(position)}: }{Use the gene list of
#' the given position.}
#' \item{\code{setCurrentSpecies(species)}: }{Select the specie/s of the
#' submitted gene list to use in the analysis.}
#' \item{\code{setAnnotationCategories(categories)}: }{Select the specie/s of
#' the submitted gene list to use in the analysis.}
#' \item{\code{addList(inputIds, idType, listName, listType=c("Gene",
#' "Background"))}: }{Add a gene or background to the current session.}
#' \item{\code{getGeneCategoriesReport()}: }{Get the gene report categories.}
#' \item{\code{getAnnotationSummary()}: }{Generate the summary of all available
#' annotation in DAVID in terms of percentage of gene list ids present in the
#' category and numbers of terms where the can be found.}
#' \item{\code{getGeneListReportFile(fileName)}: }{Generate the Gene List Report
#' a.k.a Show Gene List in DAVID website and save it into a file.}
#' \item{\code{getGeneListReport()}: }{getGeneListReport but as an R object.}
#' \item{\code{getFunctionalAnnotationChartFile(fileName, threshold=0.1,
#' count=2L)}: }{Generate the Functional Annotation Chart Report for the
#' selected functional categories, for the given EASE threshold and number of
#' genes and save it to a file.}
#' \item{\code{getFunctionalAnnotationChart(...)}:}{getFunctionalAnnotationChart
#' but as an R object.}
#' \item{\code{getClusterReportFile(fileName, type=c("Term", "Gene"),
#' overlap=4L, initialSeed=4L, finalSeed=4L, linkage=0.5,
#' kappa=35L)}:}{Generate the Term/Gene Cluster Report for the given
#' configuration.}
#' \item{\code{getClusterReport(type=c("Term", "Gene"), ...)}: }{Wrapper for
#' getClusterReportFile function.}
#' \item{\code{getFunctionalAnnotationTableFile(fileName)}: }{Generate
#' Functional Annotation Table Report File, which is a gene-centric view of the
#' genes and their associated annotation terms (selected only). There is no
#' statistics applied in this report.}
#' \item{\code{getFunctionalAnnotationTable()}: }{getFunctionalAnnotationTable
#' but as an R object.}
#' }
#'
#' @section Limitations:
#' \enumerate{
#'   \item A job with more than 3000 genes to generate gene or term cluster
#'    report will not be handled by DAVID due to resource limit.
#'   \item No more than 200 jobs in a day from one user or computer.
#'   \item DAVID Team reserves right to suspend any improper uses of the web 
#'    service without notice.
#' }
#'
#' @author Cristobal Fresno \email{cristobalfresno@@gmail.com} and Elmer A. 
#' Fernandez \email{elmerfer@@gmail.com}
#'
#' @references 
#' \enumerate{
#'  \item The Database for Annotation, Visualization and Integrated Discovery 
#'   (\url{david.abcc.ncifcrf.gov})
#'  \item Huang, D. W.; Sherman, B. T.; Tan, Q.; Kir, J.; Liu, D.; Bryant, D.;
#'  Guo, Y.; Stephens, R.; Baseler, M. W.; Lane, H. C. & Lempicki, R. A. DAVID
#'  Bioinformatics Resources: expanded annotation database and novel algorithms
#'  to better extract biology from large gene lists. Nucleic Acids Res,
#'  Laboratory of Immunopathogenesis and Bioinformatics, SAIC-Frederick, Inc.,
#'  National Cancer Institute at Frederick, MD 21702, USA., 2007, 35, W169-W175
#'  \item Huang, D. W.; Sherman, B. T. & Lempicki, R. A. Bioinformatics
#'   enrichment tools: paths toward the comprehensive functional analysis of
#'   large gene lists. Nucleic Acids Res, Laboratory of Immunopathogenesis and
#'   Bioinformatics, Clinical Services Program, SAIC-Frederick, Inc., National
#'   Cancer Institute at Frederick, Frederick, MD 21702, USA., 2009, 37, 1-13
#'  \item Xiaoli Jiao, Brad T. Sherman, Da Wei Huang, Robert Stephens, Michael 
#'   W. Baseler, H. Clifford Lane, Richard A. Lempicki, DAVID-WS: A Stateful Web
#'   Service to Facilitate Gene/Protein List Analysis Bioinformatics 2012
#'   \url{doi:10.1093/bioinformatics/bts251}
#'  \item Cristobal Fresno, Elmer A. Fernandez (2013) RDAVIDWebService: a 
#'  versatile R interface to DAVID, Bioinformatics, 29(21), 2810-2811.,
#'  \url{http://bioinformatics.oxfordjournals.org/content/29/21/2810.}
#' }
#'
#' @keywords DAVID SEA MEA
#' @family DAVIDWebService
#' @exportPattern '^[^\\.]'
DAVIDWebService <- setRefClass(
  Class="DAVIDWebService",
  fields=list(stub="jobjRef", email="character"),
  methods=list(
    show=function(){
     'Method to show a DAVIDWebService object'

      cat("DAVIDWebService object to access DAVID's website. \n")
      cat("User email: ", ifelse(length(email)==0, "NOT REGISTERED YET!!!", 
        email), "\n")
      if(is.connected()){
        if(length(getGeneListNames())!=0){
          ##Gene List
          cat("Available Gene List/s: ", "\n")
          geneLists<-data.frame(Name=getGeneListNames(), Using="",
            stringsAsFactors=FALSE)
          geneLists$Using[getCurrentGeneListPosition()]<-"*"
          methods::show(geneLists)

          ##Species
          cat("Available Specie/s: ", "\n")
          species<-data.frame(Name=getSpecieNames(), Using="",
            stringsAsFactors=FALSE)
          species$Using[getCurrentSpeciesPosition()]<-"*"
          methods::show(species)

          ##Background List
          cat("Available Background List/s: ","\n")
          backgroundList<-data.frame(Name=getBackgroundListNames(), Using="",
            stringsAsFactors=FALSE)
          backgroundList$Using[getCurrentBackgroundListPosition()]<-"*"
          methods::show(backgroundList)
        }else{
          cat("Available Gene List/s: not submited yet. \n")
        }
      }
      return(invisible(NULL))
    },
    summary=function(){
      'DAVIDWebService object summary

      @return a data.frame with a summary of all available annotation in DAVID 
      in terms of percentage of gene list ids present in the category and
      numbers of terms where they can be found (see getAnnotationSummary).

      @see getAnnotationSummary function for details
      '

      ##Default output
      out<-data.frame(Main.Category=character(0), ID=integer(0),
        Name=character(0), X.=numeric(0), Count=integer(0))

      ##Sanity Check
      if(is.connected()){
        if(length(getGeneListNames())!=0){
          ##The summary
          out<-getAnnotationSummary()
        }
      }

      ##Console output
      show()
      
      return(out)
    },
    initialize=function(email="", ... , 
    url="http://david.abcc.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/"){
      'Initialization of DAVIDWebService object. 

       Creation of a new object includes: Java Virtual Machine initialization 
       (if required), and stub initialization with provided email (if provided).

       @param  ... : Java Virtual Machine options (if required).
       @param email: registered user email to access DAVID. For user
       registration, go to
       http://david.abcc.ncifcrf.gov/webservice/register.html
       @param url: DAVID web service url. Default value
       http://david.abcc.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/
      '
  
      ##Initialization of Java Virtual Machine and sanity check
      callSuper()
      stopifnot(.jinit(...)==0)
      ##Add required imports using the .jar files in ./java directory
      stopifnot(.jpackage("RDAVIDWebService"))

      ##Create DAVIDWebServiceStub object
      stub<<-.jnew("sample/session/client/stub/DAVIDWebServiceStub", url)
      stopifnot(!is.jnull(stub))
      stub$"_getServiceClient"()$getOptions()$setManageSession(TRUE)

      ##Register email and check for internet connectivity (if available)
      if(email != ""){
        .self$email<<-email
        connect()
      }
    },
    ##################################################################################################
    ##FIELD accessors
    ##################################################################################################
    ##email
    setEmail=function(mail){
      'Set the email field with the given parameter

      @param mail character with the registered user email for authentication 
      purposes
      '

      email<<-mail
    },
    getEmail=function(){
      'Returns the current authentication email in use
      
      @return character with the email field
      '

      return(email)
    },
    getStub=function(){
      'Returns the current stub 
      
      @return jobjRef with the stub field
      '

      return(stub)
    },
    ##################################################################################################
    ##DAVID accessors
    ##################################################################################################
    is.connected=function(){
      'Check if connected to DAVID server

      @return TRUE if user has registered email with DAVID knowledge base, FALSE
      otherwise.
      '

      ans<-FALSE
      if(length(email)!=0){ 
        tryCatch(ans<-stub$getAllListNames() != "Authentication Failed",
          error=function(e)stop(e))
      }

      return(ans)
    },
    connect=function(){
      'Try to establish a connection with DAVID server using the provided email

      @return logical 
      '
      
      ##Default answer
      logicalAns<-FALSE

      ##Check email field
      if(length(email) == 0){
        stop(paste("email field is empty!!! Please provide a registed e-mail",
          "in order to connect"))

      }else{
        ##Try to establish connection
        tryCatch(ans<-.jcall(stub, "S", "authenticate", email),
          error=function(e)stop(e))
        logicalAns<-as.logical(ans)

        ##Server message
        if(is.na(logicalAns)){
          warning(ans)
          logicalAns<-FALSE
        }else{
          ##Server ban the email
          if(!logicalAns){
          warning("Not connected. Probable causes: exceded web server access",
            "policy, submit jobs in the next 24 hrs!! If the problem persists",
            "contact DAVID Web Service admin.")
          }
        }
      }

      return(logicalAns)
    },
    ##DAVID basic constants
    getIdTypes=function(){
      'Returns all acceptable DAVID idTypes

       @return character vector with the available DAVID input ID type
      '

      stopifnot(is.connected())
      return(strsplit(stub$getConversionTypes(), split=",")[[1]])
    },
    getAllAnnotationCategoryNames=function(){
      'Returns all available annotation category names

       @return character vector with the available DAVID annotation categories
      '
      
      stopifnot(is.connected())
      return(strsplit(stub$getAllAnnotationCategoryNames(), split=",")[[1]])
    },
    getDefaultCategoryNames=function(){
      'Returns all default category names
  
      @return character vector with a subset of the available DAVID annotation categories, chosen by default
      '

      stopifnot(is.connected())
      return(strsplit(stub$getDefaultCategoryNames(), split=",")[[1]])
    },
    ##DAVID gene/background lists getters
    ##Names
    getGeneListNames=function(){
      'Returns all list names

      @return a character vector with the name of the submitted gene list/s
      '

      stopifnot(is.connected())
      return(strsplit(stub$getAllListNames(), split=",")[[1]])
    },
    getBackgroundListNames=function(){
      'Returns background names
      
      @return a character vector with the name of the available background gene 
      list/s for the submitted gene list/s
      '
  
      stopifnot(is.connected())
      return(strsplit(stub$getAllPopulationNames(), split=",")[[1]])
    },
    getListName=function(listType=c("Gene", "Background"), position=1L){
      'Get the name of the selected list type at a given position
      
      @param listType character with the type of list (Gene, Background).
      Default value is "Gene".
      @param integer with the position of the list.

      @return character with the name of the list.
      '

      ##Sanity Check
      stopifnot(listType %in% c("Gene", "Background"))
      stopifnot(is.numeric(position))
      stopifnot(is.connected())
      stopifnot(position %in% seq(along=switch(listType[1], 
        Gene=getGeneListNames(), 
        Background=getBackgroundListNames())))
      
      ##Get the name of the list
      position<-as.integer(position-1)
      listType<-as.integer(which(c("Gene", "Background") %in% listType[1]>0)-1)
      return(stub$getListName(listType, position))
    },
    getSpecieNames=function(){
      'Return specie/s of the current gene list.

       @return a character vector with the specie/s and in brackets the number
         of DAVID Ids of the current gene list, e.g. Homo sapiens(155)
      '
      
      stopifnot(is.connected())
      return(strsplit(stub$getSpecies(), split=",")[[1]])
    },
    ##Positions
    getCurrentGeneListPosition=function(){
      'Return the position of current gene list.

      @return integer with the position of current gene list if available, 
        NA_integer_ otherwise.
      '
  
      stopifnot(is.connected())
      ##Check if there is an uploaded Gene List.
      ans<-ifelse(length(getGeneListNames())==0, NA_integer_,
        as.integer(stub$getCurrentList()+1))
      return(ans)
    }, 
    getCurrentBackgroundListPosition=function(){
      'Return the position of current background list.

       @return integer with the position of current background list if
         available, NA_integer_ otherwise.
      '
      
      stopifnot(is.connected())
      ##Check if there is an uploaded Background List.
      ans<-ifelse(length(getBackgroundListNames())==0, NA_integer_,
        as.integer(stub$getCurrentPopulation()+1))
      return(ans)
    },
    getCurrentSpeciesPosition=function(){
      'Return current specie/s used positions for the uploaded gene list.

      @return a integer vector with the specie/s position under use for the
        gene list under use if available, NA_character_ otherwise.
      '

      stopifnot(is.connected())
      ##Check if there is an uploded Gene List.
      ans<-ifelse(length(getSpecieNames())==0, NA_character_,
        as.integer(strsplit(stub$getCurrentSpecies(), split=",")[[1]])+1)
      return(ans)
    },
    ##DAVID gene/background lists and Species setters
    setCurrentGeneListPosition=function(position){
      'Use the gene list of the given position.
      
      @param position integer with the position of the gene list to set
      '

      ##Sanity Check
      stopifnot(is.connected())
      stopifnot(is.numeric(position))
      stopifnot(all(position %in% seq(along=getGeneListNames())))
      position<-as.integer(position-1)
      stub$setCurrentList(position)
      return(invisible(NULL))
    },
    setCurrentBackgroundPosition=function(position){
      'Use the background list of the given position.
      
      @param position integer with the position of the background list to set.
      '

      ##Sanity Check
      stopifnot(is.connected())
      stopifnot(is.numeric(position))
      stopifnot(all(position %in% seq(along=getBackgroundListNames())))
      position<-as.integer(position-1)
      stub$setCurrentPopulation(position)
      return(invisible(NULL))
    },
    setCurrentSpecies=function(species){
      'Select the specie/s of the submitted gene list to use in the analysis.

      @param species numeric vector with the specie/s to use.
      '

      ##Sanity Check
      stopifnot(is.connected())
      stopifnot(is.numeric(species))
      stopifnot(all(species %in% seq(along=getSpecieNames())))
      
      ##Set the species to use
      species<-gsub(pattern=", ", replacement=",", toString(species-1))    
      stub$setCurrentSpecies(species)
      return(invisible(NULL))
    },
    setAnnotationCategories=function(categories){
      'Let the user to select specific annotation categories.

      @param categories: character vector with the category name/s to use in 
        the analysis.
      '

      ##Sanity Check
      stopifnot(is.connected())
      stopifnot(all(categories %in% getAllAnnotationCategoryNames()))

      ##Set the categories to use
      categories<-gsub(pattern=", ", replacement=",", toString(categories))
      categories<-stub$setCategories(categories)

      return(invisible(NULL))
    }, 
    ##################################################################################################
    ##DAVID upload list
    ##################################################################################################
    addList=function(inputIds, idType, listName, listType=c("Gene",
      "Background")){
      'Add a gene or background to the current session.

      @param inputIds: character vector with the associated ids.
      @param idType: character with the type of submitted ids.
      @param listName: character to identify the submitted list.
      @param character with the type of list (Gene, Background). Default value
        is "Gene".

      @return list with the following items:
        \\item{inDavid}{numeric with the percentage of the inputIds in DAVID
          knowledge database.}
        \\item{unmappedIds}{character vector with the unmapped ids if listType
          is "Gene", NA_character_ otherwise.}
      '
      
      ##Sanity check
      stopifnot(listType[1] %in% c("Gene", "Background"))
      stopifnot(length(inputIds)!=0)
      stopifnot(length(listName)!=0)
      stopifnot(is.connected())
      stopifnot(idType %in% getIdTypes())
      stopifnot(!listName %in% switch(listType[1], 
        Gene=getGeneListNames(), 
        Background=getBackgroundListNames()))
      listType<-as.integer(which(c("Gene", "Background") %in% listType[1]>0)-1)

      ##The actual submission
      inputIds<-gsub(pattern=" ", replacement="", toString(inputIds))
      ans<-stub$addList(inputIds, idType, listName, listType)

      ##Sanity check
      if(ans < 0.5){
        warning("More than half of the list is not present in DAVID. Check if",
          "the list match the id type!!!")
      }

      ##Check for unmaped ids for Gene list only
      unmappedIds<-NA_character_
      if(listType == 0L){   
        genes<-getGeneListReport()
        inputIds<-unlist(strsplit(inputIds, split=","))
        unmappedIds<-inputIds[!inputIds %in% genes$ID]
      }

      return(list(inDavid=ans, unmappedIds=unmappedIds))
    },
    ##################################################################################################
    ##DAVID Reports
    ##################################################################################################
    getGeneCategoriesReport=function(){
      'Get the gene report categories.

      @return integer vector with the IDs of the categories.
      '
      
      ##Sanity Check
      stopifnot(is.connected()) 

      ##Get the report
      return(unlist(strsplit(stub$getGeneReportCategories(), split=",")))
    },
    getAnnotationSummary=function(){
      'Generate the summary of all available annotation in DAVID in terms of 
       percentage of gene list ids present in the category and numbers of terms
       where the can be found.

      @return data.frame with the annotation summary report with the following 
        columns:
        \\item{Main.Category}{factor with the main categories under used in the
        present analysis.}
        \\item{ID}{integer to identify the annotation category} 
        \\item{Name}{character with the name of category (the available ones in
        getAllAnnotationCategoryNames function).}
        \\item{X.}{numeric with the percentage of the gene list ids present in
        the term.}
        \\item{Count}{integer with the number of ids of the gene list that
        belong to this term.}
      '

      ##Sanity Check
      stopifnot(is.connected()) 

      ##Get the report if possible
      summaryReport<-.jnull()
      if(length(getGeneListNames())!=0){
        summaryReport<-stub$getSummaryReport()
      }
      
      ##Parse the report to a file in the java side (save time)
      davidParser<-.jnew("sample/session/client/DAVIDParser")
      stopifnot(!is.jnull(davidParser))
      tempFileName<-tempfile()
      davidParser$writeSummaryReport(tempFileName, summaryReport)
      davidParser<-NULL

      ##Import data to R
      annotationSummary<-read.table(file=tempFileName, header=TRUE, sep="\t")
      return(annotationSummary)
    },
    getGeneListReportFile=function(fileName){
      'Generate the Gene List Report a.k.a Show Gene List in DAVID website and 
      save it into a file.

      @param fileName: character with the name of the file to store the Gene 
      List Report.

      @see also getGeneListReport for a description of the file.
      '

      ##Sanity Check
      stopifnot(length(fileName)!=0)
      stopifnot(is.connected()) 

      ##Get the report if possible
      geneList<-.jnull()
      if(length(getGeneListNames())!=0){
        geneList<-stub$getListReport()
      }
      
      ##Parse the report to a file in the java side (save time)
      davidParser<-.jnew("sample/session/client/DAVIDParser")
      stopifnot(!is.jnull(davidParser))
      davidParser$writeListReport(fileName, geneList, idType="ID")
      davidParser<-NULL
      geneList<-NULL

      return(invisible(NULL))     
    },
    getGeneListReport=function(){ 
      'Generate Gene List Report a.k.a Show Gene List in DAVID website.

      @return data.frame with the Gene List Report with the following columns:
      \\item{ID}{character with the Gene List ID present in DAVID knowledge
      base, in the submitted type. If more than one ids map to the same DAVID
      ID, the record is a comma separated character.}
      \\item{Name}{character with the name of the gene as seen in DAVID
      knowledge base, in a comma separated fashion (if more than one ID maps to
      the same DAVID ID).}
      \\item{Species}{factor with the name of the Specie.}
      '    
       
      ##Get the Report
      tempFileName<-tempfile()      
      getGeneListReportFile(fileName=tempFileName)

      ##Import data to R
      return(new("DAVIDGenes", fileName=tempFileName))
    },
    getFunctionalAnnotationChartFile=function(fileName, threshold=0.1,
      count=2L){
      'Generate the Functional Annotation Chart Report for the selected 
       functional categories, for the given EASE threshold and number of genes
       and save it to a file.

      @param fileName: character with the name of the file to store the 
      Functional Annotation Chart Report.
      @param threshold: numeric with the EASE score (at most equal) that must 
      be present in the category to be included in the report. Default value is
      0.1.
      @param count: integer with the number of genes (greater equal) that must 
      be present in the category to be included in the report. Default value is
      2.

      Functional Chart report with the following columns:
      \\item{Category}{factor with the main categories under used in the present
      analysis.}
      \\item{Term}{character with the name of the term in format id~name (if
      available).}
      \\item{Count}{integer with the number of ids of the gene list that belong
      to this term.}
      \\item{X.}{after converting user input gene IDs to  corresponding DAVID
      gene ID, it refers to the percentage of DAVID genes in the list
      associated with a particular annotation term. Since DAVID gene ID is
      unique per gene, it is more accurate to use DAVID ID percentage to
      present the gene-annotation association by removing any redundancy in user
      gene list, i.e. two user IDs represent same gene.}
      \\item{PValue}{numeric with the EASE Score of the term (see DAVID Help
      page).}
      \\item{Genes}{character in comma separated style with the genes present in
      the term.}
      \\item{List.Total, Pop.Hits, Pop.Total}{integers (in addition to Count) to
      build the 2x2 contingency table in order to compute the EASE Score (see
      DAVID Help page).}
      \\item{Fold.Enrichment}{numeric with the ratio of the two proportions. For
      example, if 40/400 (i.e. 10%) of your input genes involved in "kinase
      activity" and the background information is 300/30000 genes (i.e. 1%)
      associating with "kinase activity", roughly 10% / 1% = 10 fold
      enrichment.}
      \\item{Bonferroni, Benjamini, FDR}{numerics with p-value adjust different
      criteria (see p.adjust).}

      @references DAVID Help page http://david.abcc.ncifcrf.gov/helps/functional_annotation.html#E3

      @see also p.adjust, fisher.test, getFunctionalAnnotationChart
      '

      ##Sanity Check
      stopifnot(length(fileName)!=0)
      stopifnot(threshold <= 1 && threshold >=0)
      stopifnot(count >= 0)
      stopifnot(is.connected()) 
    
      ##Get the report is possible
      count<-as.integer(count)
      FunChart<-.jnull()
      if(length(getGeneListNames())!=0){
        FunChart<-stub$getChartReport(threshold, count)
      }
      
      ##Parse the report to a file in the java side (save time)
      davidParser<-.jnew("sample/session/client/DAVIDParser")
      stopifnot(!is.jnull(davidParser))
      davidParser$writeChartReport(fileName, FunChart)
      davidParser<-NULL
      FunChart<-NULL

      return(invisible(NULL))
    },
    getFunctionalAnnotationChart=function(...){
      'Generate the Functional Annotation Chart Report for the selected 
      functional categories, for the given EASE threshold and number of genes.

      @param ...: additional parameters for getFunctionalAnnotationChartFile 
        function (threshold and count).
    
      @return data.frame with the Functional Chart report.

      @see also getFunctionalAnnotationChartFile.
      '

      ##Get the Report
      tempFileName<-tempfile()      
      getFunctionalAnnotationChartFile(fileName=tempFileName, ...)
      
      ##Import data into R
      return(new("DAVIDFunctionalAnnotationChart", fileName=tempFileName))
    },
    getClusterReportFile=function(fileName, type=c("Term", "Gene"), overlap=4L,
      initialSeed=4L, finalSeed=4L, linkage=0.5, kappa=35L){
      'Generate the Term/Gene Cluster Report for the given configuration.

      @param fileName: character with the name of the file to store the Cluster
        Report.
      @param type: character with the type of cluster to obtain Term/Genes. 
        Default value "Term".
      @param overlap: integer with the minimum number of annotation terms 
        overlapped between two genes in order to be qualified for kappa
        calculation. This parameter is to maintain necessary statistical power
        to make kappa value more meaningful. The higher value, the more
        meaningful the result is. Default value is 4L.
       @param initialSeed,finalSeed: integer with the number of genes in the
        initial (seeding) and final (filtering) cluster criteria. Default value
        is 4L for both.
      @param linkage: numeric with the percentage of genes that two clusters 
        share in order to become one.
      @param kappa: integer (kappa * 100), with the minimum kappa value to be 
        considered biological significant. The higher setting, the more genes
        will be put into unclustered group, which lead to higher quality of
        functional classification result with a fewer groups and a fewer gene
        members. Kappa value 0.3 starts giving meaningful biology based on our
        genome-wide distribution study. Anything below 0.3 have great chance to
        be noise.

      Cluster Report will include the following data: 
      \\item{Annotation/Gene Cluster}{integer with the number of cluster.}
      \\item{EnrichmentScore}{numeric with the geometric mean (in -log scale)
      of members p-values in a corresponding annotation cluster, is used to
      rank their biological significance. Thus, the top ranked annotation groups
      most likely have consistent lower p-values for their annotation members.
      \\item{Members}{according to the type of cluster, changes the associated
      data to include Gene List or Functional Chart Report (see
      getGeneListReport and getFunctionalAnnotationChart).}

      @see also getGeneListReport, getFunctionalAnnotationChart.

      @references 
      \\enumerate{
        \\item DAVID Clustering Help page
        http://david.abcc.ncifcrf.gov/helps/functional_classification.html#clustering
        \\item Cohen, J: A coefficient of agreement for nominal scales,
        Educational and Psychological Measurement, 1960, 20, 37-46.
      }
      '

      ##Sanity Check
      stopifnot(length(fileName)!=0)
      stopifnot(type[1] %in% c("Term", "Gene"))
      overlap<-as.integer(overlap)
      initialSeed<-as.integer(initialSeed)
      finalSeed<-as.integer(finalSeed)
      kappa<-as.integer(kappa)
      linkage<-as.numeric(linkage)
      stopifnot(!any(is.na(c(overlap, initialSeed, finalSeed, kappa, linkage))))
      stopifnot(all(c(overlap, initialSeed, finalSeed, kappa, linkage)>0))
      stopifnot(is.connected()) 

      ##Get the report if possible
      cluster<-.jnull()
      if(length(getGeneListNames())!=0){
    
        ##Check Web Service limitations, at most 3000 genes to make the cluster
        ##works.
        idsInUse<-gsub(pattern="[[:alpha:][:space:][:punct:]]",
          replacement="", getSpecieNames()[getCurrentSpeciesPosition()])
        idsInUse<-sum(as.integer(idsInUse))
        if(idsInUse < 3000){
          cluster<-switch(type[1],     
            Term=stub$getTermClusterReport(overlap, initialSeed, finalSeed,
              linkage, kappa),
            Gene=stub$getGeneClusterReport(overlap, initialSeed, finalSeed,
              linkage, kappa))
        }else{
          warning("DAVID does not provide service if the number of genes in",
            "your list is over 3000. Returning an empty cluster")
        }
      }#check for results

      ##Parse the report to a file in the java side (save time)
      davidParser<-.jnew("sample/session/client/DAVIDParser")
      stopifnot(!is.jnull(davidParser))
      invisible(switch(type[1], 
        Term=davidParser$writeTermClusterReport(fileName, cluster),
        Gene=davidParser$writeGeneClusterReport(fileName, cluster)))
      davidParser<-NULL
      cluster<-NULL
      
      return(invisible(NULL))
    },
    getClusterReport=function(type=c("Term", "Gene"), ...){
      'Generate the Term/Gene Cluster Report for the given configuration.
      
      @param ...: additional parameters for getClusterReportFile function
      (type, overlap and so on).

      @return a list of cluster where each one includes: 
      \\item{AnnotationCluster}{integer with the number of cluster.}
      \\item{EnrichmentScore}{numeric with the geometric mean (in -log scale) of
      members p-values in a corresponding annotation cluster, is used to rank
      their biological significance. Thus, the top ranked annotation groups most
      likely have consistent lower p-values for their annotation members.
      \\item{Members}{data.frame with the same columns of a Functional
      Annotation Chart (see getFunctionalAnnotationChart).}

      @see also getClusterReportFile
      '
      
      ##Get the Report
      tempFileName<-tempfile()      
      getClusterReportFile(fileName=tempFileName, type=type[1], ...)

      ##Import data into R
      return(switch(type[1],
        Term=new("DAVIDTermCluster", fileName=tempFileName), 
        Gene=new("DAVIDGeneCluster", fileName=tempFileName)))
    },
    getFunctionalAnnotationTableFile=function(fileName){
      'Generate Functional Annotation Table Report File, which is a gene-centric
       view of the genes and their associated annotation terms (selected only).
       There is no statistics applied in this report.

       @param fileName: character with the name of the file to store the 
         Functional Annotation Table Report.
      
      Functional Annotation Table Report File includes the following data:
      \\item{Gene}{Three Columns with the same data included in Gene List Report
      (ID, Gene.Name and Species) but coding for DAVID ID, i. e., comma
      separated character with input ids if two or more stands for the same
      gene.}
      \\item{Annotation}{As many columns as Annotation Categories were in used.
      In each column, a comma separated style is use to delimitate the different
      terms where is reported evidence for DAVID ID record.}

      @see also getFunctionalAnnotationTable
      '
      ##Sanity Check
      stopifnot(length(fileName)!=0)
      stopifnot(is.connected()) 

      ##Get the report if posible
      annotationTable<-.jnull()
      if(length(getGeneListNames())!=0){
        annotationTable<-stub$getTableReport()
      }
      
      ##Parse the report to a file in the java side (save time)
      davidParser<-.jnew("sample/session/client/DAVIDParser")
      stopifnot(!is.jnull(davidParser))
      davidParser$writeTableReport(fileName, annotationTable)
      davidParser<-NULL
      annotationTable<-NULL
     
      return(invisible(NULL))
    },
    getFunctionalAnnotationTable=function(){
      'Generate Functional Annotation Table Report, which is a gene-centric view
      of the genes and their associated annotation terms (selected only). There
      is no statistics applied in this report.

      @return list with the Functional Annotation Table Report including the
      following items:
      \\item{Dictionary}{named list with as many items as Functional Categories
      were selected in the present analysis, where each item corresponds a
      data.frame with ID and Term columns}
      \\item{Genes}{data.frame with ID (DAVID ones), Gene.Name and Species
      columns (see getGeneListReport).}
      \\item{Membership}{a named list with as many membership matrix as
      Functional Categories were selected in the present analysis. In this
      context rows stand for genes (identify in Genes) and columns for terms in
      the Dictionary.}
      '

      ##Get the Report
      tempFileName<-tempfile()      
      getFunctionalAnnotationTableFile(fileName=tempFileName)

      ##Import data into R
      return(new("DAVIDFunctionalAnnotationTable", fileName=tempFileName))
    }
  )#end of method declarations
)#end of DAVIDWebService Reference Class
