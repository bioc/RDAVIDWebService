#' High level constructors for DAVIDWebService package's classes.
#'
#' Different ways to build the different DAVIDWebService's object according to
#' the signature in use. 
#' 
#' @param object could be a character with the file name of the .tab report or 
#'  data.frame already loaded.
#' @param fileName character with the file name of the .tab report to load.
#' @param .Object character to use in new function call. Possible values are:
#'  "DAVIDGenes", "DAVIDFunctionalAnnotationChart" or "DAVIDCluster". 
#' @param Class character to use in the \code{\link{as}} function call. Possible
#'  values are: "DAVIDGenes" and "DAVIDFunctionalAnnotationChart".
#' @param strict,ext  see \code{\link{as}} function.
#' @param funChart DAVIDFunctionalAnnotationChart object.
#' @param type character to indicate Gene Ontology main category: "BP", "MF" or
#' "CC".
#' @param pvalueCutoff numeric >0 <=1 to indicate the p-value to use as the
#' threshold for enrichment. Default value is 0.1
#' @param removeUnattached Should unattached nodes be removed from GO
#' DAG? Default value is FALSE.
#' @param ... Additional parameters for lower level constructors (initialize).
#'
#' @return a DAVIDWebService object according to function call:
#' \item{DAVIDGenes}{object with genes description related data.}
#' \item{DAVIDFunctionalAnnotationChart}{object with the respective report.}
#' \item{DAVIDFunctionalAnnotationTable}{object with the respective report.}
#' \item{DAVIDCluster}{Not possible to invoke as it is a Virtual class.}
#' \item{DAVIDGeneCluster}{object with the respective report.}
#' \item{DAVIDTermCluster}{object with the respective report.}
#' \item{DAVIDGODag}{derived GOstats GO Direct Acyclic Graph from 
#'  DAVIDFunctionalAnnotationChart data.}
#'   
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType methods
#' @exportMethod DAVIDGenes
#' @name DAVIDGenes
#' @rdname DAVIDClasses-constructor
#' @usage DAVIDGenes(object)
#' @aliases DAVIDGenes-methods
#' @family DAVIDGenes
#' @examples
#' {
#' ##DAVIDGenes example:
#' ##Load Show Gene List file report for the input demo file 1, using data
#' ##function. Then, create a DAVIDGenes object using the loaded data.frame
#' ##geneList1. 
#' data(geneList1)
#' davidGenes1<-DAVIDGenes(geneList1)
#'
#' ##In addition, the user can use the file name of the downloaded file report. 
#' ##Here, we need to first uncompressed the report included in the package, in
#' ##order to load it.
#' setwd(tempdir())
#' fileName<-system.file("files/geneListReport1.tab.tar.gz",
#'   package="RDAVIDWebService")
#' untar(fileName)
#' davidGenes1<-DAVIDGenes(untar(fileName,list=TRUE))
#'
#'
#' ##DAVIDFunctionalAnnotationChart example
#' ##Load the Functional Annotation Chart file report for the input demo 
#' ##file 2, using data function. Then, create a DAVIDFunctionalAnnotationChart
#' ## object using the loaded data.frame funChart2.
#' data(funChart2)
#' davidFunChart2<-DAVIDFunctionalAnnotationChart(funChart2)
#'
#' ##In addition, the user can use the file name of the downloaded file report. 
#' ##Here, we need to first uncompressed the report included in the package, in
#' ##order to load it.
#' setwd(tempdir())
#' fileName<-system.file("files/functionalAnnotationChartReport2.tab.tar.gz",
#'   package="RDAVIDWebService")
#' untar(fileName)
#' davidFunChart2<-DAVIDFunctionalAnnotationChart(untar(fileName, list=TRUE))
#' 
#' 
#' ##DAVIDFunctionalAnnotationTable example
#' ##Load the Functional Annotation Table file report for the input demo 
#' ##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
#' ##object using the loaded data.frame annotationTable1. 
#' data(annotationTable1)
#' davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)
#'
#' ##In addition, the user can use the file name of the downloaded file report. 
#' ##Here, we need to first uncompressed the report included in the package, in
#' ##order to load it.
#' setwd(tempdir())
#' fileName<-system.file("files/annotationTableReport1.tab.tar.gz",
#'   package="RDAVIDWebService")
#' untar(fileName)
#' davidFunTable1<-DAVIDFunctionalAnnotationTable(untar(fileName, list=TRUE))
#'
#' 
#' ##Example DAVIDGODag
#' ##Load the Functional Annotation Chart file report for the input demo 
#' ##file 2, using data function. Then, create a DAVIDGODag object using
#' ##Molecular Function main category of DAVIDFunctionalAnnotationChart object,
#' ##obtained from the loaded data.frame funChart2. In addition, we have
#' ##selected a threshold pvalue of 0.001 and removed unattached nodes, in case
#' ##DAVID/GO.db database are not using the same version.
#' data(funChart2)
#' davidGODag<-DAVIDGODag(DAVIDFunctionalAnnotationChart(funChart2), type="MF",
#'   pvalueCutoff=0.001, removeUnattached=TRUE)
#'
#'
#' ##DAVIDGeneCluster example:
#' ##Load the Gene Functional Classification Tool file report for the
#' ##input demo list 1 file to create a DAVIDGeneCluster object.
#' setwd(tempdir())
#' fileName<-system.file("files/geneClusterReport1.tab.tar.gz",
#'   package="RDAVIDWebService")
#' untar(fileName)
#' davidGeneCluster1<-DAVIDGeneCluster(untar(fileName, list=TRUE))
#'
#'
#' ##DAVIDTermCluster example:
#' ##Load the Gene Functional Classification Tool file report for the
#' ##input demo file 2 to create a DAVIDGeneCluster object.
#' setwd(tempdir())
#' fileName<-system.file("files/termClusterReport2.tab.tar.gz",
#'   package="RDAVIDWebService")
#' untar(fileName)
#' davidTermCluster2<-DAVIDTermCluster(untar(fileName, list=TRUE))
#' }
#' 
setGeneric(name="DAVIDGenes", def=function(object){
  standardGeneric("DAVIDGenes")
})
#'
#' @exportMethod DAVIDGenes
#' @name DAVIDGenes
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{DAVIDGenes}{character}(object)
#' @aliases DAVIDGenes,character-method
setMethod(f="DAVIDGenes", signature=signature("character"), 
  definition=function(object){
    new("DAVIDGenes", fileName=object)
})
#'
#' @exportMethod DAVIDGenes
#' @name DAVIDGenes
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{DAVIDGenes}{data.frame}(object)
#' @aliases DAVIDGenes,data.frame-method
setMethod(f="DAVIDGenes", signature=signature("data.frame"), 
  definition=function(object){
    as(object, "DAVIDGenes")
})
#'
#' @exportMethod initialize
#' @name initialize
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{initialize}{DAVIDGenes}(.Object, fileName)
#' @aliases initialize,DAVIDGenes-method
setMethod(f="initialize", signature=signature("DAVIDGenes"), 
  definition=function(.Object, fileName){
    ##Sanity Check
    if(missing(fileName)){
      .Object<-callNextMethod()
    }else{
      ##Import data to R
      datos<-read.csv(file=fileName, sep="\t")
      .Object<-as(datos, "DAVIDGenes")
    }
    validObject(.Object)
    return(.Object)
})
#' @importFrom methods as
#' @name as
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage as(object, Class, strict=TRUE, ext=possibleExtends(thisClass, Class))
#' @export
setAs(from="data.frame", to="DAVIDGenes", def=function(from){
  ##Default value
  to<-new("DAVIDGenes")
  ##Sanity Check
  if(nrow(from)>0){
    stopifnot(ncol(from)>=2)
    ##names and types
    names(from)[1:2]<-c("ID", "Name")
    from$ID<-as.character(from$ID)
    from$Name<-as.character(from$Name)
    if(ncol(from)==3){
      names(from)[3]<-"Species"
      from$Species<-as.factor(from$Species)
    }
  
    ##Check for ids in DAVID for the same gene, in order to get one id per row
    duplicateids<-which(regexpr(pattern=",", from$ID)>0)
    if(length(duplicateids)!=0){
      ##for each duplicate, generate the equivalent data.frame rows to add to
      ##from object.
      from<-rbind(from, do.call(rbind, lapply(duplicateids, function(index){
        ID<-unlist(strsplit(split=",", gsub(pattern=" ", replacement="",
          from$ID[index])))
        Name<-from$Name[index]
        out<-data.frame(ID=ID, Name=Name)
        if(ncol(from)==3){
          out$Species<-from$Species[index]
        }
        return(out)
      })))
      ##remove the original duplicateIds
      from<-from[-duplicateids, ]
    }

    ##The Gene List Report
    to@.Data<-as.list(from)
    to@names<-names(from)
    to@row.names<-row.names(from)

  }#end nrow(from)>0

  ##The output
  validObject(to)
  return(to)
})
#' @exportMethod DAVIDFunctionalAnnotationChart
#' @name DAVIDFunctionalAnnotationChart
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage DAVIDFunctionalAnnotationChart(object)
#' @aliases DAVIDFunctionalAnnotationChart-methods
#' @family DAVIDFunctionalAnnotationChart
setGeneric(name="DAVIDFunctionalAnnotationChart", def=function(object){
  standardGeneric("DAVIDFunctionalAnnotationChart")
})
#'
#' @exportMethod DAVIDFunctionalAnnotationChart
#' @name DAVIDFunctionalAnnotationChart
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{DAVIDFunctionalAnnotationChart}{character}(object)
#' @aliases DAVIDFunctionalAnnotationChart,character-method
setMethod(f="DAVIDFunctionalAnnotationChart", signature=signature("character"), 
  definition=function(object){
    new("DAVIDFunctionalAnnotationChart", fileName=object)
})
#'
#' @exportMethod DAVIDFunctionalAnnotationChart
#' @name DAVIDFunctionalAnnotationChart
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{DAVIDFunctionalAnnotationChart}{data.frame}(object)
#' @aliases DAVIDFunctionalAnnotationChart,data.frame-method
setMethod(f="DAVIDFunctionalAnnotationChart",
  signature=signature("data.frame"), definition=function(object){
    as(object, "DAVIDFunctionalAnnotationChart")
})
#'
#' @name initialize
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{initialize}{DAVIDFunctionalAnnotationChart}(.Object, fileName)
#' @aliases initialize,DAVIDFunctionalAnnotationChart-method
setMethod(f="initialize", signature=signature("DAVIDFunctionalAnnotationChart"),
  definition=function(.Object, fileName){
    ##Sanity Check
    if(missing(fileName)){
      .Object<-callNextMethod()
    }else{
      ##Import data to R
      datos<-read.csv(file=fileName, sep="\t")
      .Object<-as(datos, "DAVIDFunctionalAnnotationChart")
    }
    validObject(.Object)
    return(.Object)
})
#' @name as
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage as(object, Class, strict=TRUE, ext=possibleExtends(thisClass, Class))
setAs(from="data.frame", to="DAVIDFunctionalAnnotationChart",
  def=function(from){
    ##Default value
    to<-new("DAVIDFunctionalAnnotationChart")
    ##Sanity Check
    if(nrow(from)>0){
      stopifnot(ncol(from)==13)
      ##Force data type
      from$Category<-as.factor(from$Category)
      from$Term<-as.character(from$Term)
      from$Genes<-as.character(from$Genes)
      from$PValue<-as.numeric(as.character(from$PValue))
      
      ##The Gene List Report
      to@.Data<-as.list(from)
      to@names<-names(from)
      to@row.names<-row.names(from)
    }
    validObject(to)
    return(to)
})
#'
#' @name initialize
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{initialize}{DAVIDCluster}(.Object, fileName)
#' @aliases initialize,DAVIDCluster-method
setMethod(f="initialize",signature=signature(.Object="DAVIDCluster"),
  definition=function(.Object, fileName){
    ##Default value
    out<-list()
    
    ##Sanity Check
    if(!missing(fileName)){
      ##Parse the Cluster Report into R
      con<-NULL
      tryCatch({
        ##Open the file connection 
        con <- file(description=fileName, open="rt")

        ##For each cluster
        repeat{

          ##The header
          ans<-readLines(con, n=1)
          if(length(ans)==0){break}
          if(ans != ""){
            ##Parse the cluster header as a list
            fields<-as.character(gsub(sapply(unlist(strsplit(ans, split="\t")),
              sub, pattern=" ", replacement=""), pattern=":", replacement=""))
            cluster<-sapply(seq(along=fields), function(index){
              field<-unlist(strsplit(fields[index], split=" "))
              aux<-list(type.convert(field[2]))
              names(aux)<-field[1]
              return(aux)
            })

          ##Parse the members
          ##The members' header
          ans<-readLines(con, n=1)
          header<-unlist(strsplit(ans, split="\t"))
          header<-gsub(header, pattern=" ", replacement=".")
          header<-gsub(header, pattern="%", replacement="X.")
          members<-NULL
            
          ##The members
          repeat{
            ans<-readLines(con, n=1)
            if(ans==""){break}
            members<-rbind(members, unlist(strsplit(ans, split="\t")))
          }
          colnames(members) <- header
          members <- as.data.frame(members,stringsAsFactors = FALSE)
          for(index in 1:ncol(members)){
            members[, index]<-type.convert(members[, index])
          } 
        }#Members repeat

        ##Ensemble the cluster
        cluster<-c(cluster, list(Members=members))
        out <- c(out, list(cluster))
          }#Cluster repeat.Object$type <- names[out]

        },#Try statement

        ##Report the error or warning
        error=function(e) stop(e),
        warning=function(w) warning(w),

        ##Close the connection
        finally={
          if(!is.null(con)){
            tryCatch(close(con),
              ##Report the error or warning
              error=function(e) stop(e),
              warning=function(w) warning(w)
            )
          }
        }#finally
      )#Try Catch
    }#Sanity Check

    ##Move out list into .Object, if present
    if(length(out)>0){
      type<-names(out[[1]])[1]
      .Object@type<-type

      ##Sort cluster and remove type 
      number<-do.call(c, lapply(out, function(x){x[[type]]}))
      out<-out[order(number)]
      out<-lapply(out, function(x){x[-1]})

      .Object@cluster<-out
    }

    return(.Object)
})
#'
#' @name initialize
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{initialize}{DAVIDGeneCluster}(.Object, fileName)
#' @aliases initialize,DAVIDGeneCluster-method
setMethod(f="initialize", signature=signature("DAVIDGeneCluster"), 
  definition=function(.Object, fileName){
    if(missing(fileName)){
      .Object<-callNextMethod()
    }else{
      .Object<-callNextMethod(.Object, fileName)
      ##Convert cluster Members into DAVIDGenes
      .Object@cluster<-lapply(.Object@cluster, function(clust){
        clust$Members<-as(clust$Members, "DAVIDGenes")
        return(clust)
      })
    }
    return(.Object)
})
#' @exportMethod DAVIDGeneCluster
#' @name DAVIDGeneCluster
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage DAVIDGeneCluster(object)
#' @aliases DAVIDGeneCluster-methods
#' @family DAVIDGeneCluster
setGeneric(name="DAVIDGeneCluster", def=function(object){
  standardGeneric("DAVIDGeneCluster")
})
#'
#' @exportMethod DAVIDGeneCluster
#' @name DAVIDGeneCluster
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{DAVIDGeneCluster}{character}(object)
#' @aliases DAVIDGeneCluster,character-method
setMethod(f="DAVIDGeneCluster", signature=signature("character"), 
  definition=function(object){
    new("DAVIDGeneCluster", fileName=object)
})
#'
#' @name initialize
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{initialize}{DAVIDTermCluster}(.Object, fileName)
#' @aliases initialize,DAVIDTermCluster-method
setMethod(f="initialize", signature=signature("DAVIDTermCluster"), 
  definition=function(.Object, fileName){
    if(missing(fileName)){
      .Object<-callNextMethod()
    }else{
      .Object<-callNextMethod(.Object, fileName)
      ##Convert cluster Members into DAVIDFunctionalAnnotationChart
      .Object@cluster<-lapply(.Object@cluster,function(clust){
        clust$Members<-as(clust$Members, "DAVIDFunctionalAnnotationChart")
        return(clust)
      })
    }
    return(.Object)
})
#' @exportMethod DAVIDTermCluster
#' @name DAVIDTermCluster
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage DAVIDTermCluster(object)
#' @aliases DAVIDTermCluster-methods
#' @family DAVIDTermCluster
setGeneric(name="DAVIDTermCluster", def=function(object){
  standardGeneric("DAVIDTermCluster")
})
#'
#' @exportMethod DAVIDTermCluster
#' @name DAVIDTermCluster
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{DAVIDTermCluster}{character}(object)
#' @aliases DAVIDTermCluster,character-method
setMethod(f="DAVIDTermCluster", signature=signature("character"), 
  definition=function(object){
    new("DAVIDTermCluster", fileName=object)
})
#'
#' @name initialize
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{initialize}{DAVIDFunctionalAnnotationTable}(.Object, fileName)
#' @aliases initialize,DAVIDFunctionalAnnotationTable-method
setMethod(f="initialize", signature=signature("DAVIDFunctionalAnnotationTable"),
  definition=function(.Object, fileName){
    if(missing(fileName)){
      .Object<-callNextMethod()
      validObject(.Object)
    }else{
  
    ##Read the file and parse it
    datos<-read.csv(file=fileName, sep="\t", stringsAsFactors=FALSE)
    .Object<-as(datos, "DAVIDFunctionalAnnotationTable")
  }#exist fileName

  ##Finally check the object
  validObject(.Object)
  return(.Object)
})
#' @name as
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage as(object, Class, strict=TRUE, ext=possibleExtends(thisClass, Class))
setAs(from="data.frame", to="DAVIDFunctionalAnnotationTable",
  def=function(from){
    ##Default value
    to<-new("DAVIDFunctionalAnnotationTable")
    ##Sanity Check
    if(nrow(from)>0){
      ##Check for duplicateIds, and create new rows for each gene
      duplicateids<-which(regexpr(pattern=",", from$ID)>0)
      if(length(duplicateids)!=0){
        ##for each duplicate, generate the equivalent data.frame rows to add
        ##into from
        from<-rbind(from, do.call(rbind,
          ##for each duplicate row
          lapply(duplicateids, function(index){
            ID<-unlist(strsplit(split=",", gsub(pattern=" ", replacement="",
              from$ID[index])))
            Name<-from$Name[index]
            out<- do.call(rbind, lapply(ID,function(id){
              cbind(data.frame(ID=id), from[index, -1, drop =FALSE])}))
            return(out)
          })
        ))
        ##remove the original duplicateIds
        from<-from[-duplicateids, ]
      }
  
      ##Default values
      dictionary<-lapply(1:(ncol(from)-3), function(x){
        data.frame(ID=character(0), Term=character(0))
      })
      names(dictionary)<-names(from)[-(1:3)]
      membership<-lapply(dictionary, function(x){matrix(nrow=0, ncol=0)})

      ##Check for data to parse
      if(nrow(from)>0){
        ##For each main category
        for(category in names(dictionary)){
          ##For each gene, parse the given category
          complete<-lapply(from[, category], function(gene){
            ##Check for annotation
            if(gene!=""){
              ##find category separator ","
              if(category == "ENTREZ_GENE_SUMMARY"){
                cbind(gene, "")
              }else{
                ##Special category separators
                cuts<-switch(category, 
                  BIND=unlist(gregexpr(pattern=",[[:digit:]]", gene)),
                  HOMOLOGOUS_GENE={
                    numer<-unlist(gregexpr(pattern=",[[:digit:]]", gene))
                    punt<-unlist(gregexpr(pattern=":", gene))
                    if(length(punt[-1])>0){
                      unlist(sapply(punt[-1], function(x){
                        aux<-which(numer<x)
                        if(length(aux)>0){
                          numer[max(aux)]
                        }else{integer(0)}
                      }))
                    }else{-1}
                  },
                  COG_NAME=unlist(gregexpr(pattern=",COG[[:digit:]]", gene)),
                  PIR_SUPERFAMILY=unlist(gregexpr(pattern=",PIRSF[[:digit:]]", 
                    gene))
                )#cuts
            
                ##GOTERM_MF comma compounds
                if(sum(regexpr(pattern="GOTERM_MF", category)>0)>0){
                  cuts<-unlist(gregexpr(pattern=",GO:[[:digit:]]", gene))
                }

                ##The ordinary separators
                if(is.null(cuts)){
                  cuts<-unlist(gregexpr(pattern=",\\S", gene))
                }
                cuts<-data.frame(start=c(1,cuts+1), stop=c(cuts-1,
                  nchar(gene)-1))
                members<-apply(cuts, 1, function(x){
                  substr(gene, start=x[1], stop=x[2])
                })
                members<-members[nchar(members)>0]
            
                ##Unify the category id/term separator to "\t"
                ##For GOTERM or PANTHER_FAMILY "~" to "\t"
                if(sum(regexpr(pattern="GOTERM",category)>0)>0 | 
                  category == "PANTHER_FAMILY"){
                  members<-sub(pattern="~", replacement="\t", members,
                    fixed=TRUE)
                }
                ##For BBID "." to "\t"
                if(category %in% "BBID"){
                  members<-sub(pattern=".", replacement="\t", members,
                  fixed=TRUE)
                }
                ##For ":" to "\t"
                if(category %in% c("BIOCARTA", "INTERPRO", "KEGG_PATHWAY",
                  "PIR_SUPERFAMILY", "SMART", "BIND", "BLOCKS",
                  "CGAP_EST_QUARTILE", "CGAP_SAGE_QUARTILE",
                  "PANTHER_SUBFAMILY", "COG_NAME", "DIP", "GENERIF_SUMMARY",
                  "HOMOLOGOUS_GENE", "MINT", "PROFILE", "PROSITE",
                  "NCICB_CAPATHWAY_INTERACTION", "PANTHER_PATHWAY", "PFAM",
                  "PRINTS", "PRODOM", "REACTOME_INTERACTION",
                  "REACTOME_PATHWAY", "SP_COMMENT", "SSF",
                  "TIGRFAMS", "HIV_INTERACTION", "HIV_INTERACTION_CATEGORY")){
                  members<-sub(pattern=":", replacement="\t", members,
                  fixed=TRUE)
                }
                ##For SCOP_xx ":" to "\t"
                if(sum(regexpr(pattern="SCOP_", category)>0)>0){
                  members<-sub(pattern=":", replacement="\t", members,
                  fixed=TRUE)
                } 

                ##Check for "\t" presence 
                if(all(regexpr(pattern="\t", members)>0)){
                  do.call(rbind, strsplit(members, split="\t"))
                }else{
                  ##no id token       
                  cbind(members, "")
                }
        
              }#category == ENTREZ_GENE_SUMMARY
            }else{
              ##Empty annotation
              matrix(nrow=0, ncol=2)
            }#gene != ""
          })
  
          ##Main category dictionary
          uniqueCategory<-unique(do.call(rbind, complete))
          colnames(uniqueCategory)<-c("ID", "Term")
          uniqueCategory<-as.data.frame(uniqueCategory, stringsAsFactors=FALSE)
          dictionary[[category]]<-uniqueCategory
    
          ##Map each gene in the dictionary
          out<-do.call(rbind, lapply(complete, function(gene){
            uniqueCategory$ID%in% gene[,1]
          }))
          colnames(out)<-uniqueCategory$ID

          membership[[category]]<-out

        }#end for each main category

      }#end check for data to parse

      ##Generate DAVIDFunctionalAnnotationTable
      to@Genes<-as(from[, 1:3], "DAVIDGenes")
      to@Dictionary<-dictionary
      to@Membership<-membership
    }
    validObject(to)
    return(to)
})
#' @exportMethod DAVIDFunctionalAnnotationTable
#' @name DAVIDFunctionalAnnotationTable
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage DAVIDFunctionalAnnotationTable(object)
#' @aliases DAVIDFunctionalAnnotationTable-methods
#' @family DAVIDFunctionalAnnotationTable
setGeneric(name="DAVIDFunctionalAnnotationTable", def=function(object){
  standardGeneric("DAVIDFunctionalAnnotationTable")
})
#'
#' @exportMethod DAVIDFunctionalAnnotationTable
#' @name DAVIDFunctionalAnnotationTable
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{DAVIDFunctionalAnnotationTable}{character}(object)
#' @aliases DAVIDFunctionalAnnotationTable,character-method
setMethod(f="DAVIDFunctionalAnnotationTable", signature=signature("character"), 
  definition=function(object){
    new("DAVIDFunctionalAnnotationTable", fileName=object)
})
#'
#' @exportMethod DAVIDFunctionalAnnotationTable
#' @name DAVIDFunctionalAnnotationTable
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{DAVIDFunctionalAnnotationTable}{data.frame}(object)
#' @aliases DAVIDFunctionalAnnotationTable,data.frame-method
setMethod(f="DAVIDFunctionalAnnotationTable",
  signature=signature("data.frame"), definition=function(object){
    as(object, "DAVIDFunctionalAnnotationTable")
})
#'
#' @name initialize
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @importClassesFrom graph graphNEL
#' @usage \S4method{initialize}{DAVIDGODag}(.Object,funChart,type=c("BP","MF","CC"),pvalueCutoff=0.1,removeUnattached=FALSE,...)
#' @aliases initialize,DAVIDGODag-method
setMethod(f="initialize", signature=signature("DAVIDGODag"), 
  definition=function(.Object, funChart, type=c("BP", "MF", "CC"),
  pvalueCutoff=0.1, removeUnattached=FALSE, ...){
    ##Sanity Check
    stopifnot(type[1] %in% c("BP", "MF", "CC"))

    ##Empty output
    .Object<-callNextMethod()
    .Object@pvalueCutoff<-pvalueCutoff
    .Object@testName[2]<-type[1]
    .Object@goDag<-new("graphNEL", edgemode="directed")
    nodeDataDefaults(.Object@goDag, "pvalue")<-1
    nodeDataDefaults(.Object@goDag, "geneIds")<-character(0)
    nodeDataDefaults(.Object@goDag, "condGeneIds")<-character(0)
    nodeDataDefaults(.Object@goDag, "oddsRatio")<-numeric(0)
    nodeDataDefaults(.Object@goDag, "expCount")<-numeric(0)
    ##new ones
    nodeDataDefaults(.Object@goDag, "percentage")<-NA_real_
    nodeDataDefaults(.Object@goDag, "count")<-NA_integer_
    nodeDataDefaults(.Object@goDag, "term")<-NA_character_ 
    nodeDataDefaults(.Object@goDag, "listTotal")<-NA_integer_
    nodeDataDefaults(.Object@goDag, "popHit")<-NA_integer_
    nodeDataDefaults(.Object@goDag, "popTotal")<-NA_integer_
    nodeDataDefaults(.Object@goDag, "foldEnrichment")<-NA_real_
    nodeDataDefaults(.Object@goDag, "bonferroni")<-NA_real_
    nodeDataDefaults(.Object@goDag, "benjamini")<-NA_real_
    nodeDataDefaults(.Object@goDag, "fdr")<-NA_real_

    ##Check DAVIDFunctionalAnnotationChart presence
    if(!missing(funChart)){
      stopifnot(class(funChart)=="DAVIDFunctionalAnnotationChart")
      ##Check if the GO category exists in the results
    if(any(regexpr(pattern=paste("GOTERM_", type[1], sep=""),
      levels(funChart$Category))>0)){
      funChart<-funChart[regexpr(pattern=paste("GOTERM_", type[1],sep=""),
        funChart$Category)>0, ] 
      funChart<-cbind(funChart, do.call(rbind, strsplit(funChart$Term,
        split="~")))
      names(funChart)[length(funChart)+(-1:0)]<-c("ID", "Name")
      found<-funChart$PValue < pvalueCutoff 

      ##Check whether to build the DAG or not
      if(any(found)){
        ##Get the GO DAG
        goGraph<-GOGraph(as.character(funChart$ID[found]),
         getFromNamespace(x="GOenv", ns="GOstats")(paste(type[1], "PARENTS",
         sep="")))
        goGraph<-upsideDown(goGraph)

        ##Check for unattached nodes, due to database inconsistency
        possibleRoots<-lapply(inEdges(goGraph), length)==0
        possibleRoots<-possibleRoots[possibleRoots]
        if(length(possibleRoots)>0){
          possibleRoots<-possibleRoots[names(possibleRoots)!="all"]
          if(length(possibleRoots)>0){
            if(removeUnattached){
              goGraph<-removeNode(names(possibleRoots), goGraph)
            }else{
              warning("DAVID/GO.db database not using the same version.", 
              "Keeping unattached nodes: ", toString(names(possibleRoots)))
            }
          }
        }
        ##Remove all node, if present
        if(any(nodes(goGraph)%in%"all")){goGraph<-removeNode("all", goGraph)}
         
        ##Copy GODag attributes
        ##Default values
        goGraph@nodeData@defaults<-.Object@goDag@nodeData@defaults

        ##Using all available data in funChart to fill the goDag nodeData
        invisible(sapply(nodes(goGraph), function(node){
          ##Check if available data
          index<-which(funChart$ID %in% node)
          if(length(index)>0){
            nodeData(goGraph, n=node, attr="pvalue")<<-funChart$PValue[index]
            nodeData(goGraph, n=node,
              attr="geneIds")<<-strsplit(funChart$Genes[index], split=", ")
            nodeData(goGraph, n=node, attr="percentage") <<- funChart$X.[index]
            nodeData(goGraph, n=node, attr="count")<<-funChart$Count[index]
            nodeData(goGraph, n=node, 
              attr="term")<<-as.character(funChart$Name[index])
            nodeData(goGraph, n=node, 
              attr="listTotal")<<-funChart$List.Total[index]
            nodeData(goGraph, n=node, 
              attr="popHit")<<-funChart$Pop.Hits[index]
            nodeData(goGraph, n=node,
              attr="popTotal")<<-funChart$Pop.Total[index]
            nodeData(goGraph, n=node, 
              attr="foldEnrichment")<<-funChart$Fold.Enrichment[index]
            nodeData(goGraph, n=node,
              attr="bonferroni")<<-funChart$Bonferroni[index]
            nodeData(goGraph, n=node,
              attr="benjamini")<<-funChart$Benjamini[index]
            nodeData(goGraph, n=node,
              attr="fdr")<<-funChart$FDR[index]
          }else{
            ##Use GO.db data
            nodeData(goGraph, n=node, attr="term")<<-Term(GOTERM[[node]])
          }
          return(NULL)
        }))
        ##geneIds in the root node
        roots<-c("GO:0008150", "GO:0003674", "GO:0005575")
        if(any(roots %in% nodes(goGraph))){
          root<-roots[which(roots %in% nodes(goGraph))]
          nodeData(goGraph, n=root, attr="geneIds")<-
            list(unique(unlist(nodeData(goGraph, attr="geneIds"))))
          nodeData(goGraph, n=root, attr="listTotal")<-
            na.omit(unique(unlist(nodeData(goGraph, attr="listTotal"))))
          nodeData(goGraph, n=root, attr="popTotal")<-
            na.omit(unique(unlist(nodeData(goGraph, attr="popTotal"))))
          nodeData(goGraph, n=root, attr="popHit")<-
            na.omit(unique(unlist(nodeData(goGraph, attr="popTotal"))))
        }

        ##New data into .Object
        .Object@goDag<-goGraph
        .Object@pvalue.order<-order(unlist(nodeData(goGraph, attr="pvalue")))
        .Object@geneIds<-unique(unlist(nodeData(goGraph, attr="geneIds")))
      }#have to build
    }#category exists
  }#!missing(funChart)
  return(.Object)
})
#' @exportMethod DAVIDGODag
#' @name DAVIDGODag
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage DAVIDGODag(funChart, ...)
#' @aliases DAVIDGODag-methods
#' @family DAVIDGODag
setGeneric(name="DAVIDGODag", def=function(funChart, ...){
  standardGeneric("DAVIDGODag")
})
#'
#' @exportMethod DAVIDGODag
#' @name DAVIDGODag
#' @rdname DAVIDClasses-constructor
#' @inheritParams DAVIDGenes
#' @usage \S4method{DAVIDGODag}{DAVIDFunctionalAnnotationChart}(funChart, ...)
#' @aliases DAVIDGODag,DAVIDFunctionalAnnotationChart-method
setMethod(f="DAVIDGODag",signature=signature("DAVIDFunctionalAnnotationChart"), 
  definition=function(funChart, ...){
    new("DAVIDGODag", funChart, ...)
})
