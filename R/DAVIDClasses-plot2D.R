#' Visualization of biological relationships
#'
#' \code{plot2D} uses a 2D tile ggplot to explore biological relationships 
#' between two variables such as annotation category and genes, for Functional
#' Annotation Chart/Table or Term cluster results. For Gene cluster, the cluster
#' number vs genes membership is plotted.
#' 
#' @param object DAVIDResult heirs (DAVIDFunctionalAnnotationChart/Table or 
#' DAVIDGeneCluster/TermCluster)
#' @param dataFrame data.frame with three columns (x, y and fill) to be used 
#' in ggplot. X(Y) is a character/factor with the X(Y)-axis labels and "fill" is
#' a the color to be used for x-y labels.
#' @param color named character vector to indicate tile color. Default value
#' is c("FALSE"="black", "TRUE"="green").
#' @param names should gene names be plotted? Default value is FALSE, i.e, use
#' ids.
#' @param number integer to indicate which cluster to plot. Default value is 1.
#' @param category character vector to select the main annotation categories. By
#' default is missing in order to use all the available ones.
#' @param id character vector to indicate which gene ids to use. By default is
#' missing in order to use all the available ones.
#' @param names.genes,names.category Should genes and/or category names
#' used? Default value is FALSE, i.e., use both ids.
#' @param ... Additional parameters for heirs functions.
#'
#' @return a ggplot object if the object is not empty.
#'
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType methods
#' @exportMethod plot2D
#' @name plot2D
#' @rdname DAVIDClasses-plot2D
#' @usage plot2D(object,...)
#' @aliases plot2D-methods
#' @examples
#' {
#' ##DAVIDFunctionalAnnotationChart example:
#' ##Load the Functional Annotation Chart file report for the input demo 
#' ##file 2, using data function. Just to keep it simple, for the first five
#' ##terms present in funChart2 object, create a DAVIDFunctionalAnnotationChart
#' ##object and plot a 2D tile matrix with the reported evidence (green) or not
#' ##(black).
#' data(funChart2)
#'   plot2D(DAVIDFunctionalAnnotationChart(funChart2[1:5, ]),
#'     color=c("FALSE"="black", "TRUE"="green"))
#'
#' ##DAVIDFunctionalAnnotationTable example
#' ##Load the Functional Annotation Table file report for the input demo 
#' ##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
#' ##object using the loaded data.frame annotationTable1. 
#' data(annotationTable1)
#' davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)
#'  
#' ##Plot the membership of only for the first six terms in this
#' ##category, with only the genes of the first six terms with at least one
#' ##evidence code.
#' ##Category filtering...
#' categorySelection<-list(head(dictionary(davidFunTable1,
#' categories(davidFunTable1)[1])$ID))
#' names(categorySelection)<-categories(davidFunTable1)[1]
#'  
#' ##Gene filter...
#' id<-membership(davidFunTable1, categories(davidFunTable1)[1])[,1:6]
#' id<-ids(genes(davidFunTable1))[rowSums(id)>0]
#'  
#' ##Finally the membership tile plot
#'    plot2D(davidFunTable1, category=categorySelection, id=id,
#'      names.category=TRUE)
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
#' ##We can inspect a 2D tile membership plot, to visual inspect for
#' ##overlapping of genes across the clusters. Or use an scaled version of gene
#' ##names to see the association of gene cluster, e.g., cluster 3 is related to
#' ##ATP genes.
#'  plot2D(davidGeneCluster1)
#'  plot2D(davidGeneCluster1,names=TRUE)+
#'    theme(axis.text.y=element_text(size=rel(0.9)))
#'
#' ##DAVIDTermCluster example:
#' ##Load the Gene Functional Classification Tool file report for the
#' ##input demo file 2 to create a DAVIDGeneCluster object.
#' setwd(tempdir())
#' fileName<-system.file("files/termClusterReport2.tab.tar.gz",
#'   package="RDAVIDWebService")
#' untar(fileName)
#' davidTermCluster2<-DAVIDTermCluster(untar(fileName, list=TRUE))
#'
#' ##Finally, we can inspect a 2D tile membership plot, to visual inspect for
#' ##overlapping of genes across the term members of the selected cluster,
#' ##e.g., the first cluster . 
#'    plot2D(davidTermCluster2, number=1)
#' }
#'
setGeneric(name="plot2D", def=function(object, ...){standardGeneric("plot2D")})
#'
#' @exportMethod plot2D
#' @name plot2D
#' @inheritParams plot2D
#' @rdname DAVIDClasses-plot2D
#' @usage \S4method{plot2D}{DAVIDResult}(object, dataFrame)
#' @aliases plot2D,DAVIDResult-method
#' @family DAVIDResult
setMethod(f="plot2D", signature=signature("DAVIDResult"), 
  definition=function(object, dataFrame){
  ##Sanity check 
  ans <- ggplot()
  if(!missing(dataFrame)){
    if(nrow(dataFrame)>0){
      ##Add line breaks
      dataFrame$y<-gsub(pattern="~", replacement="\n", dataFrame$y)
      dataFrame$y<-sapply(dataFrame$y, function(name){
        spaces<-unlist(gregexpr(" ", name))
        if(length(spaces)>6){
          invisible(sapply(spaces[seq(from=4, to=length(spaces), by=4)],
          function(position){
            substr(name, start=position, stop=position)<<-"\n"
            invisible(NULL)
          }))
        }
        return(name)
      })
      ##the plot
      ans<-ggplot(dataFrame, aes(y=y, x=x, fill=fill)) 
      ans<-ans+geom_tile() 
      ans<-ans+scale_fill_identity(guide="legend", labels=c(FALSE,TRUE))
    }#nrow(dataFrame)>0
  }#!missing(dataFrame)
  return(ans)
})
#'
#' @exportMethod plot2D
#' @name plot2D
#' @inheritParams plot2D
#' @rdname DAVIDClasses-plot2D
#' @usage \S4method{plot2D}{DAVIDFunctionalAnnotationChart}(object,color=c("FALSE"="black", "TRUE"="green"))
#' @aliases plot2D,DAVIDFunctionalAnnotationChart-method
#' @family DAVIDFunctionalAnnotationChart
setMethod(f="plot2D",signature=signature("DAVIDFunctionalAnnotationChart"),
  definition=function(object, color=c("FALSE"="black", "TRUE"="green")){
    ##Generate data.frame object for callNextMethod (y:term, x:gene, fill:color)
    membership <- data.frame(x=character(0),y=character(0),fill=character(0))

    ##Generate a dictionary with ids, for the membership data.frame
    if(nrow(object)>0){
      ##The unique ids for plotting
      uniqueids<-unique(unlist(ids(object)))
      ##For each term get whether the ids belong or not
      membership<-do.call(rbind,
        lapply(1:nrow(object), function(index){
          data.frame(x=uniqueids,
            y=gsub(pattern="~", replacement="\n", object$Term[index]),
            fill=uniqueids%in%unlist(strsplit(gsub(pattern=" ", replacement="",
              object$Genes[index]), split=",")))
        })
      )
      ##Apply the color schema
      membership$fill<-color[membership$fill+1]

      ##Standard plot2D
      gplot<-callNextMethod(object,membership)
      gplot<-gplot+ylab("Terms")+xlab("Genes")+labs(fill="Evidence")+
        theme(axis.text.x=element_text(angle=90))
      return(gplot)
    }
})
#'
#' @exportMethod plot2D
#' @name plot2D
#' @inheritParams plot2D
#' @rdname DAVIDClasses-plot2D
#' @usage \S4method{plot2D}{DAVIDGeneCluster}(object,color=c("FALSE"="black","TRUE"="green"),names=FALSE)
#' @aliases plot2D,DAVIDGeneCluster-method
#' @family DAVIDGeneCluster
setMethod(f="plot2D", signature=signature("DAVIDGeneCluster"), 
  definition=function(object, color=c("FALSE"="black", "TRUE"="green"),
  names=FALSE){  
    ##Generate data.frame object for callNextMethod (x:cluster, y:gene,
    ##fill:color)
    membership<-data.frame(x=character(0), y=character(0), fill=character(0))

    ##Generate a dictionary with ids, for the membership data.frame
    if(length(object@cluster)>0){
      inputIds<-unique(unlist(ids(object)))
      membership<-do.call(rbind, lapply(ids(object),
        function(clust){data.frame(fill=inputIds %in%  clust,y=inputIds)}))
      membership$x<-c(sapply(1:length(ids(object)), rep,
        length=length(inputIds)))
      ##Use the names instead of the ids
      if(names){
        ##Get the gene name
        membership$y<-as.character(unlist(apply(membership, 1, function(gene){
          genes(object@cluster[[as.numeric(gene["x"])]]$Members,
            ids=gene["y"])$Name
        })))
      }
      ##Change logical to color
      membership$fill<-color[membership$fill+1]
        
      ##Finally the plot
      gplot<-callNextMethod(object, membership)
      gplot<-gplot+xlab("Cluster")+ylab("Genes")+labs(fill="Member")
      gplot
    }
})
#'
#' @exportMethod plot2D
#' @name plot2D
#' @inheritParams plot2D
#' @rdname DAVIDClasses-plot2D
#' @usage \S4method{plot2D}{DAVIDTermCluster}(object,number=1,color=c("FALSE"="black","TRUE"= "green"))
#' @aliases plot2D,DAVIDTermCluster-method
#' @family DAVIDTermCluster
setMethod(f="plot2D", signature=signature("DAVIDTermCluster"), 
  definition=function(object, number=1, color=c("FALSE"="black",
  "TRUE"="green")){
    ##Sanity Check
    if(length(object@cluster)>0){
      if(number %in% 1:length(object@cluster)){
        return(plot2D(members(object)[[number]], color))
      }
    }
})
#'
#' @exportMethod plot2D
#' @name plot2D
#' @inheritParams plot2D
#' @rdname DAVIDClasses-plot2D
#' @usage \S4method{plot2D}{DAVIDFunctionalAnnotationTable}(object, category, id, names.genes=FALSE, names.category=FALSE,color=c("FALSE"="black","TRUE"="green"))
#' @aliases plot2D,DAVIDFunctionalAnnotationTable-method
#' @family DAVIDFunctionalAnnotationTable
setMethod(f="plot2D", signature=signature("DAVIDFunctionalAnnotationTable"),
  definition=function(object, category, id, names.genes=FALSE,
  names.category=FALSE, color=c("FALSE"="black","TRUE"="green")){
    ##Sanity check
    stopifnot(!missing(category))
    if(!missing(id)){
      stopifnot(nrow(genes(object,id))!=0)
    }else{
      id<-ids(genes(object))
    }
  
    ##Get for the selected categories the membership and dictionary, to build up
    ##the dataFrame
    membershipMatrix<-do.call(cbind, membership(object, category, drop=FALSE))
    dictionaryDataFrame<-do.call(rbind, dictionary(object, category,drop=FALSE))
    dataFrame<-do.call(rbind, lapply(1:ncol(membershipMatrix), function(index){
      ##Check for ids
      found<-ids(genes(object)) %in% id
      data.frame(x=genes(object)[found, c("ID","Name")[names.genes+1]],
        y=dictionaryDataFrame[index, c("ID","Term")[names.category+1]], 
        fill=membershipMatrix[found, index])
    }))

    ##Change logical to color
    dataFrame$fill<-color[dataFrame$fill+1]
    
    ##Finally the plot
    gplot<-callNextMethod(object, dataFrame)
    gplot<-gplot+xlab("Genes")+ylab("Terms")+labs(fill="Evidence")
    gplot<-gplot+theme(axis.text.x=element_text(angle=90, hjust=1))
    gplot

})
