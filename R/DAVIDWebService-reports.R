#' Obtain DAVID website reports
#' 
#' \code{DAVIDWebService} class methods to obtain DAVID website reports from
#' R. This includes the different functionalities starting from the basic
#' "Show Gene List" or "Annotation Summary", to Set Enrichment Analysis
#' using "Functional Annotation Chart" or Modular Enrichment Analysis using
#' "Functional Annotation Clustering" or "Gene Functional Classification Tool".
#' Note that \code{DAVIDWebService} is a Reference class, hence invoke it
#' using object_name$method_name(parameters). In  addition, the user can use
#' the S4 version style function call (see Details).
#'
#' Available functions include:
#' \describe{
#'  \item{\code{getGeneCategoriesReport}:}{Get the gene categories report.}
#'  \item{\code{getAnnotationSummary}:}{Generate the summary of all available
#'  annotation in DAVID in terms of percentage of gene list ids present in the
#'  category and numbers of terms where the can be found.}
#'  \item{\code{getGeneListReportFile}:}{Generate the Gene List Report a.k.a
#'  Show Gene List in DAVID website and save it into a file.}
#'  \item{\code{getGeneListReport}:}{Generate Gene List Report a.k.a Show Gene
#'  List in DAVID website and import it as a DAVIDGenes object into R.}
#'  \item{\code{getFunctionalAnnotationChartFile}:}{Generate the Functional
#'  Annotation Chart Report for the selected functional categories, for the
#'  given EASE threshold and number of genes and save it to a file.}
#'  \item{\code{getFunctionalAnnotationChart}:}{Generate the Functional
#'  Annotation Chart Report for the selected functional categories, for the
#'  given EASE threshold and number of genes, and import it as a
#'  DAVIDFunctionalAnnotationChart object in R.}
#'  \item{\code{getClusterReportFile}:}{Generate the Term/Gene Cluster Report
#'  for the given configuration.}
#'  \item{\code{getClusterReport}:}{Generate the Term/Gene Cluster Report for
#'  the given configuration, and import it as a DAVIDGeneCluster or
#'  DAVIDTermCluster object, according to function call.}
#'  \item{\code{getFunctionalAnnotationTableFile}:}{Generate Functional
#'  Annotation Table Report File, which is a gene-centric view of the genes and
#'  their associated annotation terms (selected only). There is no statistics
#'  applied in this report.}
#'  \item{\code{getFunctionalAnnotationTable}:}{Generate Functional Annotation
#'  Table Report and import it as a DAVIDFunctionalAnnotationTable object in R.}
#' }
#'
#' @param object DAVIDWebService class object.
#' @param fileName character with the name of the file to store the Report.
#' @param threshold numeric with the EASE score (at most equal) that must be
#' present in the category to be included in the report. Default value is 0.1.
#' @param count integer with the number of genes (greater equal) that must 
#' be present in the category to be included in the report. Default value is 2.
#' @param type character with the type of cluster to obtain Term/Genes. Default
#' value "Term".
#' @param overlap integer with the minimum number of annotation terms 
#' overlapped between two genes in order to be qualified for kappa
#' calculation. This parameter is to maintain necessary statistical power
#' to make kappa value more meaningful. The higher value, the more
#' meaningful the result is. Default value is 4L.
#' @param initialSeed,finalSeed integer with the number of genes in the
#' initial (seeding) and final (filtering) cluster criteria. Default value
#' is 4L for both.
#' @param linkage numeric with the percentage of genes that two clusters 
#' share in order to become one.
#' @param kappa integer (kappa * 100), with the minimum kappa value to
#' be considered biological significant. The higher setting, the more genes
#' will be put into unclustered group, which lead to higher quality of
#' functional classification result with a fewer groups and a fewer gene
#' members. Kappa value 0.3 starts giving meaningful biology based on our
#' genome-wide distribution study. Anything below 0.3 have great chance to 
#' be noise.
#' @param ... additional parameters for getXXFile functions.
#'
#' @return according to the call one of the following objects can be returned
#'  \item{getGeneCategoriesReport}{integer vector with the IDs of the
#'  categories.}
#'  \item{getAnnotationSummary}{data.frame with the annotation summary report
#'  with the following columns:
#'  \enumerate{
#'    \item \bold{Main.Category}: factor with the main categories under used
#'    in the present analysis.
#'    \item \bold{ID}: integer to identify the annotation category.
#'    \item \bold{Name}: character with the name of category (the available
#'    ones in getAllAnnotationCategoryNames function).
#'    \item \bold{X.}: numeric with the percentage of the gene list ids
#'    present in the term.
#'    \item \bold{Count}: integer with the number of ids of the gene list that
#'    belong to this term.
#'   }
#'  }
#'  \item{getGeneListReportFile}{data.frame with the Gene List Report with the
#'  following columns:
#'  \enumerate{
#'    \item \bold{ID}: character with the Gene List ID present in DAVID
#'    knowledge base, in the submitted type. If more than one ids map to the
#'    same DAVID ID, the record is a comma separated character.
#'    \item \bold{Name}: character with the name of the gene as seen in DAVID
#'    knowledge base, in a comma separated fashion (if more than one ID maps
#'    to the same DAVID ID).
#'    \item \bold{Species}: factor with the name of the Specie.
#'    }
#'  }
#'  \item{getGeneListReport}{Generate Gene List Report a.k.a Show Gene
#'  List in DAVID website and import it as a DAVIDGenes object in R.}
#'  \item{getFunctionalAnnotationChartFile}{file with the following columns:
#'  \enumerate{
#'   \item \bold{Category}: factor with the main categories under used in the
#'   present analysis.
#'   \item \bold{Term}: character with the name of the term in format id~name
#'   (if available).
#'   \item \bold{Count}: integer with the number of ids of the gene list that
#'   belong to this term.
#'   \item \bold{X.}: after converting user input gene IDs to  corresponding
#'   DAVID gene ID, it refers to the percentage of DAVID genes in the list
#'   associated with a particular annotation term. Since DAVID gene ID is
#'   unique per gene, it is more accurate to use DAVID ID percentage to
#'   present the gene-annotation association by removing any redundancy in user
#'   gene list, i.e. two user IDs represent same gene.
#'   \item \bold{PValue}: numeric with the EASE Score of the term (see DAVID
#'   Help page).
#'   \item \bold{Genes}: character in comma separated style with the genes
#'   present in the term.
#'   \item \bold{List.Total, Pop.Hits, Pop.Total}: integers (in addition to
#'   Count) to build the 2x2 contingency table in order to compute the EASE
#'   Score (see DAVID Help page).
#'   \item \bold{Fold.Enrichment}: numeric with the ratio of the two
#'   proportions. For example, if 40/400 (i.e. 10\%) of your input genes
#'   involved in "kinase activity" and the background information is 300/30000
#'   genes (i.e. 1\%) associating with "kinase activity", roughly 10\% / 1\% =
#'   10 fold enrichment.
#'   \item \bold{Bonferroni, Benjamini, FDR}: numerics with p-value adjust
#'   different criteria (see p.adjust).
#'    }
#'  } 
#'  \item{getFunctionalAnnotationChart}{Generate the Functional
#'  Annotation Chart Report for the selected functional categories, for the
#'  given EASE threshold and number of genes, and import it as a
#'  DAVIDFunctionalAnnotationChart object in R.}
#'  \item{getClusterReportFile}{file with the following columns:
#'  \enumerate{
#'   \item \bold{Annotation/Gene Cluster}: integer with the number of cluster.
#'   \item \bold{EnrichmentScore}: numeric with the geometric mean (in -log
#'   scale) of members p-values in a corresponding annotation cluster, is used
#'   to rank their biological significance. Thus, the top ranked annotation
#'   groups most likely have consistent lower p-values for their annotation
#'   members.
#'   \item \bold{Members}: according to the type of cluster, changes the
#'   associated data to include Gene List or Functional Chart Report (see
#'   getGeneListReport and getFunctionalAnnotationChart).
#'    }
#'   }
#'  \item{getClusterReport}{Generate the Term/Gene Cluster Report for
#'  the given configuration, and import it as a DAVIDGeneCluster or
#'  DAVIDTermCluster according to function call.}
#'  \item{getFunctionalAnnotationTableFile}{file with the following columns:
#'  \enumerate{
#'   \item \bold{Gene}: Three Columns with the same data included in Gene List
#'   Report (ID, Gene.Name and Species) but coding for DAVID ID, i. e., comma
#'   separated character with input ids if two or more stands for the same
#'   gene.
#'   \item \bold{Annotation}: as many columns as Annotation Categories were in
#'   used. In each column, a comma separated style is use to delimitate the
#'   different terms where is reported evidence for DAVID ID record.
#'    }
#'   }
#'  \item{\code{getFunctionalAnnotationTable}:}{Generate Functional Annotation
#'  Table Report, which is a gene-centric view of the genes and their associated
#'  annotation terms (selected only), and import it as a
#'  DAVIDFunctionalAnnotationTable object in R.}
#'
#' @references
#' \enumerate{
#' \item \url{http://david.abcc.ncifcrf.gov/helps/functional_annotation.html#E3}
#' \item \url{http://david.abcc.ncifcrf.gov/helps/functional_classification.html#clustering}
#' \item Cohen, J: A coefficient of agreement for nominal scales,
#'  Educational and Psychological Measurement, 1960, 20, 37-46.
#' }
#'
#' @seealso \code{\link{p.adjust}} and \code{\link{fisher.test}}
#'
#' @docType methods
#' @exportMethod getGeneCategoriesReport
#' @name getGeneCategoriesReport
#' @rdname DAVIDWebService-reports
#' @usage getGeneCategoriesReport(object)
#' @aliases getGeneCategoriesReport
#' @family DAVIDWebService
setGeneric(name="getGeneCategoriesReport", def=function(object){
  standardGeneric("getGeneCategoriesReport")
})
#'
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage \S4method{getGeneCategoriesReport}{DAVIDWebService}(object)
#' @aliases getGeneCategoriesReport,DAVIDWebService-method
setMethod(f="getGeneCategoriesReport",signature=signature("DAVIDWebService"), 
  definition=function(object){
    object$getGeneCategoriesReport()
})
#' @exportMethod getAnnotationSummary
#' @name getAnnotationSummary
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage getAnnotationSummary(object)
#' @aliases getAnnotationSummary
setGeneric(name="getAnnotationSummary", def=function(object){
  standardGeneric("getAnnotationSummary")
})
#'
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage \S4method{getAnnotationSummary}{DAVIDWebService}(object)
#' @aliases getAnnotationSummary,DAVIDWebService-method
setMethod(f="getAnnotationSummary",signature=signature("DAVIDWebService"), 
  definition=function(object){
    object$getAnnotationSummary()
})
#' @exportMethod getGeneListReportFile
#' @name getGeneListReportFile
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage getGeneListReportFile(object, fileName)
#' @aliases getGeneListReportFile
setGeneric(name="getGeneListReportFile", def=function(object, fileName){
  standardGeneric("getGeneListReportFile")
})
#'
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage \S4method{getGeneListReportFile}{DAVIDWebService}(object, fileName)
#' @aliases getGeneListReportFile,DAVIDWebService-method
setMethod(f="getGeneListReportFile",signature=signature("DAVIDWebService"), 
  definition=function(object, fileName){
    object$getGeneListReportFile(fileName)
})
#' @exportMethod getGeneListReport
#' @name getGeneListReport
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage getGeneListReport(object)
#' @aliases getGeneListReport
setGeneric(name="getGeneListReport", def=function(object){
  standardGeneric("getGeneListReport")
})
#'
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneListReport
#' @usage \S4method{getGeneListReport}{DAVIDWebService}(object)
#' @aliases getGeneListReport,DAVIDWebService-method
setMethod(f="getGeneListReport",signature=signature("DAVIDWebService"), 
  definition=function(object){
    object$getGeneListReport()
})
#' @exportMethod getFunctionalAnnotationChartFile
#' @name getFunctionalAnnotationChartFile
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage getFunctionalAnnotationChartFile(object, fileName, threshold=0.1, count=2L)
#' @aliases getFunctionalAnnotationChartFile
setGeneric(name="getFunctionalAnnotationChartFile", def=function(object,
  fileName, threshold=0.1, count=2L){
  standardGeneric("getFunctionalAnnotationChartFile")
})
#'
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneListReport
#' @usage \S4method{getFunctionalAnnotationChartFile}{DAVIDWebService}(object, fileName, threshold=0.1, count=2L)
#' @aliases getFunctionalAnnotationChartFile,DAVIDWebService-method
setMethod(f="getFunctionalAnnotationChartFile", 
  signature=signature("DAVIDWebService"), definition=function(object,
  fileName, threshold=0.1, count=2L){
  object$getFunctionalAnnotationChartFile(fileName, threshold=threshold, count=count)
})
#' @exportMethod getFunctionalAnnotationChart
#' @name getFunctionalAnnotationChart
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage getFunctionalAnnotationChart(object, ...)
#' @aliases getFunctionalAnnotationChart
setGeneric(name="getFunctionalAnnotationChart", def=function(object, ...){
  standardGeneric("getFunctionalAnnotationChart")
})
#'
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage \S4method{getFunctionalAnnotationChart}{DAVIDWebService}(object, ...)
#' @aliases getFunctionalAnnotationChart,DAVIDWebService-method
setMethod(f="getFunctionalAnnotationChart",
  signature=signature("DAVIDWebService"), definition=function(object, ...){
  object$getFunctionalAnnotationChart(...)
})
#' @exportMethod getClusterReportFile
#' @name getClusterReportFile
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage getClusterReportFile(object, fileName, type=c("Term", "Gene"), overlap=4L, initialSeed=4L, finalSeed=4L, linkage=0.5, kappa=35L)
#' @aliases getClusterReportFile
setGeneric(name="getClusterReportFile", def=function(object, fileName,
  type=c("Term", "Gene"), overlap=4L, initialSeed=4L, finalSeed=4L, linkage=0.5,
  kappa=35L){
  standardGeneric("getClusterReportFile")
})
#'
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage \S4method{getClusterReportFile}{DAVIDWebService}(object, fileName, type=c("Term", "Gene"), overlap=4L, initialSeed=4L, finalSeed=4L, linkage=0.5, kappa=35L)
#' @aliases getClusterReportFile,DAVIDWebService-method
setMethod(f="getClusterReportFile",
  signature=signature("DAVIDWebService"), definition=function(object, fileName,
  type=c("Term", "Gene"), overlap=4L, initialSeed=4L, finalSeed=4L, linkage=0.5,
  kappa=35L){
  object$getClusterReportFile(fileName, type, overlap, initialSeed, finalSeed,
  linkage, kappa)
})
#' @exportMethod getClusterReport
#' @name getClusterReport
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage getClusterReport(object, type=c("Term", "Gene"), ...)
#' @aliases getClusterReport
setGeneric(name="getClusterReport", def=function(object, type=c("Term", "Gene"),
  ...){
  standardGeneric("getClusterReport")
})
#'
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage \S4method{getClusterReport}{DAVIDWebService}(object, type=c("Term", "Gene"), ...)
#' @aliases getClusterReport,DAVIDWebService-method
setMethod(f="getClusterReport", signature=signature("DAVIDWebService"),
  definition=function(object, type=c("Term", "Gene"), ...){
  object$getClusterReport(type, ...)
})
#' @exportMethod getFunctionalAnnotationTableFile
#' @name getFunctionalAnnotationTableFile
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage getFunctionalAnnotationTableFile(object, fileName)
#' @aliases getFunctionalAnnotationTableFile
setGeneric(name="getFunctionalAnnotationTableFile", def=function(object,
  fileName){
  standardGeneric("getFunctionalAnnotationTableFile")
})
#'
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage \S4method{getFunctionalAnnotationTableFile}{DAVIDWebService}(object, fileName)
#' @aliases getFunctionalAnnotationTableFile,DAVIDWebService-method
setMethod(f="getFunctionalAnnotationTableFile", 
  signature=signature("DAVIDWebService"), definition=function(object,
  fileName){
  object$getFunctionalAnnotationTableFile(fileName)
})
#' @exportMethod getFunctionalAnnotationTable
#' @name getFunctionalAnnotationTable
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage getFunctionalAnnotationTable(object)
#' @aliases getFunctionalAnnotationTable
setGeneric(name="getFunctionalAnnotationTable", def=function(object){
  standardGeneric("getFunctionalAnnotationTable")
})
#'
#' @rdname DAVIDWebService-reports
#' @inheritParams getGeneCategoriesReport
#' @usage \S4method{getFunctionalAnnotationTable}{DAVIDWebService}(object)
#' @aliases getFunctionalAnnotationTable,DAVIDWebService-method
setMethod(f="getFunctionalAnnotationTable",
  signature=signature("DAVIDWebService" ), definition=function(object){
  object$getFunctionalAnnotationTable()
})
