#' DAVID's website gene/term cluster report example files
#' 
#' These datasets correspond to the Functional Annotation Clustering or
#' Gene Functional Classification report obtained in the Database for
#' Annotation, Visualization and Integrated Discovery (DAVID) website, using as
#' input file the ones provided for demo purposes (demoList1 or demoList2)
#' with GOTERM_BP_ALL, GOTERM_MF_ALL and GOTERM_CC_ALL categories.
#' 
#' @format geneCluster1/2 or termCluster1/2 are tab delimitate unstructured
#' files with DAVID format where:
#' \describe{
#'  \item{Cluster header}{
#'    \enumerate{
#'      \item{Type}{Gene Cluster or Annotation Cluster.}
#'      \item{Number}{integer to indicate the cluster label.}
#'      \item{Enrichment Score}{numeric with the geometric mean (in -log scale)
#' of members p-values in a corresponding annotation cluster, is used to rank
#' their biological significance. Thus, the top ranked annotation groups most
#' likely have consistent lower p-values for their annotation members.}
#'      }
#' }
#' \item{Members Header}{according to the type of cluster it can be:
#'    \enumerate{
#'      \item{Gene}{the character vector with "ID", "Gene" and "Name".}
#'      \item{Annotation}{the same columns of a Functional Annotation Chart (see
#'      getFunctionalAnnotationChart).}
#'    }
#' }
#' \item{Members Body}{member data per line according to the respective type
#' of cluster.} 
#' }
#'
#' @references 
#' \enumerate{
#'  \item The Database for Annotation, Visualization and Integrated Discovery 
#'  (\url{davidgeneList.abcc.ncifcrf.gov})
#'  \item Huang, D. W.; Sherman, B. T.; Tan, Q.; Kir, J.; Liu, D.; Bryant, D.; 
#'  Guo, Y.; Stephens, R.; Baseler, M. W.; Lane, H. C.; Lempicki, R. A. DAVID
#'  Bioinformatics Resources: expanded annotation database and novel algorithms
#'  to better extract biology from large gene lists. Nucleic Acids Res,
#'  Laboratory of Immunopathogenesis and Bioinformatics, SAIC-Frederick, Inc.,
#'  National Cancer Institute at Frederick, MD 21702, USA., 2007, 35, W169-W175
#'  \item DAVID Help page
#'  \url{http://david.abcc.ncifcrf.gov/helps/functional_classification.html#textmode}
#' }
#'
#' @source At DAVID's website -> Start Analysis -> Demolist1/2 then go to
#' Functional Annotation Table to obtain the appropriate reports. 
#' \url{david.abcc.ncifcrf.gov}
#'
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType methods
#' @name geneCluster1
#' @rdname DAVIDdemo-clusterReport
#' @keywords datasets
#' @family DAVIDGeneCluster DAVIDTermCluster  DataExamples
NULL
#' @name geneCluster2
#' @rdname DAVIDdemo-clusterReport
NULL
#' @name termCluster1
#' @rdname DAVIDdemo-clusterReport
NULL
#' @name termCluster2
#' @rdname DAVIDdemo-clusterReport
NULL
