#' DAVID's website functional annotation table example files
#' 
#' These datasets correspond to the Functional Annotation Table report obtained
#' in the Database for Annotation, Visualization and Integrated Discovery
#' (DAVID) website, using as input file, the ones provided for demo purposes
#' (demoList1 or demoList2) for GOTERM_BP_ALL, GOTERM_MF_ALL and GOTERM_CC_ALL
#' categories. No statistical analysis is performed on these results.
#' 
#' @format annotationTable1/2 are data.frame for demoList1/2 input ids,
#' respectively, with the following columns.
#' \describe{
#' \item{Gene}{Three Columns with the same data included in Gene List Report
#' (ID, Gene.Name and Species) but coding for DAVID ID, i. e., comma separated
#' character with input ids if, two or more stands for the same gene.}
#' \item{Annotation}{As many columns as Annotation Categories were used.
#' In each column, a comma separated style is use to delimitate the different
#' terms where is evidence reported for DAVID ID record.}
#'  }
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
#'  \url{http://david.abcc.ncifcrf.gov/helps/functional_annotation.html#EXP2}
#' }
#'
#' @source At DAVID's website -> Start Analysis -> Demolist1/2 then go to
#' Functional Annotation Table to obtain the appropriate reports. 
#' \url{david.abcc.ncifcrf.gov}
#'
#' @author Cristobal Fresno and Elmer A Fernandez
#'
#' @docType data
#' @name annotationTable1
#' @rdname DAVIDdemo-annotationTable
#' @usage data(annotationTable1)
#' @keywords datasets
#' @family DAVIDFunctionalAnnotationTable DataExamples
NULL
#' @name annotationTable2
#' @rdname DAVIDdemo-annotationTable
#' @usage data(annotationTable2)
NULL
