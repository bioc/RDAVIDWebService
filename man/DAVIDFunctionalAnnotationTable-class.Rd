\docType{class}
\name{DAVIDFunctionalAnnotationTable-class}
\alias{DAVIDFunctionalAnnotationTable-class}
\title{class "DAVIDFunctionalAnnotationTable}
\description{
  This class represents the output of a DAVID Functional
  Annotation Table report. In this class no statistical
  analysis is carried out.
}
\section{Type}{
  This class is a "\code{Concrete}" one.
}

\section{Extends}{
  \itemize{ \item \emph{DAVIDResult} in the conceptual way,
  and to reuse some functionalities such as plot2D, type
  and so on. }
}

\section{Slots}{
  \describe{ \item{\code{Genes}}{a DAVIDGenes object with
  the submitted genes.} \item{\code{Dictionary}}{a look up
  list of data.frame of each main annotation category,
  where the specified IDs and Terms used can be found.}
  \item{\code{Membership}}{list with logical membership
  matrix, where gene ids are coded by rows and the
  respective annotation category ids as columns.} }
}

\section{Methods}{
  \describe{
  \item{\code{initialize}}{\code{signature(.Object=
  "DAVIDFunctionalAnnotationTable", fileName="character")}:
  basic Functional Annotation Table report file parser.}
  \item{\code{DAVIDFunctionalAnnotationTable}}{\code{signature(fileName=
  "character")}: high level Functional Annotation Table
  report file parser.}
  \item{\code{valid}}{\code{signature(object=
  "DAVIDFunctionalAnnotationTable")}:logical which checks
  for Membership, Dictionary and Genes cohesion.}
  \item{\code{show}}{\code{signature(object="DAVIDFunctionalAnnotationTable")}:
  returns a basic console output.}
  \item{\code{genes}}{\code{signature(object="DAVIDFunctionalAnnotationTable")}
  : returns a DAVIDGenes object.}
  \item{\code{subset}}{\code{signature(object=
  "DAVIDFunctionalAnnotationTable",
  selection=c("Membership","Dictionary"), category,
  drop=TRUE)}: returns a subset list using the selection
  slot, looking up the category parameter if provided.
  Otherwise, it returns all the available main categories.
  Drop parameter indicates whether to drop list structure
  or not, if a list of length==1 is to be returned.}
  \item{\code{dictionary}}{\code{signature(object=
  "DAVIDFunctionalAnnotationTable", category, drop=TRUE)}:
  returns subset using selection="Dictionary" and category
  and drop parameters.}
  \item{\code{membership}}{\code{signature(object=
  "DAVIDFunctionalAnnotationTable", category="character",
  drop=TRUE)}: returns subset using selection="Membership"
  and category and drop parameters.}
  \item{\code{genes}}{\code{signature(object=
  "DAVIDFunctionalAnnotationTable", ...)}: returns a
  DAVIDGenes object slot, according to additional ...
  parameters.}
  \item{\code{categories}}{\code{signature(object=
  "DAVIDFunctionalAnnotationTable")}: returns a character
  vector with the main annotation categories available..}
  \item{\code{plot2D}}{\code{signature(object="DAVIDFunctionalAnnotationTable",
  category, id, names.genes=FALSE, names.category=FALSE,
  color=c("FALSE"="black", "TRUE"="green"))}: ggplot2 tile
  plot of genes id vs functional annotation category
  membership. If missing, all available data is used. In
  addition, names.genes and names.category parameters
  indicate whether to use or not, genes and category names
  respectively. Default value is FALSE.} }
}
\examples{
{
##Load the Functional Annotation Table file report for the input demo
##file 1, using data function. Then, create a DAVIDFunctionalAnnotationTable
##object using the loaded data.frame annotationTable1. In addition, the user
##can use the file name of the downloaded file report.
data(annotationTable1)
davidFunTable1<-DAVIDFunctionalAnnotationTable(annotationTable1)

##Now we can obtain the genes for the given ids, or the complete list if the
##parameter is omitted.
genes(davidFunTable1, id=c("37166_at","41703_r_at"))

##Or the main categories used on the analysis, in order to get the
##dictionary for a specific category (ID and Term fields), for the head of
##the data.frame.
categories(davidFunTable1)
head(dictionary(davidFunTable1, categories(davidFunTable1)[1]))

##And what about the membership of the genes in these terms? Just for the
##first six ids we can use:
head(membership(davidFunTable1, categories(davidFunTable1)[1]))

##Or simply plot the membership of only for the first six terms in this
##category, with only the genes of the first six terms with at least one
##evidence code.
##Category filtering...
categorySelection<-list(head(dictionary(davidFunTable1,
categories(davidFunTable1)[1])$ID))
names(categorySelection)<-categories(davidFunTable1)[1]

##Gene filter...
id<-membership(davidFunTable1, categories(davidFunTable1)[1])[,1:6]
id<-ids(genes(davidFunTable1))[rowSums(id)>0]

##Finally the membership tile plot
plot2D(davidFunTable1, category=categorySelection, id=id,
names.category=TRUE)
}
}
\author{
  Cristobal Fresno and Elmer A Fernandez
}
\references{
  \enumerate{ \item The Database for Annotation,
  Visualization and Integrated Discovery
  (david.abcc.ncifcrf.gov) \item Huang, D. W.; Sherman, B.
  T.; Tan, Q.; Kir, J.; Liu, D.; Bryant, D.; Guo, Y.;
  Stephens, R.; Baseler, M. W.; Lane, H. C. & Lempicki, R.
  A. DAVID Bioinformatics Resources: expanded annotation
  database and novel algorithms to better extract biology
  from large gene lists. Nucleic Acids Res, Laboratory of
  Immunopathogenesis and Bioinformatics, SAIC-Frederick,
  Inc., National Cancer Institute at Frederick, MD 21702,
  USA., 2007, 35, W169-W175 }
}
\seealso{
  Other DAVIDFunctionalAnnotationTable:
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationChart}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDFunctionalAnnotationTable}},
  \code{\link{DAVIDGODag}}, \code{\link{DAVIDGODag}},
  \code{\link{DAVIDGeneCluster}},
  \code{\link{DAVIDGeneCluster}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDGenes}}, \code{\link{DAVIDGenes}},
  \code{\link{DAVIDTermCluster}},
  \code{\link{DAVIDTermCluster}}, \code{\link{as}},
  \code{\link{as}}, \code{\link{as}},
  \code{\link{categories}}, \code{\link{categories}},
  \code{\link{categories}}, \code{\link{dictionary}},
  \code{\link{dictionary}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{genes}},
  \code{\link{genes}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{initialize}}, \code{\link{initialize}},
  \code{\link{membership}}, \code{\link{membership}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{plot2D}}, \code{\link{plot2D}},
  \code{\link{subset}}, \code{\link{subset}}
}
\keyword{classes}

