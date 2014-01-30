#' Methods to manipulate DAVID website
#' 
#' \code{DAVIDWebService} class methods to manipulate DAVID website status from
#' R. This includes different functionalities to set up and track the connexion,
#' upload a Gene/Background list, check the species, etc. Note that
#' \code{DAVIDWebService} is a Reference class, hence invoke it
#' using object_name$method_name(parameters). In  addition, the user can use
#' the S4 version style function call (see Details).
#'
#' Available functions include:
#' \describe{
#'  \item{\code{connect}:}{Try to establish a connection with DAVID server using
#'  the provided email.}
#'  \item{\code{is.connected}:}{Check if connected to DAVID server.}
#'  \item{\code{getIdTypes}:}{Returns all acceptable DAVID idTypes.}
#'  \item{\code{addList}:}{Add a gene or background to the current session.}
#'  \item{\code{getAllAnnotationCategoryNames}:}{Returns all available
#'  annotation category names.}
#'  \item{\code{getDefaultCategoryNames}:}{Returns all default category names.}
#'  \item{\code{getGeneListNames}:}{Returns all list names}
#'  \item{\code{getBackgroundListNames}:}{Returns background names.}
#'  \item{\code{getListName}:}{Get the name of the selected list type at a given
#'  position.}
#'  \item{\code{getSpecieNames}:}{Return specie/s of the current gene list.}
#'  \item{\code{getCurrentGeneListPosition}:}{Return the position of current
#'  gene list.}
#'  \item{\code{getCurrentBackgroundListPosition}:}{Return the position of
#'  current background list.}
#'  \item{\code{getCurrentSpeciesPosition}:}{Return current specie/s used
#'  positions for the uploaded gene list.}
#'  \item{\code{setCurrentGeneListPosition}:}{Use the gene list of the given
#'  position.}
#'  \item{\code{setCurrentBackgroundPosition:}}{Use the background list of the
#'  given position.}
#'  \item{\code{setCurrentSpecies}:}{Select the specie/s of the submitted gene
#'  list to use in the analysis.}
#'  \item{\code{setAnnotationCategories}:}{Let the user to select specific
#'  annotation categories.}
#'  \item{\code{getTimeOut}:}{Get apache Axis time out in milliSeconds.}
#'  \item{\code{setTimeOut}:}{Set apache Axis time out in milliSeconds.}
#'  \item{\code{getHttpProtocolVersion}:}{Get apache Axis HTTP_PROTOCOL_VERSION.}
#'  \item{\code{setHttpProtocolVersion}:}{Set apache Axis HTTP_PROTOCOL_VERSION. 
#'  possible values are defined in org.apache.axis2.transport.http.HTTPConstants
#'  class with HEADER_PROTOCOL_XX property. At present available strings are: 
#'  "1.1", "1.0", "HTTP/1.1" and "HTTP/1.0".}
#' }
#'
#' @param object DAVIDWebService class object.
#' @param inputIds character vector with the associated ids.
#' @param idType character with the type of submitted ids.
#' @param listName character to identify the submitted list.
#' @param listType character with the type of list (Gene, Background). Default
#' value is "Gene".
#' @param position integer with the position of the gene/background list to
#' set.
#' @param species numeric vector with the specie/s to use.
#' @param categories character vector with the category name/s to use in the
#' analysis.
#' @param milliSeconds integer with time defined in milli seconds.
#' @param version character with HTTP_PROTOCOL_VERSION to use.
#'
#' @return according to the call one of the following objects can be returned
#'  \item{is.connected}{TRUE if user has registered email with DAVID knowledge
#'  base, FALSE otherwise.}
#'  \item{getIdTypes}{character vector with the available DAVID input ID type.}
#'  \item{addList}{list with two items: i)\code{inDavid}, a numeric with the
#'  percentage of the inputIds in DAVID knowledge database,
#'  ii)\code{unmappedIds}, a character vector with the unmapped ids if listType
#'  is "Gene", NA_character_ otherwise.}
#'  \item{getAllAnnotationCategoryNames}{character vector with the available
#'  DAVID annotation categories.}
#'  \item{getDefaultCategoryNames}{character vector with a subset of the
#'  available DAVID annotation categories, chosen by default.}
#'  \item{getGeneListNames}{return a character vector with the name of
#'  the submitted gene list/s.}
#'  \item{getBackgroundListNames}{character vector with the name of the
#'  available background gene list/s for the submitted gene list/s.}
#'  \item{getListName}{character with the name of the list.}
#'  \item{getSpecieNames}{character vector with the specie/s and in brackets the
#'  number of DAVID Ids of the current gene list, e.g. Homo sapiens(155).}
#'  \item{getCurrentGeneListPosition}{integer with the position of current gene
#'  list if available, NA_integer_ otherwise.}
#'  \item{getCurrentBackgroundListPosition}{integer with the position of current
#'  background list if available, NA_integer_ otherwise.}
#'  \item{getCurrentSpeciesPosition}{integer vector with the specie/s position
#'  under use for the gene list under use if available, NA_character_
#'  otherwise.}
#'
#' @docType methods
#' @exportMethod is.connected
#' @name is.connected
#' @rdname DAVIDWebService-methods
#' @usage is.connected(object)
#' @aliases is.connected
#' @family DAVIDWebService
#' @examples
#' david <- DAVIDWebService$new()
#' david$is.connected()
#' ##Or the equivalent S4 style function call
#' is.connected(david)
#' 
setGeneric(name="is.connected", def=function(object){
  standardGeneric("is.connected")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{is.connected}{DAVIDWebService}(object)
#' @aliases is.connected,DAVIDWebService-method
setMethod(f="is.connected",signature=signature("DAVIDWebService"), 
  definition=function(object){
    object$is.connected()
})
#' @exportMethod connect
#' @name connect
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage connect(object)
#' @aliases connect
#' @family DAVIDWebService
setGeneric(name="connect", def=function(object){
  standardGeneric("connect")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{connect}{DAVIDWebService}(object)
#' @aliases connect,DAVIDWebService-method
setMethod(f="connect",signature=signature("DAVIDWebService"), 
  definition=function(object){
    object$connect()
})
#' @exportMethod getIdTypes
#' @name getIdTypes
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage getIdTypes(object)
#' @aliases getIdTypes
#' @family DAVIDWebService
setGeneric(name="getIdTypes", def=function(object){
  standardGeneric("getIdTypes")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{getIdTypes}{DAVIDWebService}(object)
#' @aliases getIdTypes,DAVIDWebService-method
setMethod(f="getIdTypes",signature=signature("DAVIDWebService"), 
  definition=function(object){
    object$getIdTypes()
})
#' @exportMethod addList
#' @name addList
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage addList(object, inputIds, idType, listName, listType=c("Gene", "Background"))
#' @aliases addList
#' @family DAVIDWebService
setGeneric(name="addList", def=function(object, inputIds, idType, listName,
  listType=c("Gene", "Background")){
  standardGeneric("addList")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{addList}{DAVIDWebService}(object, inputIds, idType, listName, listType=c("Gene", "Background"))
#' @aliases addList,DAVIDWebService-method
setMethod(f="addList",signature=signature("DAVIDWebService"), 
  definition=function(object, inputIds, idType, listName, listType=c("Gene",
  "Background")){
 object$addList(inputIds, idType, listName, listType[1])
})
#' @exportMethod getAllAnnotationCategoryNames
#' @name getAllAnnotationCategoryNames
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage getAllAnnotationCategoryNames(object)
#' @aliases getAllAnnotationCategoryNames
#' @family DAVIDWebService
setGeneric(name="getAllAnnotationCategoryNames", def=function(object){
  standardGeneric("getAllAnnotationCategoryNames")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{getAllAnnotationCategoryNames}{DAVIDWebService}(object)
#' @aliases getAllAnnotationCategoryNames,DAVIDWebService-method
setMethod(f="getAllAnnotationCategoryNames", 
  signature=signature("DAVIDWebService"), definition=function(object){
    object$getAllAnnotationCategoryNames()
})
#' @exportMethod getDefaultCategoryNames
#' @name getDefaultCategoryNames
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage getDefaultCategoryNames(object)
#' @aliases getDefaultCategoryNames
#' @family DAVIDWebService
setGeneric(name="getDefaultCategoryNames", def=function(object){
  standardGeneric("getDefaultCategoryNames")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{getDefaultCategoryNames}{DAVIDWebService}(object)
#' @aliases getDefaultCategoryNames,DAVIDWebService-method
setMethod(f="getDefaultCategoryNames", 
  signature=signature("DAVIDWebService"), definition=function(object){
    object$getDefaultCategoryNames()
})
#' @exportMethod getGeneListNames
#' @name getGeneListNames
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage getGeneListNames(object)
#' @aliases getGeneListNames
#' @family DAVIDWebService
setGeneric(name="getGeneListNames", def=function(object){
  standardGeneric("getGeneListNames")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{getGeneListNames}{DAVIDWebService}(object)
#' @aliases getGeneListNames,DAVIDWebService-method
setMethod(f="getGeneListNames", 
  signature=signature("DAVIDWebService"), definition=function(object){
    object$getGeneListNames()
})
#' @exportMethod getBackgroundListNames
#' @name getBackgroundListNames
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage getBackgroundListNames(object)
#' @aliases getBackgroundListNames
#' @family DAVIDWebService
setGeneric(name="getBackgroundListNames", def=function(object){
  standardGeneric("getBackgroundListNames")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{getBackgroundListNames}{DAVIDWebService}(object)
#' @aliases getBackgroundListNames,DAVIDWebService-method
setMethod(f="getBackgroundListNames", 
  signature=signature("DAVIDWebService"), definition=function(object){
    object$getBackgroundListNames()
})
#' @exportMethod getListName
#' @name getListName
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage getListName(object, listType=c("Gene", "Background"), position=1L)
#' @aliases getListName
#' @family DAVIDWebService
setGeneric(name="getListName", def=function(object, listType=c("Gene",
  "Background"), position=1L){
  standardGeneric("getListName")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{getListName}{DAVIDWebService}(object, listType=c("Gene", "Background"), position=1L)
#' @aliases getListName,DAVIDWebService-method
setMethod(f="getListName", signature=signature("DAVIDWebService"),
  definition=function(object, listType=c("Gene", "Background"), position=1L){
    object$getListName(listType[1], position)
})
#' @exportMethod getSpecieNames
#' @name getSpecieNames
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage getSpecieNames(object)
#' @aliases getSpecieNames
#' @family DAVIDWebService
setGeneric(name="getSpecieNames", def=function(object){
  standardGeneric("getSpecieNames")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{getSpecieNames}{DAVIDWebService}(object)
#' @aliases getSpecieNames,DAVIDWebService-method
setMethod(f="getSpecieNames", signature=signature("DAVIDWebService"),
  definition=function(object){
    object$getSpecieNames()
})
#' @exportMethod getCurrentGeneListPosition
#' @name getCurrentGeneListPosition
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage getCurrentGeneListPosition(object)
#' @aliases getCurrentGeneListPosition
#' @family DAVIDWebService
setGeneric(name="getCurrentGeneListPosition", def=function(object){
  standardGeneric("getCurrentGeneListPosition")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{getCurrentGeneListPosition}{DAVIDWebService}(object)
#' @aliases getCurrentGeneListPosition,DAVIDWebService-method
setMethod(f="getCurrentGeneListPosition",
  signature=signature("DAVIDWebService"), definition=function(object){
    object$getCurrentGeneListPosition()
})
#' @exportMethod getCurrentBackgroundListPosition
#' @name getCurrentBackgroundListPosition
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage getCurrentBackgroundListPosition(object)
#' @aliases getCurrentBackgroundListPosition
#' @family DAVIDWebService
setGeneric(name="getCurrentBackgroundListPosition", def=function(object){
  standardGeneric("getCurrentBackgroundListPosition")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{getCurrentBackgroundListPosition}{DAVIDWebService}(object)
#' @aliases getCurrentBackgroundListPosition,DAVIDWebService-method
setMethod(f="getCurrentBackgroundListPosition",
  signature=signature("DAVIDWebService"), definition=function(object){
    object$getCurrentBackgroundListPosition()
})
#' @exportMethod getCurrentSpeciesPosition
#' @name getCurrentSpeciesPosition
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage getCurrentSpeciesPosition(object)
#' @aliases getCurrentSpeciesPosition
#' @family DAVIDWebService
setGeneric(name="getCurrentSpeciesPosition", def=function(object){
  standardGeneric("getCurrentSpeciesPosition")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{getCurrentSpeciesPosition}{DAVIDWebService}(object)
#' @aliases getCurrentSpeciesPosition,DAVIDWebService-method
setMethod(f="getCurrentSpeciesPosition", signature=signature("DAVIDWebService"),
  definition=function(object){
    object$getCurrentSpeciesPosition()
})
#' @exportMethod setCurrentGeneListPosition
#' @name setCurrentGeneListPosition
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage setCurrentGeneListPosition(object, position)
#' @aliases setCurrentGeneListPosition
#' @family DAVIDWebService
setGeneric(name="setCurrentGeneListPosition", def=function(object, position){
  standardGeneric("setCurrentGeneListPosition")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{setCurrentGeneListPosition}{DAVIDWebService}(object, position)
#' @aliases setCurrentGeneListPosition,DAVIDWebService-method
setMethod(f="setCurrentGeneListPosition",
  signature=signature("DAVIDWebService"), definition=function(object, position){
    object$setCurrentGeneListPosition(position)
})
#' @exportMethod setCurrentBackgroundPosition
#' @name setCurrentBackgroundPosition(position)
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage setCurrentBackgroundPosition(object, position)
#' @aliases setCurrentBackgroundPosition
#' @family DAVIDWebService
setGeneric(name="setCurrentBackgroundPosition", def=function(object, position){
  standardGeneric("setCurrentBackgroundPosition")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{setCurrentBackgroundPosition}{DAVIDWebService}(object, position)
#' @aliases setCurrentBackgroundPosition,DAVIDWebService-method
setMethod(f="setCurrentBackgroundPosition",
  signature=signature("DAVIDWebService"), definition=function(object, position){
    object$setCurrentBackgroundPosition(position)
})
#' @exportMethod setCurrentSpecies
#' @name setCurrentSpecies
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage setCurrentSpecies(object, species)
#' @aliases setCurrentSpecies
#' @family DAVIDWebService
setGeneric(name="setCurrentSpecies", def=function(object, species){
  standardGeneric("setCurrentSpecies")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{setCurrentSpecies}{DAVIDWebService}(object, species)
#' @aliases setCurrentSpecies,DAVIDWebService-method
setMethod(f="setCurrentSpecies", signature=signature("DAVIDWebService"),
  definition=function(object, species){
    object$setCurrentSpecies(species)
})
#' @exportMethod setAnnotationCategories
#' @name setAnnotationCategories
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage setAnnotationCategories(object, categories)
#' @aliases setAnnotationCategories
#' @family DAVIDWebService
setGeneric(name="setAnnotationCategories", def=function(object, categories){
  standardGeneric("setAnnotationCategories")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{setAnnotationCategories}{DAVIDWebService}(object, categories)
#' @aliases setAnnotationCategories,DAVIDWebService-method
setMethod(f="setAnnotationCategories", signature=signature("DAVIDWebService"),
  definition=function(object, categories){
    object$setAnnotationCategories(categories)
})
#'
#' @exportMethod getTimeOut
#' @name getTimeOut
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage getTimeOut(object)
#' @aliases getTimeOut
#' @family DAVIDWebService
setGeneric(name="getTimeOut", def=function(object){
  standardGeneric("getTimeOut")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{getTimeOut}{DAVIDWebService}(object)
#' @aliases getTimeOut,DAVIDWebService-method
setMethod(f="getTimeOut", signature=signature("DAVIDWebService"), 
  definition=function(object){
  getStub(object)$"_getServiceClient"()$getOptions()$getTimeOutInMilliSeconds()
})
#'
#' @exportMethod setTimeOut
#' @name setTimeOut
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage setTimeOut(object, milliSeconds)
#' @aliases setTimeOut
#' @family DAVIDWebService
setGeneric(name="setTimeOut",def = function(object, milliSeconds){
  standardGeneric("setTimeOut")})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{setTimeOut}{DAVIDWebService}(object, milliSeconds)
#' @aliases setTimeOut,DAVIDWebService-method
setMethod(f="setTimeOut", signature=signature("DAVIDWebService"), 
  definition=function(object, milliSeconds){
  ##Get the stub
  stub<-getStub(object)
  
  ##Check for milliSeconds
  if(missing(milliSeconds)){
    milliSeconds<-stub$"_getServiceClient"()$getOptions()$DEFAULT_TIMEOUT_MILLISECONDS
  }

  ##Set new time out value
  stub$"_getServiceClient"()$getOptions()$setTimeOutInMilliSeconds(.jlong(milliSeconds))

  return(invisible(NULL))
})
#'
#' @exportMethod getHttpProtocolVersion
#' @name getHttpProtocolVersion
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage getHttpProtocolVersion(object)
#' @aliases getHttpProtocolVersion
#' @family DAVIDWebService
setGeneric(name="getHttpProtocolVersion", def=function(object){
  standardGeneric("getHttpProtocolVersion")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{getHttpProtocolVersion}{DAVIDWebService}(object)
#' @aliases getHttpProtocolVersion,DAVIDWebService-method
setMethod(f="getHttpProtocolVersion", signature=signature("DAVIDWebService"), 
  definition=function(object){
   axisConstants<-.jnew("org.apache.axis2.transport.http.HTTPConstants")
   stub<-getStub(object)
   stub$"_getServiceClient"()$getOptions()$getProperty(axisConstants$HTTP_PROTOCOL_VERSION) 
})
#'
#' @exportMethod setHttpProtocolVersion
#' @name setHttpProtocolVersion
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage setHttpProtocolVersion(object, version)
#' @aliases setHttpProtocolVersion
#' @family DAVIDWebService
setGeneric(name="setHttpProtocolVersion", def=function(object, version){
  standardGeneric("setHttpProtocolVersion")
})
#'
#' @rdname DAVIDWebService-methods
#' @inheritParams is.connected
#' @usage \S4method{setHttpProtocolVersion}{DAVIDWebService}(object, version)
#' @aliases setHttpProtocolVersion,DAVIDWebService-method
setMethod(f="setHttpProtocolVersion", signature=signature("DAVIDWebService"), 
  definition=function(object, version){

  ##Get possible Protocol Versions
  axisConstants<-.jnew("org.apache.axis2.transport.http.HTTPConstants")
  possibleVersions<-which(regexpr(pattern="HEADER_PROTOCOL_", names(axisConstants))>0)
  possibleVersions<-names(axisConstants)[possibleVersions]

  possibleVersions<-sapply(possibleVersions, function(x){
    .jfield(axisConstants, name=x)})
  
  ##Check version parameter
  if(!missing(version)){
    stopifnot(version %in% possibleVersions)

    stub<-getStub(object)
    stub$"_getServiceClient"()$getOptions()$setProperty(
      axisConstants$HTTP_PROTOCOL_VERSION, version)
  }
  
  return(invisible(NULL))
})
