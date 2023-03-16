# define new generics for each of the new 9 methods here.
# ORDER ALPHANUMERICALLY

#' @export
setGeneric("molecules",
    function(x) standardGeneric("molecules"))

#' @export
setGeneric("addMoleculeData<-",
    function(x, value) standardGeneric("addMoleculeData<-"))

