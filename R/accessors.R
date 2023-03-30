#' Accessor functions to work with MoleculeExperiment objects
#'
#' @section getters:
#' Accessor functions to get data from the MoleculeExperiment object.
#' These include:
#' - molecules() to retrieve information from the molecules slot.
#' - boundaries() to retrieve information from the boundaries slot.
#' - features() to retrieve feature names from the molecules slot.
#' - segmentIDs() to retrieve segment ids from the boundaries slot.
#'
#' @section setters:
#' The `molecules<-` setter accesses the molecules slot, whereas the boundaries
#' slot can be accessed with `boundaries<-`.
#'
#' @param object The MoleculeExperiment to access.
#' @param assay_name Character string specifying the name of the assay from
#' which to retrieve or set information in the slot of interest.
#' @param flatten Logical value specifying whether to flatten the ME list into
#' a data.frame or not. Defaults to FALSE.
#' @param value New value to be added to the slot and assay of interest.
#'
#' @aliases
#' molecules
#' boundaries
#' features
#' segmentIDs
#' molecules<-
#' boundaries<-
#'
#' @name accessors
#' @docType methods
#'
#' @examples
#' # get example data
#' repo_dir <- system.file("extdata", package = "MoleculeExperiment")
#' me <- readXenium(repo_dir,
#'                   n_samples = 2,
#'                   keep_cols = "essential",
#'                   add_boundaries = "cell")
#'
#' # molecules() getter
#' molecules(me)
#' molecules(me, assay_name = "detected", flatten = TRUE)
#'
#' # boundaries() getter
#' boundaries(me, assay_name = "cell")
#' boundaries(me, assay_name = "cell", flatten = TRUE)
#'
#' # features() getter
#' features(me)
#'
#' # segmentIDs() getter
#' segmentIDs(me, "cell")
#'
#' # setter example
#' # read in and standardise nucleus boundaries too
#' nuclei_ls <- readBoundaries(data_dir = repo_dir,
#'                             pattern = "nucleus_boundaries.csv",
#'                             n_samples = 2,
#'                             segment_id_col = "cell_id",
#'                             x_col = "vertex_x",
#'                             y_col = "vertex_y",
#'                             keep_cols = "essential",
#'                             boundaries_assay = "nucleus",
#'                             scale_factor_vector = 1)
#'
#' # use `boundaries<-` setter to add nucleus boundaries to the boundaries slot
#' boundaries(me, "nucleus") <- nuclei_ls
#' me
NULL

#' @rdname accessors
#' @export
setMethod("molecules",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object,
                          assay_name = "detected",
                          flatten = FALSE) {
        if (assay_name == "detected") {
            message("The transcripts from the detected assay were
retrieved. Other assay transcripts can be retrieved by specifying the assay_name
argument.")
        }
        if (! assay_name %in% names(object@molecules)){
            stop("Assay name specified does not exist in molecules slot.
Please specify another assay name in the assay_name argument.")
        }
        if (flatten) {
            big_df <- .flatten_molecules(object, assay_name)
            return(big_df)
        } else {
            object@molecules[assay_name]
        }
    }
)

#' @rdname accessors
#' @export
setMethod("boundaries",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object, assay_name = NULL, flatten = FALSE) {
        if (is.null(assay_name)) {
            warning(
                "All boundaries assays were returned: ",
                names(object@boundaries), ". To select only a specific",
                "boundary subslot, specify the assay_name argument."
            )

            if (flatten) {
                unlist(object@boundaries)
            } else {
                object@boundaries
            }
        } else {
            if (flatten) {
                big_df <- .flatten_boundaries(object, assay_name)
                return(big_df)
            } else {
                object@boundaries[assay_name]
            }
        }
    }
)

#' @rdname accessors
#' @export
setMethod("features",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object, assay_name = "detected") {
        samples <- names(object@molecules[[assay_name]])
        f_list <- lapply(samples, function(s) {
            names(object@molecules[[assay_name]][[s]])
        })
        names(f_list) <- samples

        return(f_list)

        message("Features collected: ", assay_name, " assay.
To select features from a different assay, specify that assay in the
assay_name argument to this function.")
    }
)

#' @rdname accessors
#' @export
setMethod("segmentIDs",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object, assay_name = NULL) {
        if (is.null(assay_name)) {
            stop("Please specify the name of the assay from which to
retrieve the unique IDs. For example, the \"cells\" assay for cell boundaries.")
        }
        samples <- names(object@boundaries[[assay_name]])
        id_ls <- lapply(samples, function(x) {
            names(object@boundaries[[assay_name]][[x]])
        })
        names(id_ls) <- samples
        return(id_ls)
    }
)

#' @rdname accessors
#' @export
setMethod("molecules<-",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object, assay_name = NULL, value) {
                if (is.null(assay_name)) {
                    stop("No assay name specified in the assay_name argument.
                    Please specify a title with which to identify this molecule
                    information later on.")
                }
                object@molecules[assay_name] <- value
                methods::validObject(object)
                return(object)
            }
)

#' @rdname accessors
#' @export
setMethod("boundaries<-",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object, assay_name = NULL, value)  {
                if (is.null(assay_name)) {
                    stop("No assay name specified in the assay_name argument.
                    Please specify a title with which to identify this boundary
                    information later on.")
                }
                object@boundaries[assay_name] <- value
                methods::validObject(object)
                return(object)
            }
)