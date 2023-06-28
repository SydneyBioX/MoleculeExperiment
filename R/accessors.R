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
#' @param assayName Character string specifying the name of the assay from
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
#' repoDir <- system.file("extdata", package = "MoleculeExperiment")
#' repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")
#' me <- readXenium(repoDir,
#'     keepCols = "essential",
#'     addBoundaries = "cell"
#' )
#'
#' # get insight into molecules slot
#' showMolecules(me)
#'
#' # for developers, use molecules() getter
#' # expect a large output from call below
#' # molecules(me)
#' # alternatively, return rectangular data structure with flatten = TRUE
#' molecules(me, assayName = "detected", flatten = TRUE)
#'
#' # get insight into boundaries slot
#' showBoundaries(me)
#'
#' # for developers, use boundaries() getter
#' # expect a large output from call below
#' # boundaries(me, assayName = "cell")
#' # alternatively, return rectangular data structure with flatten = TRUE
#' boundaries(me, assayName = "cell", flatten = TRUE)
#'
#' # features() getter
#' features(me)
#'
#' # segmentIDs() getter
#' segmentIDs(me, "cell")
#'
#' # setter example
#' # read in and standardise nucleus boundaries too
#' nucleiMEList <- readBoundaries(
#'     dataDir = repoDir,
#'     pattern = "nucleus_boundaries.csv",
#'     segmentIDCol = "cell_id",
#'     xCol = "vertex_x",
#'     yCol = "vertex_y",
#'     keepCols = "essential",
#'     boundariesAssay = "nucleus",
#'     scaleFactorVector = 1
#' )
#'
#' # use `boundaries<-` setter to add nucleus boundaries to the boundaries slot
#' boundaries(me, "nucleus") <- nucleiMEList
#' me
#' @return A MoleculeExperiment object slot.
NULL

#' @rdname accessors
#' @export
#' @importFrom methods is
#' @importFrom cli cli_inform
setMethod("molecules",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object,
                          assayName = "detected",
                          flatten = FALSE) {
        # check arg validity
        .check_if_character(assayName)

        if (!assayName %in% names(object@molecules)) {
            stop("Assay name specified does not exist in molecules slot.
Please specify another assay name in the assayName argument.")
        }

        # get molecules slot information
        if (flatten) {
            big_df <- .flatten_molecules(object, assay_name = assayName)
            return(big_df)
        } else {
            cli::cli_inform(c(
                "{.emph {assayName}} assay transcripts were retrieved.",
                "i" = paste0(
                    "Other transcript assays can be retrieved by",
                    " specifying the {.var assayName} argument."
                )
            ))
            return(object@molecules[assayName])
        }
    }
)

#' @rdname accessors
#' @export
#' @importFrom methods is
setMethod("boundaries",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object, assayName = NULL, flatten = FALSE) {
        # check arg validity
        .check_if_character(assayName)

        # get boundaries slot information
        if (is.null(assayName)) {
            warning(
                "All boundaries assays were returned: ",
                names(object@boundaries), ". To select only a specific boundary
subslot, specify the assayName argument."
            )
            return(object@boundaries)
        } else {
            if (!assayName %in% names(object@boundaries)) {
                stop("Assay name specified does not exist in boundaries slot.
Please specify another assay name in the assayName argument.")
            }
            if (flatten) {
                big_df <- .flatten_boundaries(object, assay_name = assayName)
                return(big_df)
            } else {
                cli::cli_inform(c(
                    "{.emph {assayName}} assay boundaries were retrieved.",
                    "i" = paste0(
                        "Other boundary assays can be retrieved by",
                        " specifying the {.var assayName} argument."
                    )
                ))
                return(object@boundaries[assayName])
            }
        }
    }
)

#' @rdname accessors
#' @export
setMethod("features",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object, assayName = "detected") {
        # check arg validity
        .check_if_character(assayName)

        # get the features from the molecules slot
        samples <- names(object@molecules[[assayName]])
        f_list <- lapply(samples, function(s) {
            names(object@molecules[[assayName]][[s]])
        })
        names(f_list) <- samples
        # TODO: use a verbosity setting to fix this!!!
        return(f_list)
        cli::cli_inform(c(
            " {.emph {assayName}} assay features were retrieved.",
            "i" = paste0(
                "To select features from a different assay, specify it ",
                "assay in the {.var assayName} argument."
            )
        ))

    }
)

#' @rdname accessors
#' @export
setMethod("segmentIDs",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object, assayName = NULL) {
        # check arg validity
        if (is.null(assayName)) {
            stop("Please specify the name of the assay from which to
retrieve the unique IDs. For example, the \"cells\" assay for cell boundaries.")
        }
        .check_if_character(assayName)

        # get the segment IDs from the boundaries slot
        samples <- names(object@boundaries[[assayName]])
        id_ls <- lapply(samples, function(x) {
            names(object@boundaries[[assayName]][[x]])
        })
        names(id_ls) <- samples
        return(id_ls)
    }
)

#' @rdname accessors
#' @export
setMethod("molecules<-",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object, assayName = NULL, value) {
        # check arg validity
        if (is.null(assayName)) {
            stop("No assay name specified in the assayName argument.
                    Please specify a title with which to identify this molecule
                    information later on.")
        }
        .check_if_character(assayName)

        # add new value to molecules slot
        object@molecules[assayName] <- value
        methods::validObject(object)
        return(object)
    }
)

#' @rdname accessors
#' @export
setMethod("boundaries<-",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object, assayName = NULL, value) {
        # check arg validity
        if (is.null(assayName)) {
            stop("No assay name specified in the assayName argument.
                    Please specify a title with which to identify this boundary
                    information later on.")
        }
        .check_if_character(assayName)

        # add new value to boundaries slot
        object@boundaries[assayName] <- value
        methods::validObject(object)
        return(object)
    }
)
