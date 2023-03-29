# =============================================================================
# Getters for accessing data from a MoleculeExperiment object
# =============================================================================

# -----------------------------------------------------------------------------

#' @rdname MoleculeExperiment-class
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
        if (flatten) {
            big_df <- .flatten_molecules(object, assay_name)
            return(big_df)
        } else {
            object@molecules[assay_name]
        }
    }
)

#' @rdname MoleculeExperiment-class
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


# -----------------------------------------------------------------------------

#' @rdname MoleculeExperiment-class
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

# -----------------------------------------------------------------------------
# get list of segment ids identified after segmentation in each sample

#' @rdname MoleculeExperiment-class
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
