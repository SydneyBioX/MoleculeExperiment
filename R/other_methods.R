#' Summarization methods to get insights into a MoleculeExperiment object
#'
#' The following methods are useful to get quick view of the contents in a
#' MoleculeExperiment object.
#' For example, showMolecules and showBoundaries summarise the large nested ME
#' list of lists in the molecules and boundaries slots.
#' nFeatures and nTranscripts get the numbers of features or transcripts,
#' respectively. 
#'
#' @param object Name of MoleculeExperiment object of interest.
#' @param assayName Character string specifying the name of the assay from
#' which to view a summary of the contents.
#'
#' @aliases
#' showMolecules
#' showBoundaries
#' nFeatures
#' nTranscripts
#' extent
#'
#' @name summarization
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
#' showMolecules(me)
#' showBoundaries(me)
#'
#' nFeatures(me)
#'
#' nTranscripts(me)
#' @return A MoleculeExperiment object summary.
NULL


#' @importFrom S4Vectors coolcat
.me_show <- function(object) {
    cat("MoleculeExperiment class\n\n")

    mols_assay_names <- names(object@molecules)
    boundaries_assay_names <- names(object@boundaries)

    # show molecules contents
    S4Vectors::coolcat("molecules slot (%d): %s\n", mols_assay_names)

    # show molecules slot contents per asssay and per sample
    for (assay in mols_assay_names) {
        
        cat(paste0("- ", assay, ":", "\n"))
        sample_names <- names(object@molecules[[assay]])
        S4Vectors::coolcat("samples (%d): %s\n", sample_names)
                
        for (sample in sample_names) {
            cat(paste0("-- ", sample, ":\n"))
            # show feature names
            S4Vectors::coolcat("---- features (%d): %s\n",
                MoleculeExperiment::features(object,
                                                assayName = assay)[[sample]])

            # show number of molecules
            features <- object@molecules[[assay]][[sample]]
            numbers_ls <- lapply(names(features),
                                    function(x) {nrow(features[[x]])})
            
            total <- sum(unlist(numbers_ls))
            cat(paste0("---- molecules (", total, ")\n"))

            # show location range of detected molecules
            feature_names <- MoleculeExperiment::features(
                object, assayName = assay)[[sample]]
            gene_x <- lapply(feature_names, function(f) {
                object@molecules[[assay]][[sample]][[f]][["x_location"]]
                })
            gene_y <- lapply(feature_names, function(f) {
                object@molecules[[assay]][[sample]][[f]][["y_location"]]
                })
            x_v <- unlist(gene_x)
            y_v <- unlist(gene_y)
            cat(paste0("---- location range: [",
                round(min(x_v), 2), ",", round(max(x_v), 2), "] x [",
                round(min(y_v), 2), ",", round(max(y_v), 2), "]", "\n"
            ))
        }
    }

    # show boundary contents
    if (is.null(object@boundaries)) {
        cat("\n\nboundaries slot: NULL\n")
    } else {
        S4Vectors::coolcat("\n\nboundaries slot (%d): %s\n",
                            boundaries_assay_names)

        # show the contents per assay and per sample
        for (assay in boundaries_assay_names) {
            cat(paste0("- ", assay, ":", "\n"))
            sample_names <- names(object@boundaries[[assay]])
            S4Vectors::coolcat("samples (%d): %s\n", sample_names)

            for (sample in sample_names) {
                cat(paste0("-- ", sample, ":\n"))
                S4Vectors::coolcat("---- segments (%d): %s\n",
                    MoleculeExperiment::segmentIDs(
                        object, 
                        assayName = assay)[[sample]]
                    )
            }
        }
    }
}

#' @rdname summarization 
setMethod("show",
    signature = signature(object = "MoleculeExperiment"),
    .me_show)

#' @rdname summarization
#' @export
#' @importFrom utils str
setMethod("showMolecules",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        str(object@molecules, max.level = 3, list.len = 2)
    }
)

#' @rdname summarization
#' @export
#' @importFrom utils str
setMethod("showBoundaries",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        str(object@boundaries, max.level = 3, list.len = 2)
    }
)

#' @rdname summarization
#' @export
#' @importFrom utils str
setMethod("extent",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object, assayName = NULL) {
        .stop_if_null(assayName)
        .check_if_character(assayName)
        
        if (!assayName %in% names(object@molecules)) {
            stop("Assay name specified does not exist in molecules slot.
Please specify another assay name in the assayName argument.")
        }

        samples <- names(object@molecules[[assayName]])
        sample_x <- lapply(samples, function(x) {
            f <- MoleculeExperiment::features(object, assayName = assayName)[[x]]
            # get x coordinates for each gene
            gene_x <- lapply(f, function(f) {
                object@molecules[[assayName]][[x]][[f]][["x_location"]]
            })
        })

        sample_y <- lapply(samples, function(x) {
            f <- MoleculeExperiment::features(object, assayName = assayName)[[x]]
            # get y coordinates for each gene
            gene_y <- lapply(f, function(f) {
                object@molecules[[assayName]][[x]][[f]][["y_location"]]
            })
        })

        x_v <- unlist(sample_x)
        y_v <- unlist(sample_y)
        c(
            xmin = min(x_v), xmax = max(x_v),
            ymin = min(y_v), ymax = max(y_v)
        )
    }
)


#' @rdname summarization
#' @export
setMethod("nFeatures",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        # get assayNames from molecules slot 
        all_assayNames <- names(object@molecules)
        for (assayName in all_assayNames) {
            cat(paste("- assay", assayName,":", "\n"))
            # print number of features in each sample and each assay
            all_samples <- names(MoleculeExperiment::molecules(
                object, assayName = assayName)[[assayName]])
            for (sample in all_samples) {
                cat(paste(sample,
                            ":",
                            length(object@molecules[[assayName]][[sample]]),
                            "\n"
                        )
                    )
            }
        }
    }
)


#' @rdname summarization
#' @export
setMethod("nTranscripts",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {

        # get assayNames from molecules slot 
        all_assayNames <- names(object@molecules)
        for (assayName in all_assayNames) {
            cat(paste("- assay", assayName,":", "\n"))
            # print number of transcripts in each sample and each assay
            all_samples <- names(MoleculeExperiment::molecules(
                object, assayName = assayName)[[assayName]])
            for (sample in all_samples) {
                features <- object@molecules[[assayName]][[sample]]
                numbers_ls <- lapply(names(features),
                                    function(x) {nrow(features[[x]])})
                total <- sum(unlist(numbers_ls))
                cat(paste(sample, ":", total, "\n")
                )
            }
        }
    }
)
