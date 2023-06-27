#' Summarization methods to get insights into a MoleculeExperiment object
#'
#' The following methods are useful to get quick view of the contents in a
#' MoleculeExperiment object.
#' For example, showMolecules and showBoundaries summarise the large nested ME
#' list of lists in the molecules and boundaries slots.
#' nFeatures and nTranscripts get the numbers of features or transcripts,
#' respectively. They can do so across all samples, or per sample.
#'
#' @param object Name of MoleculeExperiment object of interest.
#' @param assayName Character string specifying the name of the assay from
#' which to view a summary of the contents.
#' @param perSample Logical value specifying whether or not to summarize the
#' information per sample.
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
#' nFeatures(me, perSample = TRUE)
#'
#' nTranscripts(me)
#' nTranscripts(me, perSample = TRUE)
#' @return A MoleculeExperiment object summary.
NULL


setMethod("show",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        # TODO: Make these methods assay name agnostic
        cat("class: ", class(object), "\n")
        cat(
            paste(
                length(object@molecules[["detected"]]),
                "samples:",
                paste(utils::head(names(object@molecules[["detected"]])),
                    collapse = " "
                )
            ),
            "\n"
        )

        cat("\n@molecules contents: ", "-detected assay:", sep = "\n")
        nFeatures(object, "detected")
        nTranscripts(object, "detected")

        # show range of coordinates
        samples <- names(object@molecules[["detected"]])
        sample_x <- lapply(samples, function(x) {
            f <- features(object)[[x]]
            # get x coordinates for each gene
            gene_x <- lapply(f, function(f) {
                object@molecules[["detected"]][[x]][[f]][["x_location"]]
            })
        })

        sample_y <- lapply(samples, function(x) {
            f <- features(object)[[x]]
            # get y coordinates for each gene
            gene_y <- lapply(f, function(f) {
                object@molecules[["detected"]][[x]][[f]][["y_location"]]
            })
        })

        x_v <- unlist(sample_x)
        y_v <- unlist(sample_y)
        cat(paste0(
            "Location range across all samples in assay \"detected\": [",
            round(min(x_v), 2), ",", round(max(x_v), 2), "] x [",
            round(min(y_v), 2), ",", round(max(y_v), 2), "]", "\n"
        ))

        if (length(names(object@molecules)) > 1) {
            all <- utils::head(names(object@molecules))
            cat(paste0(
                "-other assays: ",
                paste(all[all != "detected"], sep = ",", collapse = " "), "\n"
            ))
        }

        if (is.null(object@boundaries)) {
            cat("\n@boundaries contents: NULL\n")
        } else {
            cat("\n@boundaries contents:\n")
            for (i in names(object@boundaries)) {
                cat(paste0("-", i, ":\n"))
                id_ls <- segmentIDs(object, assayName = i)
                n_comp <- mean(lengths(id_ls))
                cat(paste0(
                    n_comp, " unique segment IDs: ",
                    paste(utils::head(id_ls[[1]]), collapse = " "), " ...\n"
                ))
            }
        }
    }
)

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
setMethod("extent",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        samples <- names(object@molecules[["detected"]])
        sample_x <- lapply(samples, function(x) {
            f <- features(object)[[x]]
            # get x coordinates for each gene
            gene_x <- lapply(f, function(f) {
                object@molecules[["detected"]][[x]][[f]][["x_location"]]
            })
        })

        sample_y <- lapply(samples, function(x) {
            f <- features(object)[[x]]
            # get y coordinates for each gene
            gene_y <- lapply(f, function(f) {
                object@molecules[["detected"]][[x]][[f]][["y_location"]]
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
#' @importFrom utils str
setMethod("showBoundaries",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        str(object@boundaries, max.level = 3, list.len = 2)
    }
)

#' @rdname summarization
#' @export
setMethod("nFeatures",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object, assayName = "detected", perSample = FALSE) {
        # check arg validity
        .check_if_character(assayName)

        # calculate number of features in molecules slot
        if (perSample) {
            return(lengths(object@molecules[[assayName]]))
        } else {
            f_sample <- lapply(
                names(object@molecules[[assayName]]),
                function(t) {
                    names(object@molecules[[assayName]][[t]])
                }
            )

            number <- length(unique(unlist(f_sample)))

            features <- paste(
                utils::head(features(object, assayName)[[1]]),
                collapse = " "
            )
            cli::cli_inform(paste0(
                "{number} unique features across all samples in assay ",
                "{.emph {assayName}}: ",
                "{features}",
                "..."
            ))

            number
        }
    }
)


#' @rdname summarization
#' @export
setMethod("nTranscripts",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object,
                          assayName = "detected",
                          perSample = FALSE) {
        # check arg validity
        .check_if_character(assayName)

        # calculate total transcripts in molecules slot
        samples <- names(object@molecules[[assayName]])
        sample_numbers <- vector("integer", length(samples))
        names(sample_numbers) <- samples
        for (s in samples) {
            features <- object@molecules[[assayName]][[s]]
            numbers_ls <- lapply(names(features), function(x) {
                nrow(features[[x]])
            })
            total <- sum(unlist(numbers_ls))
            sample_numbers[[s]] <- total
        }

        if (perSample) {
            return(sample_numbers)
        } else {
            cli::cli_inform(paste0(
                "{mean(sample_numbers)} molecules on average across all",
                " samples in assay {.emph {assayName}}"
            ))
        }
    }
)
