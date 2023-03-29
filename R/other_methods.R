# =============================================================================
# Various other methods to work with a MoleculeExperiment object.
# =============================================================================

# -----------------------------------------------------------------------------
# extended show method to avoid plaguing the console with object contents
# give user a hint of the contents of the ME obj

setMethod("show",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        cat("class: ", class(object), "\n")
        cat(paste(
                length(object@molecules[["detected"]]),
                "samples:",
                paste(utils::head(names(object@molecules[["detected"]])),
                    collapse = " ")),
            "\n")

        cat("\n@molecules contents: ", "-detected assay:", sep = "\n")
        nFeatures(object, "detected")
        nTranscripts(object, "detected")

        # show range of coordinates
        samples <- names(object@molecules[["detected"]])
        sample_x <- lapply(samples, function(x) {
                    f <- features(object)[[x]]
                    # get x coordinates for each gene
                    gene_x <- lapply(f, function(f) {
                        object@molecules[["detected"]][[x]][[f]][["x_location"]]})
                    })

        sample_y <- lapply(samples, function(x) {
                    f <- features(object)[[x]]
                    # get y coordinates for each gene
                    gene_y <- lapply(f, function(f) {
                        object@molecules[["detected"]][[x]][[f]][["y_location"]]})
                    })
        
        x_v <- unlist(sample_x)
        y_v <- unlist(sample_y)
        cat(paste0("Location range across all samples in assay \"detected\": [",
            round(min(x_v), 2), ",", round(max(x_v), 2), "] x [",
            round(min(y_v), 2), ",", round(max(y_v), 2), "]", "\n"))

        if (length(names(object@molecules)) > 1) {
            all <- utils::head(names(object@molecules))
            cat(paste0("-other assays: ",
                    paste(all[all != "detected"], sep = ",", collapse = " "), "\n"))
        }

        if (is.null(object@boundaries)) {
            cat("\n@boundaries contents: NULL\n")
        } else {
            cat("\n@boundaries contents:\n")
            for (i in names(object@boundaries)) {
                cat(paste0("-", i, ":\n"))
                id_ls <- segmentIDs(object, assay_name = i)
                n_comp <- mean(lengths(id_ls))
                cat(paste0(n_comp, " unique segment IDs: ",
                    paste(utils::head(id_ls[[1]]), collapse = " "), " ...\n"))

                # get boundary centroid coordinates across all samples
                sample_x <- lapply(names(object@boundaries[[i]]),
                    function(x) {
                        c <- segmentIDs(object, i)[[x]]

                        c_x <- lapply(c, function(c) {
                            object@boundaries[[i]][[x]][[c]][["x_location"]]})
                    })
                sample_y <- lapply(names(object@boundaries[[i]]),
                    function(x) {
                        c <- segmentIDs(object, i)[[x]]
                        c_y <- lapply(c, function(c) {
                            object@boundaries[[i]][[x]][[c]][["y_location"]]})
                    })
                c_x_v <- unlist(sample_x)
                c_y_v <- unlist(sample_y)
                cat(paste0("Location range across all samples: [",
                    round(min(c_x_v), 2), ",", round(max(c_x_v), 2), "] x [",
                    round(min(c_y_v), 2), ",", round(max(c_y_v), 2), "]\n"))
            }
        }
    }
)

# -----------------------------------------------------------------------------
# summarise large nested list of lists in the molecules and boundaries slots

#' @rdname MoleculeExperiment-class
#' @importFrom utils str
setMethod("strMolecules",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        str(object@molecules, max.level = 3, list.len = 2)
    }
)

#' @rdname MoleculeExperiment-class
#' @importFrom utils str
setMethod("strBoundaries",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        str(object@boundaries, max.level = 3, list.len = 2)
    }
)

# -----------------------------------------------------------------------------
# method to calculate unique features across samples


#' @rdname MoleculeExperiment-class
setMethod("nFeatures",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object, assay_name = "detected", per_sample = FALSE) {
        if (per_sample) {
            return(lengths(object@molecules[[assay_name]]))
        } else {

            f_sample <- lapply(names(object@molecules[[assay_name]]),
                                function(t) {
                                    names(object@molecules[[assay_name]][[t]])})

            number <- length(unique(unlist(f_sample)))

            cat(paste0(number,
                        " unique features across all samples in assay \"",
                        assay_name, "\": ",
                        paste(utils::head(features(object, assay_name)[[1]]),
                            collapse = " "),
                        " ...", "\n"))

        }
    }
)


# -----------------------------------------------------------------------------
# method to calculate total number of transcripts across samples


#' @rdname MoleculeExperiment-class
setMethod("nTranscripts",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object,
                               assay_name = "detected",
                               per_sample = FALSE) {

        # get number of genes per sample
        samples <- names(object@molecules[[assay_name]])

        sample_numbers <- vector("integer", length(samples))
        names(sample_numbers) <- samples
        for (s in samples) {
            features <- object@molecules[[assay_name]][[s]]
            numbers_ls <- lapply(names(features), function(x) {
                                nrow(features[[x]])})
            total <- sum(unlist(numbers_ls))
            sample_numbers[[s]] <- total
        }

        if (per_sample) {
            return(sample_numbers)
        } else {
            cat(paste0(
mean(sample_numbers), " molecules on average across all samples in assay \"",
assay_name, "\"\n"))
        }
    }
)