# -----------------------------------------------------------------------------
# extend the show method to avoid plaguing the console with object contents
# give user a hint of the contents of the ME obj

#' @importFrom utils str
setMethod("show",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        cat("class: ", class(object), "\n")
        cat(paste(
                length(object@molecules[["raw"]]),
                "samples:",
                paste(head(names(object@molecules[["raw"]])), collapse = " ")),
            "\n")

        cat("\n@molecules contents: ", "-raw assay:", sep = "\n")
        nFeatures(object, "raw")
        nTranscripts(object, "raw")

        ################################################
        samples <- features(object)

        lapply(names(samples), function(s, f) {
            f <- features(object)[[s]]
            object@molecules[["raw"]][[s]][[f]]})

        features(object)

        object@molecules[["raw"]][[]]
        # find values for column named X LOCATION

        # vector with x coordinates across ALL samples
        x_v <-
        y_v <-
        cat(paste0("Location range across all samples in assay raw: [",
            min(x_v), ",", max(x_v), "] x [", min(y_v), ",", max(y_v), "]"))

        all <- head(names(object@molecules))

        paste0("-other assays: ",
                paste(all[all != "raw"], sep = ",", collapse = " "))

        if (is.null(object@boundaries)) {
            cat("\n@boundaries contents: NULL\n")
        } else {
            cat("\n@boundaries contents:\n")
            for (i in names(object@boundaries)) {
                cat(paste0(i, "\n"))

                ## -- number of unique compartments
                cat(paste0(, "compartments"))

                ## —- location range
                # first concatenate all the x (or all the y's), and then find 
                # min and max
                cat("[", "]",
                "x",
                "[", "]")

                cat()
            }
        }
    }
)

# -----------------------------------------------------------------------------
# summarise the large nested list of lists in the molecules and boundaries slots
#' @importFrom utils str
setMethod("strMolecules",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        str(object@molecules, max.level = 3, list.len = 2)
    }
)

#' @importFrom utils str
setMethod("strBoundaries",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        str(object@boundaries, max.level = 3, list.len = 2)
    }
)

# -----------------------------------------------------------------------------
# method to calculate unique features across samples
setMethod("nFeatures",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object, assay_name = "raw", per_sample = FALSE) {
        if(per_sample) {
            return(lengths(object@molecules[[assay_name]]))
        } else {

            f_sample <- lapply(names(object@molecules[[assay_name]]),
                                function(t) {
                                    names(object@molecules[[assay_name]][[t]])})

            number <- length(unique(unlist(f_sample)))

            cat(paste0(number, " unique features across all samples in assay ",
                        assay_name, "\n"))

        }
    }
)

# -----------------------------------------------------------------------------
# method to calculate total number of transcripts across samples
setMethod("nTranscripts",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object,
                               assay_name = "raw",
                               per_sample = FALSE) {

        # get number of genes per sample
        samples <- names(object@molecules[[assay_name]])

        sample_numbers <- vector("integer", length(samples))
        for (s in samples) {
            # for each sample, pre-assign memory with length of features
            features <- object@molecules[[assay_name]][[s]]
            numbers_ls <- lapply(names(features), function(x) {
                                nrow(features[[x]])})
            total <- sum(unlist(numbers_ls))
            sample_numbers <- replace(sample_numbers, values = total)
        }

        if (per_sample) {
            names(sample_numbers) <- samples
            return(sample_numbers)
        } else {
            cat(paste0(
mean(sample_numbers), " molecules on average across all samples in assay ",
assay_name, "\n"))
        }
    }
)



## -----------------------------------------------------------------------------
## method to filter features (e.g., NegControl probes) from the @molecules slot.
## useful functions: group_by() group_indices(), group_rows(), tally(), ungroup()
# setMethod("filterFeatures",
#            signature = signature(object = "MoleculeExperiment"),
#            definition = function(object) {
##    # check that modified object is still a valid instance of ME class
##    validObject(x)
#            }
# )
#
## method to filter rows from the tibbles in the @molecules slot
## e.g., to filter out transcripts that are annotated as being in the nuclei
# setMethod("filterMoleculeData",
#            signature = signature(object = "MoleculeExperiment"),
#            definition = function(object) {
##    # check that modified object is still a valid instance of ME class
##    validObject(x)
#            }
# )
#
# useful functions: group_by() group_indices(), group_rows(), tally(), ungroup()