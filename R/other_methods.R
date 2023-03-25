# -----------------------------------------------------------------------------
# extend the show method to avoid plaguing the console with object contents
# give user a hint of the contents of the ME obj

#' @importFrom utils str
setMethod("show",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        cat("class: ", class(object), "\n")
        cat(paste(
                length(me@molecules[["raw"]]),
                "samples:",
                paste(head(names(me@molecules[["raw"]])), collapse = " ")),
            "\n")

        cat("@molecules contents: ", "-raw assay:", sep = "\n")
        ## -— 541 features across all samples: Abbc....
        paste0("--", nFeatures())
        ## -— 60,000,000 molecules across all samples:
        nTranscripts()
        ## -— location range: [0,100] x [0,200] x [0,5]
        cat(paste0("[", "]", sep = ","),
            "x",
            paste0("[", "]", sep = ",")
            )
        all <- head(names(me@molecules))
        paste0("-other assays: ",
                paste(all[all != "raw"], sep = ",", collapse = " "))

        if (is.null(me@boundaries)) {
            cat("@boundaries contents: NULL\n")
        } else {
            cat("@boundaries contents:\n")
            for (i in names(me@boundaries)) {
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



# -----------------------------------------------------------------------------
# method to calculate total number of molecules identified for each unique
# features per sample
# -----------------------------------------------------------------------------

setMethod("nFeatures",
    signature = signature(object = "MoleculeExperiment"),
    definition = function(object) {
        # identify sample
        samples <- names(object@molecules)

        for (s in samples) {

        }

        length(object@molecules)
    }
)

# also add method for features()

## method to calculate the total number of transcripts/molecules per sample.
# setMethod("nTranscripts",
#          signature = signature(object = "MoleculeExperiment"),
#          definition = function(object) {
#
#          }
# )
#
