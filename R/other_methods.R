# -----------------------------------------------------------------------------
# extend the show method to avoid plaguing the console with object contents
# give user a hint of the contents of the ME obj
# -----------------------------------------------------------------------------

# how we might want the MoleculeExperiment to display
# > me
# class: MoleculeExperiment
# 3 samples: Xenium....

# @molecules contents:
# RAW
# 541 features: Abbc....
# 60,000,000 molecules
# location range: [0,100] x [0,200] x [0,5]
# HIGH THRESHOLD (truncated)

# @boundaries contents:
# 145000 cells
# location range (first concatenate and then find min and max)
# 145000 nuclei
# location range (first concatenate and then find min and max)

#'@importFrom utils str
setMethod("show",
          signature = signature(object = "MoleculeExperiment"),
          definition = function(object) {

              # print summary message
              msg <- c(paste0("Object class: ", class(object)),
                       paste0("Slots: ", slotNames(object)))
              cat(msg, sep = "\n")

              # print with commas separating
              ## samples: do not print all names
              cat("Sample IDs: ", names(object@molecules), sep = "\n")
              cat("\n")

              # print numbers of features per sample
              # use my own nFeatures method for this?
              #paste0("The molecules slot has: ", )

              # print some contents of molecules slot
              cat("molecules slot contents: ", "\n")
              str(object@molecules, list.len = 2, no.list = TRUE)

              # show total features
              # show x range y range

              # TODO print some contents of boundaries slot
          }
)

# -----------------------------------------------------------------------------
# summarise the large nested list of lists in the molecules and boundaries slots
#'@importFrom utils str
setMethod("summariseMolecules",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object) {
                str(object@molecules, max.level = 2)
            }
)

#'@importFrom utils str
setMethod("summariseBoundaries",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object) {
                str(object@boundaries, max.level = 2)
            }
)


## -----------------------------------------------------------------------------
## method to filter features (e.g., NegControl probes) from the @molecules slot.
## useful functions: group_by() group_indices(), group_rows(), tally(), ungroup()
#setMethod("filterFeatures",
#            signature = signature(object = "MoleculeExperiment"),
#            definition = function(object) {
##    # check that modified object is still a valid instance of ME class
##    validObject(x)
#            }
#)
#
## method to filter rows from the tibbles in the @molecules slot
## e.g., to filter out transcripts that are annotated as being in the nuclei
#setMethod("filterMoleculeData",
#            signature = signature(object = "MoleculeExperiment"),
#            definition = function(object) {
##    # check that modified object is still a valid instance of ME class
##    validObject(x)
#            }
#)
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
#setMethod("nTranscripts",
#          signature = signature(object = "MoleculeExperiment"),
#          definition = function(object) {
#
#          }
#)
#