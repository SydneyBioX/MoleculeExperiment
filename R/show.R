# -----------------------------------------------------------------------------
# extend the show method to avoid plaguing the console with object contents
# -----------------------------------------------------------------------------

setMethod("show",
          signature = signature(object = "MoleculeExperiment"),
          definition = function(object) {
              # print summary message
              msg <- c(paste0("Object class: ", class(object)),
                       paste0("Slots: ", slotNames(object)))
              cat(msg, sep = "\n")
              cat("Sample IDs: ", names(object@molecules), sep = "\n")
              cat("\n")

              # print numbers of features per sample
              # use my own nFeatures method for this?
              #paste0("The molecules slot has: ", )

              # print some contents of molecules slot 
              cat("molecules slot contents: ", "\n")
              str(object@molecules, list.len = 2, no.list = T)

              # TODO print some contents of boundaries slot
          }
)

