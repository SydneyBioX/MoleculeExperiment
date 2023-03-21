# -----------------------------------------------------------------------------
# extend the show method to avoid plaguing the console with object contents
# give user a hint of the contents of the ME obj
# -----------------------------------------------------------------------------

# how we might want the MoleculeExperiment to display
# > me
# class: MoleculeExperiment
# 3 samples: Xenium....
# 541 features: Abbc....
# 60,000,000 molecules
# location range: [0,100] x [0,200] x [0,5]

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
              str(object@molecules, list.len = 2, no.list = T)

              # show total features
              # show x range y range 

              # TODO print some contents of boundaries slot
          }
)

