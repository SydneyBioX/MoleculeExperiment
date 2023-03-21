# -----------------------------------------------------------------------------
# method to calculate total number of molecules identified for each unique
# features per sample
# -----------------------------------------------------------------------------

setMethod("nFeatures",
          signature = signature(object = "MoleculeExperiment"),
          definition = function(object) {
              # identify sample
              samples <- names(me@molecules)

              for(s in samples) {

              }

              length(me@molecules)


          }
)

# also add method for features()
