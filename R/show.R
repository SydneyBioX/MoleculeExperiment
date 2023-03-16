# GOAL: define show() method

setMethod("show",
          signature = signature(object = "custom_show"),
          function(obj) {
              msg <- c(
                "< Start Message >",
                paste0(" Contents: ", obj@message),
                "< End Message >"
                )
              cat(msg, sep = "\n")
          }
          )



#setMethod("show", "MoleculeExperiment", function(obj) {
#    cat(
#        # imagine MoleculeNumbers was a new getter method
#        "molecules has ", ncol(MoleculeNumbers(obj)), " rows\n",
#        sep=""
#    )
#})



