# ==============================================================================
# helper functions for main functions
# ==============================================================================

# ------------------------------------------------------------------------------
# check if object is of MoleculeExperiment class
.check_if_me <- function(object) {
    if (!is(object, "MoleculeExperiment")) {
        stop("Please specify a MoleculeExperiment object in the object
argument of this function.")
    }
}

# ------------------------------------------------------------------------------
# check arguments that should NOT be NULL
# use ... to specify the arguments to be checked for validity
.stop_if_null <- function(...) {
    # create list with parsed trees for the argument variables
    parsed_args <- substitute(list(...))

    # get vector of argument names as character strings
    # and ignore the call to the list component of the deparsed variable tree
    args_names <- unlist(lapply(parsed_args, deparse)[-1])

    # get values for the arguments
    args_values <- list(...)
    # set arg names for the values in the list
    names(args_values) <- args_names

    # show error message if one of the specified arguments is NULL
    for (arg in names(args_values)) {
        if (is.null(args_values[[arg]])) {
            stop(
                "The argument ", arg,
                " should not be NULL. Please enter a valid value for this
argument. See documentation for more information."
            )
        }
    }
}

# ------------------------------------------------------------------------------
# check arguments that, if NOT NULL, should be of class character
# use ... to specify the arguments to be checked for validity
.check_if_character <- function(...) {
    # create list with parsed trees for the argument variables
    parsed_args <- substitute(list(...))

    # get vector of argument names as character strings
    # and ignore the call to the list component of the deparsed variable tree
    args_names <- unlist(lapply(parsed_args, deparse)[-1])

    # get values for the arguments
    args_values <- list(...)
    # set arg names for the values in the list
    names(args_values) <- args_names

    # test each arg for its class
    for (arg in names(args_values)) {
        if (!is.null(args_values[[arg]])) {
            if (!is(args_values[[arg]], "character")) {
                stop(
                    "The argument ", arg,
                    " should be a character string"
                )
            }
        }
    }
}

# ------------------------------------------------------------------------------
# get essential columns
.get_essential_cols <- function(factor_col, x_col, y_col) {
    essential_cols <- list(
        factor_col,
        x_col,
        y_col
    )
    # check essential columns

    v <- unlist(lapply(essential_cols, is.null))

    if (isTRUE(any(v))) {
        stop("Essential columns have not been specified. Please specify column
names in the three \"col\" arguments to this function")
    }
    return(unlist(essential_cols))
}

# ------------------------------------------------------------------------------
# get standard columns depending on the slot
.get_standard_cols <- function(df_type) {
    if (df_type == "molecules") {
        standard_cols <- c(
            "feature_id",
            "x_location",
            "y_location"
        )
        return(standard_cols)
    } else if (df_type == "boundaries") {
        standard_cols <- c(
            "segment_id",
            "x_location",
            "y_location"
        )
        return(standard_cols)
    }
}

# ------------------------------------------------------------------------------
# standardise column names across technologies
.standardise_cols <- function(df,
                              standard_cols,
                              essential_cols) {
    if (!identical(essential_cols, standard_cols)) {
        # get index for essential cols
        for (col in essential_cols) {
            idx <- grep(col, colnames(df))
            # change colnames to standards
            colnames(df)[idx] <- standard_cols[which(essential_cols == col)]
        }
    }

    return(df)
}

# -----------------------------------------------------------------------------
# select specified columns
.select_cols <- function(df,
                         keep_cols,
                         standard_cols) {
    if (keep_cols == "essential") {
        cols <- standard_cols
    } else if (keep_cols == "all") {
        cols <- colnames(df)
    } else {
        cols <- keep_cols
        # check that essential columns have been selected too
        for (c in standard_cols) {
            if (!c %in% cols) {
                stop("Essential columns could not be identified in the
keepCols argument. Essential columns are those specified in the \"Col\"
arguments of this function.")
            }
        }
    }
    return(cols)
}

# -----------------------------------------------------------------------------
# scale location data from pixels to microns
# scale_factor is the micron size of one pixel
.scale_locations <- function(df, scale_factor) {
    df[["x_location"]] <- df[["x_location"]] * scale_factor
    df[["y_location"]] <- df[["y_location"]] * scale_factor
    return(df)
}

# -----------------------------------------------------------------------------
# standardise csv file
# use ... argument to pass column by which to factor df

#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
.standardise_to_list <- function(df, cols, ...) {
    df %<>%
        dplyr::select(dplyr::all_of(cols)) %>%
        dplyr::group_by(dplyr::pick(...))

    ls <- df %>%
        dplyr::group_split(.keep = FALSE) %>%
        purrr::set_names(unlist(dplyr::group_keys(df))) %>%
        as.list()

    return(ls)
}

# -----------------------------------------------------------------------------
# get sample IDs by retrieving name of parent directory
# TODO: does this need n_samples? length(f_paths) is n_samples
.get_sample_id <- function(n_samples, f_paths) {
    ids <- vector("character", length = n_samples)
    # TODO: filter out empty strings
    for (f in seq_along(f_paths)) {
        id <- base::strsplit(f_paths[[f]], "/") %>%
            unlist(use.names = FALSE) %>%
            Filter(function(x) x != "", .) %>%
            utils::tail(2) %>%
            utils::head(1)

        ids <- replace(ids, f, values = id)
    }
    return(ids)
}

# ------------------------------------------------------------------------------
# flatten ME list whilst retaining information of list headers
.add_col_to_nested_list_and_flatten <- function(list_obj, column_name = NULL) {
    list_obj_added <- mapply(function(df, nm) {
        df[, column_name] <- nm
        return(df)
    }, list_obj, names(list_obj), SIMPLIFY = FALSE)
    return(do.call(rbind, list_obj_added))
}

.flatten_molecules <- function(object, assay_name) {
    molecules_flat <- .add_col_to_nested_list_and_flatten(
        lapply(
            molecules(object, assayName = assay_name),
            function(mol_2) {
                .add_col_to_nested_list_and_flatten(
                    lapply(mol_2, function(mol_1) {
                        .add_col_to_nested_list_and_flatten(
                            mol_1, "feature_id"
                        )
                    }), "sample_id"
                )
            }
        ),
        column_name = NULL
    )
    return(molecules_flat)
}

.flatten_boundaries <- function(object, assay_name) {
    boundaries_flat <- .add_col_to_nested_list_and_flatten(
        lapply(
            boundaries(object, assayName = assay_name),
            function(mol_2) {
                .add_col_to_nested_list_and_flatten(
                    lapply(mol_2, function(mol_1) {
                        .add_col_to_nested_list_and_flatten(
                            mol_1, "segment_id"
                        )
                    }), "sample_id"
                )
            }
        ),
        column_name = NULL
    )
    return(boundaries_flat)
}

# ------------------------------------------------------------------------------
# add buffer to entry within an existing boundaries slot
.add_buffer_boundary <- function(x, buffer = 0) {
    bds_mat <- as.matrix(cbind(1, x[, c("x_location", "y_location")]))
    colnames(bds_mat) <- c("factors_int", "x", "y")
    bds <- terra::vect(bds_mat, type = "polygons")
    bds_exp <- terra::buffer(bds, width = buffer)
    bds_mat_exp <- terra::as.data.frame(terra::geom(bds_exp))
    new_x <- bds_mat_exp[, c("x", "y")]
    colnames(new_x) <- c("x_location", "y_location")
    return(dplyr::tibble(new_x))
}

# ------------------------------------------------------------------------------
#' Utility function to generate BPPARAM object.
#'
#' @param cores Desired number of cores for BPPARAM object.
#' @return A BPPPARAM object.
#' @importFrom BiocParallel SerialParam SnowParam MulticoreParam bpparam
.generateBPParam <- function(cores = 1) {
  seed <- .Random.seed[1]
  if (cores == 1) {
    BPparam <- BiocParallel::SerialParam(RNGseed = seed)
  } else { ## Parallel processing is desired.
    ## Also set the BPparam RNGseed if the user ran set.seed(someNumber) themselves.
    if (Sys.info()["sysname"] == "Windows") { # Only SnowParam suits Windows.
      BPparam <- BiocParallel::SnowParam(min(
        cores,
        BiocParallel::snowWorkers("SOCK")
      ),
      RNGseed = seed
      )
    } else if (Sys.info()["sysname"] %in% c("MacOS", "Linux")) {
      BPparam <- BiocParallel::MulticoreParam(min(
        cores,
        BiocParallel::multicoreWorkers()
      ),
      RNGseed = seed
      )
      ## Multicore is faster than SNOW, but it doesn't work on Windows.
    } else { ## Something weird.
      BPparam <- BiocParallel::bpparam() ## BiocParallel will figure it out.
    }
  }
  BPparam
}
