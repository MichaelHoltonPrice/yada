#' @title
#' Parse a category specification
#'
#' @description
#' The input is a category specification such as "2 1 {3 4} -4". The output is
#' a list with named elements that maps the original categories (which are
#' interpreted as string variables onto sequential integers that run from 0 to
#' M, where there are M+1 categories. The curly brackets indicate that
#' categories "3" and "4" should be collapsed together to form a single, final
#' category. For the category specification ([cat_spec]) "2 1 {3 4} -4", the
#' mapping between original and final categories is:
#'
#' "2"  -> 0
#' "1"  -> 1
#' "3"  -> 2
#' "4"  -> 2
#' "-4" -> 3
#'
#' Note how the string variable "-4" is the final category, even though it would
#' be the first variable were it interpreted as the number -4.
#'
#' @param cat_spec The category specification (e.g., "2 1 {3 4} -4")
#'
#' @returns A list that maps the original string categories onto final integer
#' categories
#'
#' @examples
#'
#' # Call parse_cat_spec to create the category mapping
#' cat_map <- yada::parse_cat_spec("2 1 {3 4} -4")
#' print(cat_map)
#'
#' @export
parse_cat_spec <- function(cat_spec) {
  # The input category specification, cat_spec, is something like:
  # "2 1 {3 4} -4"

  # Make sure that the category names are all unique by removing all curly
  # brackets and ensuring that the resulting tokens (strings separated by
  # commas) are unique.
  # Save the input category specification as cat_spec0
  cat_spec0 <- cat_spec
  cat_spec <- stringr::str_replace_all(cat_spec,"\\{","")
  cat_spec <- stringr::str_replace_all(cat_spec,"\\}","")
  tokens <- strsplit(cat_spec," ")[[1]]

  if (length(tokens) != length(unique(tokens))) {
    stop("The category names are not unique")
  }

  # Set cat_spec back to its original value
  cat_spec <- cat_spec0

  # Locate any groups separated out by curly brackets and replace commas inside
  # such groups with semicolons. For the preceding example, this yields:
  # "2 1 {3,4} -4"
  matches <- stringr::str_locate_all(cat_spec, "(?<=\\{).+?(?=\\})")[[1]]
  if (nrow(matches) > 0) {
    for (r in 1:nrow(matches)) {
      # Substitute " " with "," inside curly brackets
      ind_start <- matches[r,"start"]
      ind_end   <- matches[r,"end"]
      new_sub_str <- substr(cat_spec,ind_start,ind_end)
      new_sub_str <- stringr::str_replace_all(new_sub_str," ",",")
      # Update cat_spec using new_sub_string
      cat_spec <- stringr::str_c(substr(cat_spec,1,ind_start-1),
                                 new_sub_str,
                                 substr(cat_spec,ind_end+1,nchar(cat_spec)))
    }
  }

  # Eliminate the curly brackets, which are now redundant since two distinct
  # separators are being used. For the preceding example, this yields:
  # "2 1 3,4 -4"
  cat_spec <- stringr::str_replace_all(cat_spec,"\\{","")
  cat_spec <- stringr::str_replace_all(cat_spec,"\\}","")

  # Call strsplit to create tokens based on a comma separator. For the preceding
  # example, tokens is a list consisting of:
  # "2", "1", "3,4" "-4"
  tokens <- strsplit(cat_spec," ")[[1]]


  # Iterate over tokens to populate the output list. M equals the number of
  # tokens minus 1
  cat_map <- list()
  M <- length(tokens) - 1

  for(m in 0:M) {
    # If tokens[m+1] is "2", sub_tokens is "2".
    # If tokens[m+1] is "3,4", sub_tokens is c("3","4").
    sub_tokens <- strsplit(tokens[m+1],",")[[1]]
    for(n in 1:length(sub_tokens)) {
      orig_cat <- sub_tokens[n]
      cat_map[[orig_cat]] <- m
    }
  }

  # For the preceding example, print(cat_map) yields:
  # $`2`
  # [1] 0
  #
  # $`1`
  # [1] 1
  #
  # $`3`
  # [1] 2
  #
  # $`4`
  # [1] 2
  #
  # $`-4`
  # [1] 3

  return(cat_map)
}

#' @title
#' Apply a category specification to an input vector of original string
#' categories
#'
#' @description
#' v_str is a vector of original string categories. cat_spec is a cateogry
#' specification (see [parse_cat_spec]). Apply the category specification to
#' each element of v_str to yield a new vector v_int, which gives the integer
#' category values.
#'
#' @param v_str The vector of original string categories
#' @param cat_spec The category specification
#' 
#' @examples 
#' # Call apply_cat_spec to assign category specification to trait string
#' v_str <- c("a",NA,"d","b","c",NA,"d","a")  # vector of string categories
#' cat_spec <- "a c d b"  # order of categories
#' 
#'# Expected application: a=0, c=1, d=2, b=3
#'# Therefore, expected output: c(0,NA,2,3,1,NA,2,0)
#'v_int <- apply_cat_spec(v_str, cat_spec)
#'print(v_int)
#'
#' @returns The vector of new, integer categories
#'
#' @export
apply_cat_spec <- function(v_str,cat_spec) {

  # Throw an error if the input is a vector of factors
  if (is.factor(v_str)) {
    stop("v_str should be a vector of strings, not factors")
  }

  cat_map <- parse_cat_spec(cat_spec)
  N <- length(v_str)
  v_int <- rep(NA,N)
  for (n in 1:N) {
    str_cat <- v_str[n]
    if (!is.na(str_cat)) {
      # Check that this category is in cat_map
      if (!(str_cat %in% names(cat_map))) {
        stop(paste0(str_cat," not found in category mapping"))
      }
      v_int[n] <- cat_map[[which(names(cat_map)==str_cat)]]
    }
  }
  return(v_int)
}

#' @title
#' Build a left/right variable from the base variable, side label, and side
#' location
#'
#' @description
#' Given a base variable (base_var), side label (side_lab), and side location
#' (side_loc), build the full left/right variable. Two examples follow.
#'
#' (1) base_var = "var1"
#'     side_lab = "_L"
#'     side_loc = "end"
#'     ---------------
#'     lr_var   = "var1_L"             [the output]
#'
#' (2) base_var = "variable_two"
#'     side_lab = "right_"
#'     side_loc = "start"
#'     ---------------
#'     lr_var   = "right_variable_two" [the output]
#'
#' @param base_var The base variable name, a string
#' @param side_loc The side location, a string ("start" or "end")
#'   approach)
#' @param side_lab The side label, a string (prefix or affix)
#' 
#' @examples 
#' # Call build_lr_var to specify left/right variable names
#' left_var <- build_lr_var("variable_name","start","L_")
#' print(left_var)  # Expected output: "L_variable_name"
#' 
#' right_var <- build_lr_var("var_name","end","_right")
#' print(right_var)  # Expected output: "var_name_right"
#'
#' @return The resulting left/right variable
#'
#' @export
build_lr_var <- function(base_var,
                         side_loc,
                         side_lab) {
  # Check that side_loc is "start" or "end"
  if (!(side_loc %in% c("start","end"))) {
    stop(paste0("side_loc = '"),side_loc,"', but should be 'start' or 'end'")
  }
  if (side_loc == "start") {
    lr_var <- paste0(side_lab,base_var)
  } else {
    lr_var <- paste0(base_var,side_lab)
  }
  return(lr_var)
}

#' @title
#' Merge a variable that has distinct left and right measurements
#'
#' @description
#' The input is a data frame with a variable that has left and right aspects.
#' For example, the input data frame might contain the left and right
#' variables man_I1_L and man_I1_R; the output data frame would contain a
#' single, merged variable man_I1. The approach for merging variables must be
#' input. There are five allowed approaches: "left", "right", "mean", "lowest",
#' and "highest". "mean" should not be used for ordinal variables. For "left",
#' the value of the left variable is used; if the left variable is NA for an
#' observation and the right variable is not NA, the right variable is used.
#' For "right", identical choices are made, except that right is preferred to
#' left when available. For mean, the mean of the two values is used. For
#' "lowest", the minimum value is used. For "highest", the maximum value is
#' used.
#'
#' Together, the three variables base_var, side_loc, and side_labels specify how
#' the base variable (e.g., "man_I1") and left / right variables (e.g.,
#' "man_I1_L" / "man_I1_R") map onto each other. side_labels is a vector of
#' length two where the first element gives the label used to distingush the
#' left variable and the second elements gives the label used to distingush the
#' right variable. side_loc must be either "start" or "end". If side_loc is
#' "start", the left / right variables are created from the base variable by
#' prefixing the two values in side_labels; if side_loc is "end", the
#' left / right variables are created from the base variable by affixing the two
#' values in side_labels. For example, if [base_ar = "man_I1],
#' [side_labels = c("_L","_R")], and [side_loc = "end"], the left variable is
#' [man_I1_L] and the right variable is [man_I1_R]. Conversely, if
#' [side_labels = c("L_","R_")] and [side_loc = "start"], the left variable is
#' [L_man_I1] and the right variable is [R_man_I1].
#'
#' @param input_df Original data frame to be manipulated.
#' @param base_var The base variable name to be merged
#' @param side_loc Character string identifying if side labels are at the
#'   "start" or "end" of the base variable name
#' @param side_labels Character string of left and right labels (in that order;
#'   ex.: c('_L','_R')).
#' @param approach The merging approach to take (must be one of: "left",
#'   "right", "mean", "lowest", and "highest")
#'
#' @examples 
#' # Call merge_lr_var to combine left and right variable columns per
#' # user-specified approach
#' ex_df <- data.frame(ord_var_L = c(0,NA,1,NA,2,NA),
#'                     ord_var_R = c(2,1,0,NA,2,1),
#'                     L_cont_var = c(10.5,NA,14.0,NA,8.7,NA),
#'                     R_cont_var = c(12.5,3.2,7.0,NA,8.7,1.0))
#' first_merge <- merge_lr_var(input_df = ex_df,
#'                             base_var = "ord_var",
#'                             side_loc = "end",
#'                             side_labels = c("_L","_R"),
#'                             approach = "highest")
#' print(first_merge)
#' 
#' second_merge <- merge_lr_var(input_df = second_merge,
#'                              base_var = "cont_var",
#'                              side_loc = "start",
#'                              side_labels = c("L_","R_"),
#'                              approach = "mean")
#' print(second_merge)
#'
#' @return The merged data frame
#'
#' @export
merge_lr_var <- function(input_df,
                         base_var,
                         side_loc,
                         side_labels,
                         approach) {

  if (!(approach %in% c("left","right","mean","lowest","highest"))) {
    stop("Invalid approach specified. See documentation for merge_lr_var.")
  }

  if (length(side_labels) != 2) {
    stop("side_labels should be length 2")
  }

  if (side_labels[1] == side_labels[2]) {
    stop("side_labels[1] should not equal side_labels[2]")
  }

  if( !(side_loc %in% c("start", "end"))) {
    stop("side_loc must be either 'start' or 'end'")
  }

  # Build the left and right variable and make sure that each has a
  # corresponding column in input_df
  left_var <- build_lr_var(base_var, side_loc, side_labels[1])
  right_var <- build_lr_var(base_var, side_loc, side_labels[2])

  if ( !(left_var %in% names(input_df))) {
    stop("Left variable is not in input dataframe")
  }

  if ( !(right_var %in% names(input_df))) {
    stop("Right variable is not in input dataframe")
  }

  # Set the output data equal to the input data frame, then remove the left and
  # right variables from the output data frame.
  output_df <- input_df

  output_df[[ left_var]] <- NULL
  output_df[[right_var]] <- NULL

  # Iterate over rows to apply the approach. The results are stored in a new
  # vector, new_values, and added below to the data frame
  new_values <- c()
  for (r in 1:nrow(input_df)) {
    left_value  <- input_df[r, left_var]
    right_value <- input_df[r, right_var]
    if (approach == "left") {
      if (!is.na(left_value)) {
        new_values <- c(new_values,left_value)
      } else {
        new_values <- c(new_values,right_value)
      }
    } else if(approach == "right") {
       if (!is.na(right_value)) {
        new_values <- c(new_values,right_value)
      } else {
        new_values <- c(new_values,left_value)
      }
    } else if(approach == "mean") {
      if (is.na(left_value) && is.na(right_value)) {
        new_values <- c(new_values,NA)
      } else {
        new_values <- c(new_values,mean(c(left_value,right_value),na.rm=T))
      }
    } else if(approach == "lowest") {
      if (is.na(left_value) && is.na(right_value)) {
        new_values <- c(new_values,NA)
      } else {
        new_values <- c(new_values,min(c(left_value,right_value),na.rm=T))
      }
    } else {
      # approach = "highest"
      if (is.na(left_value) && is.na(right_value)) {
        new_values <- c(new_values,NA)
      } else {
        new_values <- c(new_values,max(c(left_value,right_value),na.rm=T))
      }
    }
  }

  # Add the new, merged column where the first of the original left / right
  # columns is located

  # First, add the column at the end
  output_df[base_var] <- new_values

  # Next, reorder the columns
  ind_left  <- which(names(input_df) == left_var)
  ind_right <- which(names(input_df) == right_var)
  ind_new <- min(ind_left,ind_right)

  num_var <- ncol(output_df)

  if (ind_new == 1) {
    # Place at beginning
    col_reorder <- c(num_var,1:(num_var-1))
    output_df <- output_df[, col_reorder]
  } else if (ind_new < num_var) {
    # Not at beginning or end
    col_reorder <- c(1:(ind_new-1),num_var,ind_new:(num_var-1))
    output_df <- output_df[, col_reorder]
  }

  return(output_df)
}

#' @title
#' Merge multiple variables that have distinct left and right measurements
#'
#' @description
#' The input is a data frame with multiple variables that have left and right
#' aspects (to merge a single variable, use merge_lr_var). Call merge_lr_var
#' for each variable to merge it. The input variables approach and base_var
#' should be vectors of the same length (the number of variables to merge).
#' sideLabels and sideLoc are as in [merge_lr_var], but are assumed to be the
#' same for all variables (consequently, it is not possible to mix prefixing,
#' "left_man_I1" and affixing, "man_I1_L"). Aside from these requirements,
#' approach, baseVar, sideLabels, and sideLoc behave identically in
#' [merge_multiple_lr_var] and [merge_lr_var].
#'
#' @param input_df Original data frame to be manipulated.
#' @param base_var The base variables to be merged (a vector the same length as
#'   approach)
#' @param side_loc Character string identifying if side labels are at the
#'   "start" or "end" of the base variable name
#' @param side_labels Character string of left and right labels (in that order;
#'   ex.: c('_L','_R')).
#' @param approach The merging approaches to take (a vector the same length as
#'   baseVar)
#' 
#' @examples 
#' # Call merge_multiple_lr_var to combine left and right variables per
#' # user-specified approach
#' ex_df <- data.frame(ord_var_L = c(0,NA,1,NA,2,NA),
#'                     ord_var_R = c(2,1,0,NA,2,1),
#'                     cont_var_L = c(10.5,NA,14.0,NA,8.7,NA),
#'                     cont_var_R = c(12.5,3.2,7.0,NA,8.7,1.0))
#' 
#' merged_df <- merge_multiple_lr_var(input_df = ex_df,
#'                                    base_var = c("ord_var","cont_var"),
#'                                    side_loc = "end",
#'                                    side_labels = c("_L","_R"),
#'                                    approach = c("lowest","left"))
#' print(merged_df)
#'
#' @return The merged data frame
#'
#' @export
merge_multiple_lr_var <- function(input_df,
                                  base_var,
                                  side_loc,
                                  side_labels,
                                  approach) {


  if (length(approach) != length(base_var)) {
    stop("approach and base_var should have the same length")
  }

  # Iterate over variables to merge them
  output_df <- input_df
  for (n in 1:length(approach)) {
    output_df <- merge_lr_var(output_df,
                              base_var[n],
                              side_loc,
                              side_labels,
                              approach[n])
  }

  return(output_df)
}

#' @title
#' Load variable information from file
#'
#' @description
#' The input is the path to the file with variable information, which must be a
#' .csv file with the following seven named columns:
#'
#' Column Name    Description
#' Variable       The variable name
#' Type           The type of variable (x, ordinal, continuous, or other)
#' Categories     A category specification (see [parse_cat_spec] for each
#'                ordinal variable
#' Missing_Value        Value that represents missing data for that 
#'                      variable. Multiple values are separated by commas. 
#'                      Ex: -1,99             
#' Left_Right_Side      The side (prefix/affix) on which the label is appended
#'                      to distinguish left/right variables
#' Left_Label           The label that marks left variables
#' Right_Label          The label that marks right variables
#' Left_Right_Approach  The approach to use for merging left/right variables.
#'
#' The first four columns are required, whereas the final four columns are
#' optional (since they need only be used if there exist left/right variables
#' that should be merged). Further details on each column are provided next.
#'
#' Details on Variable: The variable names must be unique. An error is thrown if
#' they are not. Each variable must match a column name in the corresponding
#' data file (see [reformat_mcp_data]).
#'
#' Details on Type: There should be exactly one variable specified as type x
#' (for age estimation, x is age). Response variables (which comprise the matrix
#' Y) must be either ordinal or continuous. Additional variables that should be
#' kept in the returned data frame should be marked as other (likely, these are
#' covariates). Variables that are not specified in the variable information
#' file but are in the data file are ignored in [reformat_mcp_data].
#'
#' Details on Categories: Each ordinal variable must explicitly define the
#' allowable categories and their order in the format expected by
#' [parse_cat_spec].
#'
#' Details on Left_Right_Side, Left_Label, Right_Label, and Left_Right_Approach:
#' Optionally, variables may be specified as left/right variables with the
#' approach described in [merge_lr_var]. For such variables, the variable name
#' given in the Variable column is the base variable, and the remaining columns
#' specify the side, left label, right label, and merging approach.
#'
#' @param var_info_file The file with variable information
#' 
#' @examples
#' # Call load_var_info to read in variable specifications
#' # This example uses raw data included with the package 'yada'
#' var_info_file <- syanalysis_name.file("extdata","US_var_info.csv",package="yada")
#' print(var_info_file)
#' 
#' var_info <- load_var_info(var_info_file)  # load file using file path
#' head(var_info)
#' # The following column names MUST be present in var_info_file:
#' # Variable, Type, Categories, Missing_Value
#'
#' @return A data frame containing the cumulative probit data
#'
#' @export
load_var_info <- function(var_info_file) {
  # TODO: check that @examples works (and the same for reformat_mcp_data)
  var_info <- read.csv(var_info_file,
                       colClasses="character")


  if (!(ncol(var_info) %in% c(4,8))) {
    stop(paste0("The variable info data frame should have exactly 4 or 8 ",
                "columns, not ",ncol(var_info)))
  }

  if (ncol(var_info) == 4) {
    if (!all(names(var_info) == c("Variable",
                                  "Type",
                                  "Categories",
                                  "Missing_Value"))) {
      stop("Columns have the wrong names (see function documentation)")
    }
  } else {
    if (!all(names(var_info) == c("Variable",
                                  "Type",
                                  "Categories",
                                  "Missing_Value",
                                  "Left_Right_Side",
                                  "Left_Label",
                                  "Right_Label",
                                  "Left_Right_Approach"))) {
      stop("Columns have the wrong names (see function documentation)")
    }
  }

  # Check that there is exactly one x variable
  if (sum(var_info$Type == "x") == 0) {
    stop("No x variable is specified")
  }
  if (sum(var_info$Type == "x") > 1) {
    stop("More than one x variable is specified")
  }

  # Check that only valid variables types are used
  for (r in 1:nrow(var_info)) {
    var_type <- var_info[r,"Type"]
    if (!(var_type %in% c("x","ordinal","continuous","other"))) {
      stop(paste0("Invalid variable type, ",var_type,", used in row ",r))
    }
  }

  # Check that each ordinal variable has a valid category specification
  for (r in 1:nrow(var_info)) {
    var_type <- var_info[r,"Type"]
    if (var_type == "ordinal") {
      cat_spec <- var_info[r,"Categories"]
      if (cat_spec == "") {
        stop("A category specification must be given for each ordinal variable")
      }

      result <- try(parse_cat_spec(cat_spec),silent=TRUE)
      if (class(result) == "try-error") {
        stop(paste0("Error parsing cat_spec = '",cat_spec,"' for row ",r))
      }
    }
  }

  # TODO: consider adding further error checking for left/right variables

  return(var_info)
}

#' @title
#' Build a complete vector of variable names for the input data frame, var_info
#' (for details on var_info, see [load_var_info]
#'
#' @param var_info The variable information data frame
#' 
#' @examples 
#' # Call build_var_names to generate expected column names in data
#' var_info_file <- syanalysis_name.file("extdata","US_var_info.csv",package="yada")
#' print(var_info_file)
#' 
#' var_info <- load_var_info(var_info_file)  # load file using file path
#' head(var_info)
#' 
#' var_names <- build_var_names(var_info)
#' var_names[5:16]
#'
#' @return A list of all variable names specified by var_info, accounting for
#'   left/right variables
#'
#' @export
build_var_names <- function(var_info) {
  var_names <- c()

  for (r in 1:nrow(var_info)) {
    if (var_info[r,"Left_Right_Side"] != "") {

    var_names <- c(var_names,
                   build_lr_var(var_info[r,"Variable"],
                                var_info[r,"Left_Right_Side"],
                                var_info[r,"Left_Label"]))
    var_names <- c(var_names,
                     build_lr_var(var_info[r,"Variable"],
                                  var_info[r,"Left_Right_Side"],
                                  var_info[r,"Right_Label"]))
    } else {
      var_names <- c(var_names,var_info[r,"Variable"])
    }
  }
  return(var_names)
}

#' @title
#' Parse missing value information from var_info
#' 
#' @description 
#' The input is the Missing_Values column in var_info, which must have a length
#' equal to the number of columns in the data file. This function is for use in
#' [reformat_mcp_data] to ensure only values expected for each trait are available.
#' 
#' @param missing_values A string containing values that should be recoded as NA
#' separated by a comma
#' 
#' @examples 
#' # Call parse_NA_values to return a vector of individual proxies for NA
#' na_values <- parse_NA_values("-1,99,NA")
#' print(na_values)
#' 
#' @return A vector with the values that represent NA
#' 
#' @export
parse_NA_values <- function(missing_values) {
  # The input missing_values is something like:
  # "NA,-1" where each value to be replaced with NA is separated by a comma
  na_values <- strsplit(missing_values,',')[[1]]
  
  return(na_values)
}

#' @title
#' Load cumulative probit data from file
#'
#' @description
#' Two inputs are needed, the path to the data file (data_file) and a data frame
#' with information on variables to use (var_info; see [load_var_info]).
#'
#' Only variables with entries in var_info are kept (accounting for the
#' possibility that some are left/right variables).
#'
#' @param data_file_path The .csv data file path
#' @param var_info The data frame with variable information (see
#'   [load_var_info])
#' @param save_file Logical whether to save the resulting data frame as 
#'   "*_reformatted.csv" in the data_file_path
#' 
#'
#' @return A data frame containing reformatted data for the mcp algorithm
#'
#' @export
reformat_mcp_data <- function(data_file_path, var_info, save_file=F) {
  # Read the "raw" data frame from file, with all columns being read as strings
  raw_cp_df <- read.csv(data_file_path, colClasses="character")
  
  # Check that each variable in var_info has a corresponding column in
  # raw_cp_df
  var_names <- build_var_names(var_info)
  for(var_name in var_names) {
    if (!(var_name %in% names(raw_cp_df))) {
      stop(paste0(var_name," is not a column in the data file"))
    }
  }
  
  # Create the output data frame, cp_df. Remove any columns in cp_df that are
  # not in var_names
  cp_df <- raw_cp_df
  for (orig_var_name in names(cp_df)) {
    if (!(orig_var_name %in% var_names)) {
      cp_df[, orig_var_name] <- NULL
    }
  }
  
  # If necessary, set to NA any elements of cp_df that are in the optional
  # column 'Missing_value' in var_info
  if(length(grep('Missing_Value',names(var_info))) > 0) {
    for(vv in var_info$Variable) {
      var_columns <- grep(vv, colnames(cp_df))
      na_values <-
        parse_NA_values(var_info$Missing_Value[var_info$Variable==vv])
      for(cc in var_columns) {
        for(r in 1:nrow(cp_df)) {
          if(cp_df[r,cc] %in% na_values) {
            cp_df[r, cc] <- NA
          }
        }
      }
    }
  }
  
  # Apply category specifications for each ordinal variable
  for (r in 1:nrow(var_info)) {
    base_var <- var_info[r,"Variable"]
    if (var_info[r,"Type"] == "ordinal") {
      cat_spec <- var_info[r,"Categories"]
      
      # Is this a left/right variable?
      is_lr <- var_info[r,"Left_Right_Side"] != ""
      
      if (is_lr) {
        left_var <- build_lr_var(base_var,
                                 var_info[r,"Left_Right_Side"],
                                 var_info[r,"Left_Label"])
        cp_df[, left_var] <- apply_cat_spec(cp_df[, left_var], cat_spec)
        right_var <- build_lr_var(base_var,
                                  var_info[r,"Left_Right_Side"],
                                  var_info[r,"Right_Label"])
        cp_df[, right_var] <- apply_cat_spec(cp_df[, right_var], cat_spec)
      } else {
        # Not a left right variable
        cp_df[, base_var] <- apply_cat_spec(cp_df[, base_var], cat_spec)
      }
    } else if(var_info[r,"Type"] == "x") {
      # x must be numeric
      cp_df[, base_var] <- as.numeric(cp_df[, base_var])
    } else if(var_info[r,"Type"] %in% c("x","continuous")) {
      # continuous variables must be numeric
      
      # Is this a left/right variable?
      is_lr <- var_info[r,"Left_Right_Side"] != ""
      
      if (is_lr) {
        left_var <- build_lr_var(base_var,
                                 var_info[r,"Left_Right_Side"],
                                 var_info[r,"Left_Label"])
        cp_df[, left_var] <- as.numeric(cp_df[, left_var])
        right_var <- build_lr_var(base_var,
                                  var_info[r,"Left_Right_Side"],
                                  var_info[r,"Right_Label"])
        cp_df[, right_var] <- as.numeric(cp_df[, right_var])
      } else {
        # Not a left right variable
        cp_df[, base_var] <- as.numeric(cp_df[, base_var])
      }
    }
  }
  
  # Merge left/right variables (as necessary). In the process, update variable
  # names and variable types
  var_name_vect <- c()
  base_var_vect <- c()
  var_type_vect <- c()
  for (r in 1:nrow(var_info)) {
    base_var <- var_info[r,"Variable"]
    if (var_info[r,"Type"] %in% c("ordinal","continuous")) {
      # If this is not a left/right variable, just append to var_name and
      # var_type
      if (var_info[r,"Left_Right_Side"] == "") {
        var_name_vect <- c(var_name_vect,base_var)
        var_type_vect <- c(var_type_vect,var_info[r,"Type"])
        base_var_vect <- c(base_var_vect,base_var)
      } else {
        approach <- var_info[r,"Left_Right_Approach"]
        if (approach != "") {
          var_name_vect <- c(var_name_vect,base_var)
          var_type_vect <- c(var_type_vect,var_info[r,"Type"])
          base_var_vect <- c(base_var_vect,base_var)
          side_loc <- var_info[r,"Left_Right_Side"]
          side_labels <- c(var_info[r,"Left_Label"],
                           var_info[r,"Right_Label"])
          cp_df <- merge_lr_var(cp_df,
                                base_var,
                                side_loc,
                                side_labels,
                                approach)
          
        } else {
          # This is a left/right variable that is not being merged
          left_var <- build_lr_var(base_var,
                                   var_info[r,"Left_Right_Side"],
                                   var_info[r,"Left_Label"])
          var_name_vect <- c(var_name_vect,left_var)
          var_type_vect <- c(var_type_vect,var_info[r,"Type"])
          base_var_vect <- c(base_var_vect,base_var)
          right_var <- build_lr_var(base_var,
                                    var_info[r,"Left_Right_Side"],
                                    var_info[r,"Right_Label"])
          var_name_vect <- c(var_name_vect,right_var)
          var_type_vect <- c(var_type_vect,var_info[r,"Type"])
          base_var_vect <- c(base_var_vect,base_var)
        }
      }
    }
  }
  
  if (save_file) {
    new_data_file <- paste0(gsub(".csv","",data_file_path),"_reformatted.csv")
    write.csv(cp_df, new_data_file, row.names=F)
  }
  
  return(cp_df)
}


#' @title Generate the main problem file
#' 
#' @description The main problem file is a list containing the training data 
#'   for the mcp algorithm, as well as model specifications ("mod_spec") 
#'   including the number of ordinal (J) and continuous (K) variables and 
#'   the number of breaks between ordinal stages (M).
#' 
#' @param data_file Data frame containing the training data
#' @param var_info The var_info file loaded via [load_var_info()]
#' 
#' @return A list containing the response variable (x) as a vector, 
#'   the predictor variables as a matrix (Y), and another list (mod_spec) 
#'   containing the model specifications.
#' 
#' @export

generate_main_problem <- function(data_file, var_info) {
  # Initialize data_file as cp_df
  cp_df <- data_file
  
  # Create a problem variable in the format expected by the optimization
  # functions
  var_name_x <- var_info[which(var_info$Type == "x"),"Variable"]
  ord_cont_idx <- which(var_info$Type %in% c("ordinal","continuous"))
  var_type_vect <- var_info$Type[ord_cont_idx]
  var_name_vect <- var_info$Variable[ord_cont_idx]
  # base_var_vect <- colnames(data_file)[ord_cont_idx]
  x <- cp_df[,var_name_x]
  J <- sum(var_type_vect == "ordinal")
  K <- sum(var_type_vect == "continuous")
  
  ind_ord  <- which(var_type_vect == "ordinal")
  ind_cont <- which(var_type_vect == "continuous")
  N <- length(x)
  Y <- matrix(NA,J+K,N)
  M <- rep(NA,J)
  
  # Populate Y and M for ordinal variables
  if(length(ind_ord) > 0) {
    for (j in 1:length(ind_ord)) {
      var_j <- var_name_vect[ind_ord[j]]
      Y[j,] <- cp_df[,var_j]
      ind_vi <- which(var_name_vect[ind_ord[j]] == var_info$Variable)
      cat_spec <- var_info[ind_vi,"Categories"]
      cat_map <- parse_cat_spec(cat_spec)
      M[j] <- length(unique(cat_map)) - 1
    }
  }
  
  if(length(ind_cont) > 0) {
    for (k in 1:length(ind_cont)) {
      var_k <- var_name_vect[ind_cont[k]]
      Y[J+k,] <- cp_df[,var_k]
    }
  }
  
  # Check for x NA in problem
  if(any(is.na(x))) {
    stop(paste0(var_name_x," cannot have missing values"))
  }
  
  # Check for fully missing columns (individual trait info) in problem
  allNAs <- which(colSums(is.na(Y),na.rm=TRUE)==nrow(Y))  
  if(length(allNAs) > 0) {
    x <- x[-allNAs]  # remove individuals with fully missing traits
    Y <- Y[,-allNAs]  # remove columns (individuals) with fully missing traits
  }
  
  # Generate M
  problem <- list(x=x,
                  Y=Y,
                  var_names=c(var_name_vect[ind_ord],var_name_vect[ind_cont]),
                  mod_spec=list(J=J,K=K,M=M))
}


#' @title A wrapper to generate the cross-validation problems
#'
#' @description A wrapper to generate the cross-validation problems
#'
#' @param main_problem The main problem
#' @param K Number of cross-validation folds
#' @param seed Setting a seed to allow for reproducibility, which is stored
#'   within each problem fold. NA generates a randomly-selected seed between 1 and 1E6.
#'
#' @return A list with test and training data sets
#'
#' @export
generate_cv_problems <- function(main_problem,K,seed=NA) {
  problem0 <- main_problem
  N <- length(problem0$x)
  if(is.na(seed)){
    seed <- trunc(runif(1, min=1, max=1E6))
  }
  set.seed(seed)
  folds <- nestfs::create.folds(K,N)
  
  # Iterate over folds to generate the test sets in test_list and the training
  # sets in train_list
  test_list <- list()
  train_list <- list()
  for(ff in 1:length(folds)) {
    # train is identical to problem, but for this fold's training data only
    train <- list()
    # Test data
    test <- list()
    
    train <- list(x        = problem0$x[-folds[[ff]]],
                  var_names = problem0$var_names)
    
    test  <- list(x         = problem0$x[folds[[ff]]],
                  var_names = problem0$var_names)
    
    # Y and mod_spec may need to be adjusted for any ordinal variable with a
    # reduced number of categories in the training data. This would also have to
    # be accounted for in the test data since some categories in the test data
    # do not have a unique mapping to collapsed categories in the training data.
    # If a remapping is needed, it is done below.
    
    # First create Y_train and mod_spec by iterating variables, collapsing
    # categories as necessary
    N_test <- length(folds[[ff]])
    N_train   <- N - N_test
    num_var <- length(problem0$var_names)
    
    Y_train0 <- problem0$Y[,-folds[[ff]]]
    Y_test0  <- problem0$Y[, folds[[ff]]]
    
    Y_train  <- matrix(NA,num_var,N_train)
    Y_test   <- matrix(NA,num_var,N_test)
    # mod_spec$M must be updated if categories are collapsed
    mod_spec     <- problem0$mod_spec
    
    # Iterate over ordinal variables to see if any categories are missing for
    # this subset
    for(vv in 1:num_var) {
      if(vv <= problem0$mod_spec$J) {
        var_cat0 = unique(problem0$Y[vv,])
        var_cat0 = sort(var_cat0[!is.na(var_cat0)])
        var_cat = unique(Y_train0[vv,])
        var_cat = sort(var_cat[!is.na(var_cat)])
        if(length(var_cat) == length(var_cat0)) {
          Y_train[vv,] <- Y_train0[vv,]
          Y_test [vv,] <- Y_test0[vv,]
        } else { # remapping needed
          
          miss_var <- setdiff(var_cat0, var_cat)  # identify missing categories
          Y_train[vv,] <- Y_train0[vv,]  # initially set as same
          Y_test[vv,]  <- Y_test0[vv,]  # initially set as same
          
          for(mm in 1:length(miss_var)){
            # randomly decide adjacent attachment for training set
            att_var <- sample(c(1,-1), 1)
            
            if(miss_var[mm] == 0){
              # If the missing variable is 0, bump all scores down in the
              # training set
              # In the test set, retain the 0 score and bump the remaining
              # scores down
              Y_train[vv,] <- Y_train[vv,] - 1
              Y_test[vv,] <- ifelse(Y_test[vv,] == miss_var[mm],
                                    Y_test[vv,],
                                    Y_test[vv,] - 1)
              
            } else {
              # TRAIN: If the score is lower than the missing variable, keep
              # the score. Otherwise, reduce by 1
              # TEST: If the score is equal to the missing variable, attach to
              # neighboring score. Adjust for remapping
              Y_train[vv,] <- ifelse(Y_train[vv,] < miss_var[mm], Y_train[vv,], Y_train[vv,] - 1)
              Y_test[vv,] <- ifelse(Y_test[vv,] == miss_var[mm],
                                    Y_test[vv,] + att_var,
                                    ifelse(Y_test[vv,] > miss_var[mm],
                                           Y_test[vv,] - 1,
                                           Y_test[vv,]))
            }
            
            if(mm == length(miss_var)){
              break
            } else {
              # account for remapping for next missing variable
              miss_var[mm+1] <- miss_var[mm+1] - 1
            }
          }
        }
      } else { # continuous, not ordinal
        Y_train[vv,] <- Y_train0[vv,]
        Y_test [vv,] <- Y_test0[vv,]
      }
    }
    
    train$Y <- Y_train
    test$Y  <- Y_test
    train$mod_spec <- mod_spec
    test_list[[ff]] <- test
    train_list[[ff]] <- train
    
  }
  return(list(test_list=test_list,train_list=train_list,seed=seed))
}


#' @title Wrapper function to save problem files
#'
#' @description
#' Save the input problem to the data_dir directory using the unique ID
#' analysis_name. Either a main problem is saved (if is_folds is FALSE, the
#' default) or a set of fold problems are saved (if is_folds is TRUE). If
#' is_folds is TRUE, the input should be a list with named elements train_list
#' and test_list that contain the fold training and test problems.
#'
#' @param data_dir The data directory with problems and results
#' @param analysis_name A unique analysis name (for the input data directory)'
#' @param problem Problem that needs saving
#' @param is_folds Whether this object represents folds
#'
#' @examples 
#' # Generate a dummy problem
#' problem <- list(x = 1:6,
#'                 Y = rnorm(6))
#' # Use R's temporary directory as the data directory
#' data_dir <- tempdir()
#'
#' # Use 'ex' as the unique analysis ID
#' analysis_name <- 'ex'
#'
#' # Check if problem file already exists; should be FALSE
#' print(file.exists(paste0(data_dir,'problem_ex.rds')))
#' 
#' # Call save_problem to save problem_ex.rds in data_dir
#' save_problem(data_dir,analysis_name,problem)
#' 
#' # Check if problem file now exists in dir_path; should be TRUE
#' file.exists(paste0(dir_path,'problem_ex.rds'))  
#' 
#' # Remove dummy problem file from temporary directory
#' was_successful <- file.remove(paste0(dir_path, 'problem_ex.rds'))
#' 
#' @export
save_problem <- function(data_dir, analysis_name, problem, is_folds=F) {
  # TODO: Whether the problem represents folds could be determined from its
  #       form.
  # TODO: Consider saving the cross-validation in a single file, including
  #       saving the random number seed. Currently, the seed is never saved
  #       anywhere.
  if(is_folds){
    for(ff in 1:length(problem$train_list)){
      saveRDS(problem$train_list[[ff]],
              build_file_path(data_dir,
                              analysis_name,
                              "training_problem",
                              fold=ff))
      saveRDS(problem$test_list[[ff]],
              build_file_path(data_dir,
                              analysis_name,
                              "test_problem",
                              fold=ff))
    }
  } else {
      saveRDS(problem,
              build_file_path(data_dir,
                              analysis_name,
                              "main_problem"))
  }
}



#' @title Build the path to a file in the data directory
#'
#' @description
#' The data directory ([data_dir]) contains one or more problem files and fits
#' (solutions) uniquely specified by the analysis name ([analysis_name]). For
#' example, if the analysis name is "US" and the there are two cross validation
#' folds ([num_folds=2]), there are five total problem files in the data
#' directory:
#'
#' problem_US.rds            The main problem file
#' test_US_fold1.rds         The 1st test fold
#' test_US_fold2.rds         The 2nd test fold
#' train_US_fold1.rds        The 1st training fold
#' train_US_fold2.rds        The 2nd training fold
#'
#' This function returns the path to one of these files. Which one is
#' specified by the variables [file_type] and fold. [file_type] is a string
#' that must be one of: 'main_problem', 'test_problem', 'training_problem',
#' 'univariate_ord_soln', 'ordinal_ci', 'univariate_ord_rmd',
#' 'univariate_cont_soln', 'univariate_cont_rmd', 'solutionx', 'cv_data',
#' 'mcp_inputs', 'cindep_model', 'hjk_progress', and 'cdep_model'. If
#' [file_type] is test_problem' or training_problem', a valid fold number 
#' is required as input. If [file_type] is univariate_ord_soln', 
#' 'univariate_cont_soln', 'solutionx', mcp_inputs', 
#' 'hjk_progress', or 'mcp_model, a fold number may be input.
#'
#' @param data_dir The data directory with problems and results
#' @param analysis_name A unique analysis name (for the input data directory)'
#' @param file_type The type of file to return the path for (see Description)
#' @param j The index of the ordinal variable (if applicable)
#' @param k The index of the continuous variable (if applicable)
#' @param var_name The variable name (if applicable)
#' @param fold The fold number (only needed file_type is 'test_problem' or
#'   'training_problem').
#'
#' @return The path to the file
#'
#' @export
build_file_path <- function(data_dir,analysis_name,file_type,
                            j=NA,k=NA,var_name=c(),
                            mean_spec=c(),noise_spec=c(),
                            fold=NA) {

  if (file_type == "main_problem") {
    file_name <- paste0("problem_",analysis_name,".rds")
  } else if (file_type ==  "test_problem") {
    file_name <- paste0("test_",analysis_name,"_fold",fold,".rds")
  } else if (file_type ==  "training_problem") {
    file_name <- paste0("train_",analysis_name,"_fold",fold,".rds")
  } else if (file_type ==  "univariate_ord_soln") {
    # Examples of two univariate solutions:
    #   solutiony_US_ord_j_1_FH_EF_log_ord_lin_pos_int.rds
    #   solutiony_US_fold1_cont_k_13_RDL_pow_law_const.rds
    file_name <- paste0("solutiony_",analysis_name)
    if (!is.na(fold)) {
      file_name <- paste0(file_name,"_fold",fold)
    }
    file_name <- paste0(file_name,"_ord_j_",j,"_",var_name,
                        "_",mean_spec,"_",noise_spec,".rds")
  } else if (file_type == "ordinal_ci") {
    file_name <- paste0("ordinal_ci_",analysis_name,"_j_",j,"_",var_name,".rds")
  } else if (file_type == "univariate_ord_rmd") {
    file_name <- paste0(analysis_name,"_ord_j_",j,"_",var_name,".Rmd")
  } else if (file_type ==  "univariate_cont_soln") {
    file_name <- paste0("solutiony_",analysis_name)
    if (!is.na(fold)) {
      file_name <- paste0(file_name,"_fold",fold)
    }
    file_name <- paste0(file_name,"_cont_k_",k,"_",var_name,
                        "_",mean_spec,"_",noise_spec,".rds")
  } else if (file_type == "univariate_cont_rmd") {
    file_name <- paste0(analysis_name,"_cont_k_",k,"_",var_name,".Rmd")
  } else if (file_type ==  "solutionx") {
    file_name <- paste0("solutionx_",analysis_name)
    if (!is.na(fold)) {
      file_name <- paste0(file_name,"_fold",fold)
    }
    file_name <- paste0(file_name,".rds")
  } else if (file_type ==  "eval_data") {
    file_name <- paste0("eval_data_univariate_",analysis_name,".rds")
  } else if (file_type == "aic") {
    file_name <- paste0("aic_",analysis_name,"_",var_name,".rds")
  } else if (file_type == "mcp_inputs") {
    file_name <- paste0("mcp_inputs_",analysis_name)
    if (!is.na(fold)) {
      file_name <- paste0(file_name,"_fold",fold)
    }
    file_name <- paste0(file_name,".rds")
  } else if (file_type == "cindep_model") {
    file_name <- paste0("cindep_model_",analysis_name)
    if (!is.na(fold)) {
      file_name <- paste0(file_name,"_fold",fold)
    }
    file_name <- paste0(file_name,".rds")
  } else if(file_type == "hjk_progress") {
    file_name <- paste0("hjk_progress_",analysis_name)
    if (!is.na(fold)) {
      file_name <- paste0(file_name,"_fold",fold)
    }
    file_name <- paste0(file_name,".rds")
  } else if (file_type == "cdep_model") {
    # TODO: consider adding an optional model ID
    file_name <- paste0("cdep_model_",analysis_name)
    if (!is.na(fold)) {
      file_name <- paste0(file_name,"_fold",fold)
    }
    file_name <- paste0(file_name,".rds")
  } else {
    stop(paste0('Unrecognized file_type = ', file_type))
  }
  file_path <- file.path(data_dir,file_name)
  return(file_path)
}

#' @title Build a list of univariate ordinal problems
#'
#' @description
#' The data directory (data_dir) contains a main problem file uniquely
#' determined by the analysis name (analysis_name), and possibly some
#' corresponding fold problems. Return a list of univariate problems to
#' solve. The model specifications to include in the returned list are specified
#' by mean_specs and noise_specs, which must be the same lengths. By default,
#' all combinations of mean specifications and noise specifications supported by
#' yada are used; there are three ordinal mean specifications (pow_law_ord,
#' log_ord, and lin_ord) and two noise specifications (const and lin_pos_int), so
#' by default there are num_models = 6 six total model specifications. By
#' default, cross  validation fold problems are not included, in which case the
#' return list, ord_prob_list, has length num_models * J. If the cross
#' validation problems are included, ord_prob_list has length
#' num_models * J * (1+num_folds).
#'
#' @param data_dir The data directory with problems and results
#' @param analysis_name A unique analysis name (for the input data directory)
#' @param mean_specs A vector of mean specifications for the ordinal models
#'   (default:
#'   [c('pow_law_ord','pow_law_ord','log_ord','log_ord','lin_ord','lin_ord'])
#' @param noise_specs A vector of noise specifications for the ordinal models
#'   (default:
#'   [c('const','lin_pos_int','const','lin_pos_int','const','lin_pos_int'])
#' @param add_folds Whether or not to include cross validation folds in the
#'   return list
#'
#' @return A list of problems
#'
#' @export
build_univariate_ord_problems <- function(data_dir,
                                          analysis_name,
                                          mean_specs=c(rep('pow_law_ord', 2),
                                                       rep('log_ord',2),
                                                       rep('lin_ord',2)),
                                          noise_specs=rep(c('const',
                                                            'lin_pos_int'),
                                                          3),
                                          add_folds=FALSE) {

  if (length(mean_specs) != length(noise_specs)) {
    stop("Lengths of mean_specs and noise_specs not equal")
  }

  # Build main problems
  problem <- readRDS(build_file_path(data_dir,
                                     analysis_name,
                                     "main_problem"))
  ord_prob_list <- list()
  for(j in 1:problem$mod_spec$J) {
    for(mod_num in 1:length(mean_specs)) {
      var_name <- problem$var_names[j]

      mod_spec <- list()
      mod_spec$mean_spec  <- mean_specs [mod_num]
      mod_spec$noise_spec <- noise_specs[mod_num]
      mod_spec$J <- 1
      mod_spec$M <- problem$mod_spec$M[j]

      # Handle missing variables
      x <- problem$x
      v <- problem$Y[j,]
      ind_keep <- !is.na(x) & !is.na(v)
      x <- x[ind_keep]
      v <- v[ind_keep]

      ord_prob_list[[length(ord_prob_list)+1]] <- list(x=x,
                                                       v=v,
                                                       mod_spec=mod_spec,
                                                       var_name=var_name,
                                                       j=j,
                                                       prob_type="main")
    }
  }

  if (add_folds) {
    num_folds <- get_num_training_problems(data_dir,analysis_name)
    for (f in 1:num_folds) {
      problem_f <- readRDS(build_file_path(data_dir,
                                           analysis_name,
                                           "training_problem",
                                           fold=f))
      for(j in 1:problem$mod_spec$J) {
        for(mod_num in 1:length(mean_specs)) {
          var_name <- problem_f$var_names[j]

          mod_spec <- list()
          mod_spec$mean_spec  <- mean_specs [mod_num]
          mod_spec$noise_spec <- noise_specs[mod_num]
          mod_spec$J <- 1
          mod_spec$M <- problem_f$mod_spec$M[j]

          # Handle missing variables
          x <- problem_f$x
          v <- problem_f$Y[j,]
          ind_keep <- !is.na(x) & !is.na(v)
          x <- x[ind_keep]
          v <- v[ind_keep]

          ord_prob_list[[length(ord_prob_list)+1]] <- list(x=x,
                                                           v=v,
                                                           mod_spec=mod_spec,
                                                           var_name=var_name,
                                                           prob_type="train",
                                                           j=j,
                                                           fold=f)
        }
      }
    }
  }
  return(ord_prob_list)
}

#' @title Solve a univariate ordinal problem
#'
#' @description
#' Solve the input univariate ordinal problem by doing a maximum likelihood
#' fit to find the best parameter vector. A save file is added to the data
#' directory (data_dir) in .rds format.
#' @param data_dir The data directory with problems and results
#' @param analysis_name A unique analysis name (for the input data directory)
#' @param ord_prob The ordinal problem to solve (likely generated by
#'   [build_univariate_ord_problems]
#' @param anneal_seed An optional seed to pass to fit_univariate_ord to make
#'   annealing reproducible (default: NA, not used)
#'
#' @return Whether or not the fit succeeded (TRUE or FALSE)
#'
#' @export
# TODO: consider whether to save problems to file rather than inputing them.
solve_ord_problem <- function(data_dir, analysis_name, ord_prob,
                              anneal_seed=NA) {

  th_y <- try(yada::fit_univariate_ord(ord_prob$x,
                                               ord_prob$v,
                                               ord_prob$mod_spec,
                                               anneal_seed=anneal_seed),
                      silent=T)
  # Build the save file path
  problem <- readRDS(build_file_path(data_dir,
                                     analysis_name,
                                     "main_problem"))


  file_type <- "univariate_ord_soln"
  if (ord_prob$prob_type == "main") {
    fold <- NA
  } else {
    fold <- ord_prob$fold
  }

  file_path <- build_file_path(data_dir,
                               analysis_name,
                               file_type,
                               j=ord_prob$j,
                               var_name=ord_prob$var_name,
                               mean_spec=ord_prob$mod_spec$mean_spec,
                               noise_spec=ord_prob$mod_spec$noise_spec,
                               fold=fold)

  if(class(th_y) == "try-error") {
    saveRDS(th_y,file_path)
    return(F)
  } else {
    saveRDS(list(th_y=th_y,mod_spec=ord_prob$mod_spec),
            file_path)
    return(T)
  }
}

#' @title Build a list of univariate continuous problems
#'
#' @description
#' The data directory (data_dir) contains a main problem file uniquely
#' determined by the analysis name (analysis_name), and possibly some
#' corresponding fold problems. Return a list of univariate problems to
#' solve. The model specifications to include in the returned list are specified
#' by mean_specs and noise_specs, which must be the same lengths. By default,
#' all combinations of mean specifications and noise specifications supported by
#' yada are used; there is one continuous mean specification (pow_law) and two
#' noise specifications (const and lin_pos_int), so by default there are
#' num_models = 2 six total model specifications. By default, cross  validation
#' fold problems are not included, in which case the return list, ord_prob_list,
#' has length num_models * K. If the cross validation problems are included,
#' cont_prob_list has length num_models * K * (1+num_folds).
#'
#' @param data_dir The data directory with problems and results
#' @param analysis_name A unique analysis name (for the input data directory)
#' @param mean_specs A vector of mean specifications for the continuous models
#'   (default: [c('pow_law','pow_law')])
#' @param noise_specs A vector of noise specifications for the continuous models
#'   (default: [c('const','lin_pos_int'])
#' @param add_folds Whether or not to include cross validation folds in the
#'   return list
#'
#' @return A list of problems
#'
#' @export
build_univariate_cont_problems <- function(data_dir,
                                           analysis_name,
                                           mean_specs=c(rep('pow_law', 2)),
                                           noise_specs=c('const',
                                                         'lin_pos_int'),
                                           add_folds=FALSE) {

  if (length(mean_specs) != length(noise_specs)) {
    stop("Lengths of mean_specs and noise_specs not equal")
  }

  # Build main problems
  problem <- readRDS(build_file_path(data_dir,
                                     analysis_name,
                                     "main_problem"))
  J <- get_J(problem$mod_spec)
  cont_prob_list <- list()
  for(k in 1:problem$mod_spec$K) {
    for(mod_num in 1:length(mean_specs)) {
      var_name <- problem$var_names[J+k]

      mod_spec <- list()
      mod_spec$mean_spec  <- mean_specs [mod_num]
      mod_spec$noise_spec <- noise_specs[mod_num]
      mod_spec$K <- 1

      # Handle missing variables
      x <- problem$x
      w <- problem$Y[J+k,]
      ind_keep <- !is.na(x) & !is.na(w)
      x <- x[ind_keep]
      w <- w[ind_keep]

      cont_prob_list[[length(cont_prob_list)+1]] <- list(x=x,
                                                         w=w,
                                                         mod_spec=mod_spec,
                                                         var_name=var_name,
                                                         k=k,
                                                         prob_type="main")
    }
  }

  if (add_folds) {
    num_folds <- get_num_training_problems(data_dir,analysis_name)
    for (f in 1:num_folds) {
      problem_f <- readRDS(build_file_path(data_dir,
                                           analysis_name,
                                           "training_problem",
                                           fold=f))
      for(k in 1:problem$mod_spec$K) {
        for(mod_num in 1:length(mean_specs)) {
          var_name <- problem_f$var_names[J+k]

          mod_spec <- list()
          mod_spec$mean_spec  <- mean_specs [mod_num]
          mod_spec$noise_spec <- noise_specs[mod_num]
          mod_spec$K <- 1

          # Handle missing variables
          x <- problem_f$x
          w <- problem_f$Y[J+k,]
          ind_keep <- !is.na(x) & !is.na(w)
          x <- x[ind_keep]
          w <- w[ind_keep]

          cont_prob_list[[length(cont_prob_list)+1]] <-
            list(x=x,
                 w=w,
                 mod_spec=mod_spec,
                 var_name=var_name,
                 prob_type="train",
                 k=k,
                 fold=f)
        }
      }
    }
  }
  return(cont_prob_list)
}

#' @title Solve a univariate continuous problem
#'
#' @description
#' Solve the input unaviriate continuous problem by doing a maximum likelihood
#' fit to find the best parameter vector. A save file is added to the data
#' directory (data_dir) in .rds format.
#'
#' @param cont_prob The continuous problem to solve (likely generated by
#'   [build_univariate_cont_problems]
#' @param data_dir Data directory directory where the solution will be saved
#'
#' @return Whether or not the fit succeeded (TRUE or FALSE)
#'
#' @export
# TODO: consider whether to save problems to file rather than inputing them.
solve_cont_problem <- function(data_dir, analysis_name, cont_prob) {

  th_y <- try(yada::fit_univariate_cont(cont_prob$x,
                                                cont_prob$w,
                                                cont_prob$mod_spec),
                      silent=T)
  # Build the save file path
  problem <- readRDS(build_file_path(data_dir,
                                     analysis_name,
                                     "main_problem"))


  # J <- get_J(problem$mod_spec)

  file_type <- "univariate_cont_soln"
  if (cont_prob$prob_type == "main") {
    fold <- NA
  } else {
    fold <- cont_prob$fold
  }

  file_path <- build_file_path(data_dir,
                               analysis_name,
                               file_type,
                               k=cont_prob$k,
                               var_name=cont_prob$var_name,
                               mean_spec=cont_prob$mod_spec$mean_spec,
                               noise_spec=cont_prob$mod_spec$noise_spec,
                               fold=fold)

  if(class(th_y) == "try-error") {
    saveRDS(th_y,file_path)
    return(F)
  } else {
    saveRDS(list(th_y=th_y,mod_spec=cont_prob$mod_spec),
            file_path)
    return(T)
  }
}

#' @title Build model vector
#' 
#' @description A function to build a vector of mean-noise model specifications
#' to be used in the function [evaluate_univariate_models]. This function 
#' creates all possible mean-noise combinations, resulting in a vector of 
#' length = length(mean_models) x length(noise_models)
#' 
#' @param mean_models A vector with the mean model names
#' @param noise_models A vector with the noise model names
#' 
#' @return A vector with all possible mean-noise model combinations
#' 
#' @export

build_model_vec <- function(mean_models, noise_models) {
  model_vec <- c()
  for(mean_model in mean_models) {
    for(noise_model in noise_models) {
      model_vec <- c(model_vec, paste0(mean_model,"_",noise_model))
    }
  }
  
  return(model_vec)
}


#' @title Calculate the prior parameterization on x
#' 
#' @description A function to apply a Weibull offset mixture to the response 
#'   variable (x) to establish a parameterization for the prior on x
#' 
#' @param data_dir The directory where data are pulled and saved
#' @param analysis_name Unique identifier for the analysis
#' @param prior_type The type of prior to be applied to x, currently only 
#'   one available option, prior_type="weibull"
#' @param offset Amount by which to offset the x values
#' @param fold The fold number if not calculating for the main problem. 
#'   Default is fold=NA  
#' @param save_file Logical whether to save the output in data_dir
#' 
#' @return A list with the prior parameterization specifications
#' 
#' @export
calc_x_prior <- function(data_dir, analysis_name,
                         prior_type="weibull", offset,
                         seed, fold=NA, save_file=F) {
  
  # Load the problem file
  if (is.na(fold)) {
    problem <- readRDS(build_file_path(data_dir,
                                       analysis_name,
                                       "main_problem"))
  } else {
    problem <- readRDS(build_file_path(data_dir,
                                       analysis_name,
                                       "training_problem",
                                       fold=fold))
  }
  
  if (prior_type!="weibull") {
    stop("Current input for prior_type is not an available option.")
  }
  
  # if (prior_type=="weibull") {
  fit_type <- "offset_weib_mix"
  
  set.seed(seed)  # set seed for replication
  
  # define Weibull fit with predictor offset
  prior_fit <- mixtools::weibullRMM_SEM(problem$x + offset,
                                        k=3,
                                        maxit=2000)
  # }
  
  # Generate and save the x-solution for the problem
  th_x <- list(fit_type='offset_weib_mix',
               fit=prior_fit,
               weib_offset=offset)
  
  if (save_file) {
    saveRDS(th_x, build_file_path(data_dir, analysis_name, "solutionx"))
  }
  
  return(th_x)
}


#' @title Calculate point estimate and credible interval for test data
#'   (univariate)
#'
#' @description A wrapper function that calculates the point estimate and 
#'   credible interval for all observations in the provided test sample
#'   by input variable using univariate models. 
#'   The function prints the observation number currently being predicted and 
#'   also returns the top 6 rows of a completed prediction table.
#'   
#' @param data_dir The directory from which models should be imported
#' @param analysis_name Unique identifier for the analysis
#' @param test_samp A data frame with test observations  
#' @param demo_cols A vector representing demographic column numbers  
#' @param input_cols A vector representing the input (predictor) columns
#' @param ci_type Whether the credible intervals should be calculated using 
#'   "hdi" or "quantiles"  
#' @param th_x The prior parameterization on x  
#' @param xcalc A vector of evenly-spaced values at which to calculate the 
#'   posterior density 
#' @param seed A numeric value to set the seed for replication  
#' @param save_file A logical for whether the results should be saved. 
#'   (default: T)
#'
#' @export
univariate_batch_calc <- function(data_dir, analysis_name, 
                                  test_samp, demo_cols,
                                  input_cols, ci_type, th_x,
                                  xcalc, seed, save_file=T) {
  
  ## Initialize empty data frames for demo columns and prediction columns
  demo0 <- data.frame(matrix(data=NA, nrow=nrow(test_samp),
                             ncol=length(demo_cols)))
  pred0 <- data.frame(matrix(data=NA, nrow=nrow(test_samp), 
                             ncol=7))  # mean, median, mode, lower99, lower95, 
  # upper95, upper99
  
  # Load problem file for current analysis
  problem <- readRDS(build_file_path(data_dir, analysis_name, 'main_problem'))
  
  # Load the prior on th_x 
  # th_x is now provided
  # th_x <- readRDS(build_file_path(data_dir, analysis_name, 'solutionx'))
  
  ## Loop through available response variables in test sample
  for(i in input_cols) {
    var_name <- colnames(test_samp)[i]
    model <- tryCatch({
      load_best_univariate_model(data_dir, analysis_name, var_name=var_name)},
      error=function(cond) {
        return(NA)
      })
    
    if (length(model) != 2) {
      print(paste0("Best model not selected for ",var_name))
      next
    }
    
    print(paste0("---Starting batch processing for ",var_name,"---"))
    
    Y <- test_samp[[i]]  # vector of current variable values
    
    ## Loop through values in Y and estimate point and CI
    for (n in 1:length(Y)) {
      demo0[n,] <- test_samp[n,demo_cols]
      print(paste0("Calculating for observation n=", n))
      
      ## Check if current_val is NA
      if (is.na(Y[n])) {
        pred0[n,] <- rep(NA,7)
        next
      }
      
      ## Calculate posterior density, point estimate, and confidence interval
      x_post <- calc_x_posterior(Y[n], th_x, model, xcalc, normalize=T, seed)
      x_analyze <- analyze_x_posterior(x_post$x, x_post$density, 
                                       ci_type=ci_type)
      pred0[n,] <- round(c(x_analyze$xmean, x_analyze$xmed, x_analyze$xmode,
                           x_analyze$xlolo, x_analyze$xlo, 
                           x_analyze$xhi, x_analyze$xhihi),2)
    }
    
    df <- cbind(demo0, pred0)
    colnames(df) <- c(colnames(test_samp[demo_cols]),
                      "xmean","xmed","xmode",
                      "lower99","lower95",
                      "upper95","upper99")
    
    if(save_file) {
      write.csv(df, paste0(data_dir,'/',var_name,"_",
                           analysis_name,"_test_predictions.csv"),row.names=F)
    }
    
    print(paste0('Prediction Table for ',var_name))
    print(head(df))
  }
}


#' @title Calculate point estimate and credible interval for test data 
#'   (multivariate)
#'
#' @description A wrapper function that calculates the point estimate and 
#'   credible interval for all observations in the provided test sample
#'   using using a defined model. 
#'   The function prints the observation number currently being predicted and 
#'   also returns the top 6 rows of a completed prediction table.
#'   
#' @param data_dir The directory from which models should be imported
#' @param analysis_name Unique identifier for the analysis
#' @param test_samp A data frame with test observations  
#' @param demo_cols A vector representing demographic column numbers  
#' @param model_type A character string defining either the conditionally 
#'   independent ("cindep") or conditionally dependent ("cdep") model
#' @param ci_type Whether the credible intervals should be calculated using 
#'   "hdi" or "quantiles"
#' @param th_x The prior parameterization on x  
#' @param xcalc A vector of evenly-spaced values at which to calculate the 
#'   posterior density 
#' @param seed A numeric value to set the seed for replication  
#' @param save_file A logical for whether the results should be saved. 
#'   (default: T)
#'
#' @export
multivariate_batch_calc <- function(data_dir, analysis_name, 
                                    test_samp, demo_cols,
                                    model_type, ci_type, th_x,
                                    xcalc, seed, save_file=T) {
  
  # Load problem file for current analysis
  problem <- readRDS(build_file_path(data_dir, analysis_name,
                                     'main_problem'))
  
  
  # Calculate parameterization for prior on x (age)
  # th_x is now provided
  # th_x <- readRDS(build_file_path(data_dir,analysis_name,"solutionx"))
  
  # Extract response variables as matrix Y in order expected by multivariate model
  ef_vars <- grep("_EF|_Oss", names(test_samp))
  dent_vars <- grep("man_|max_", names(test_samp))
  lb_vars <- grep("DL|PB|MSB|DB", names(test_samp))
  Y <- t(test_samp[c(ef_vars, dent_vars, lb_vars)])
  if (!all(rownames(Y) == problem$var_names)) {
    stop("Problem with organizing response variables")
  }
  
  ## Initialize empty data frames for demo columns and prediction columns
  demo0 <- data.frame(matrix(data=NA, nrow=nrow(test_samp),
                             ncol=length(demo_cols)))
  pred0 <- data.frame(matrix(data=NA, nrow=nrow(test_samp), 
                             ncol=7))  # mean, median, mode, lower99, lower95, 
  # upper95, upper99
  
  ## Initialize multivariate model parameters
  file_type <- paste0(model_type,"_model")
  model <- readRDS(build_file_path(data_dir,analysis_name,file_type))
  
  for(i in 1:ncol(Y)) {
    print(paste0('Calculating age posterior for observation=',i))
    
    demo0[i,] <- test_samp[i,demo_cols]
    
    if (all(is.na(Y[,i]))) {
      pred0[i,] <- rep(NA,7)
    }
    
    x_post <- calc_x_posterior(Y[,i], th_x, model, xcalc, normalize=T, seed=NA)
    x_analyze <- analyze_x_posterior(x_post$x, x_post$density,
                                     ci_type=ci_type)
    
    pred0[i,] <- round(c(x_analyze$xmean, x_analyze$xmed, x_analyze$xmode,
                         x_analyze$xlolo, x_analyze$xlo, 
                         x_analyze$xhi, x_analyze$xhihi),2)
  }
  
  df <- cbind(demo0,pred0)
  colnames(df) <- c(colnames(test_samp[demo_cols]),
                    "xmean","xmed","xmode",
                    "lower99","lower95",
                    "upper95","upper99")
  
  if (save_file) {
    write.csv(df, paste0(data_dir,"/",file_type,"_",analysis_name,
                         "_test_predictions.csv"), row.names=F)
  }
  print(paste0('Prediction Table for ',file_type,':'))
  print(head(df))
}


#' @title Generate ordinal credible interval tables
#'
#' @description Calculate a table providing the point estimate and 
#'   credible interval of a given univariate ordinal model
#'   
#' @param data_dir The directory from which models should be imported
#' @param analysis_name Unique identifier for the analysis
#' @param var_name A character string with the variable of interest
#' @param th_x The prior parameterization on x  
#' @param point_est A character string with the type of point estimate to be 
#'   returned: mean ("xmean"), median ("xmed"), or mode ("xmode") 
#' @param ci_type Whether the credible interval should be calculated using 
#'   "quantiles" or highest density interval ("hdi"). (default is "hdi") 
#' @param xcalc A vector of evenly-spaced values at which to calculate the 
#'   posterior density 
#' @param save_file A logical for whether the results should be saved. 
#'   (default: T)
#'
#' @export
generate_ord_ci <- function(data_dir, analysis_name, var_name,
                            th_x, point_est, ci_type="hdi", 
                            xcalc, save_file=F) {
  
  # Load the best ordinal model
  ord_model <- load_best_univariate_model(data_dir, analysis_name,
                                          var_name)
  
  # Calculate the confidence intervals and point estimate
  ci_df <- calc_ci_ord(ord_model, th_x, point_est, ci_type, xcalc)
  
  if(save_file) {
    saveRDS(ci_df, build_file_path(data_dir,
                                   analysis_name,
                                   "ordinal_ci",
                                   var_name=var_name))
  }
  
  return(ci_df)
}

#' @title Clear all the files in the temporary directory
#'
#' @description
#' Clear all the files in the temporary directory. Directories are not deleted.
#' clear_temp_dir is used in testing.
#'
#' @export
clear_temp_dir <- function() {
  for (f in dir(tempdir())) {
    full_path <- file.path(tempdir(),f)
    if (!dir.exists(full_path)) {
      success <- file.remove(full_path)
    }
  }
}
