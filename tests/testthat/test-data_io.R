data_dir <- tempdir()
clear_temp_dir()

# Test parse_cat_spec
expect_equal(
  parse_cat_spec("2 1 {3 4} -4"),
  list("2"=0,
       "1"=1,
       "3"=2,
       "4"=2,
       "-4"=3)
)

expect_equal(
  parse_cat_spec("a c d b"),
  list("a"=0,
       "c"=1,
       "d"=2,
       "b"=3)
)

expect_equal(
  parse_cat_spec("{b a} -1 {c d e} 4"),
  list( "b"=0,
        "a"=0,
       "-1"=1,
       "c"=2,
       "d"=2,
       "e"=2,
       "4"=3)
)

expect_error(
  parse_cat_spec("{b a} -1 {a d e} 4"),
  "The category names are not unique"
)

# Test apply_cat_spec
expect_error(
  apply_cat_spec(as.factor(c("0","1")),"0 1"),
  "v_str should be a vector of strings, not factors"
)

expect_error(
  apply_cat_spec(c("0","2"),"0 1"),
  "2 not found in category mapping"
)

expect_equal(
  apply_cat_spec(c("a",NA,"d","b","c", NA,"d","a"), "a c d b"),
  c(0,NA,2,3,1,NA,2,0),
)

expect_equal(
  apply_cat_spec(c("d","-1","e","a", NA,"b","4","c"),"{b a} -1 {c d e} 4"),
  c(2,1,2,0,NA,0,3,2)
)

expect_equal(
  apply_cat_spec(7,"{0 1 2} 3 4 5 6 7"),
  5
)

# Test build_lr_var
expect_error(
  build_lr_var("var1","middle","_L"),
  "side_loc = 'middle', but should be 'start' or 'end'"
)

expect_equal(
  build_lr_var("var1","end","_L"),
  "var1_L"
)

expect_equal(
  build_lr_var("variable_two","start","right_"),
  "right_variable_two"
)

# To check left/right merging, create a test data frame in which the labels are
# affixed. merge_lr_var and merge_multiple_lr_var both expect that ordinal
# variables are integers for which relative order is meaning (e.g., by calling
# parse_cat_spec and apply_cat_spec).
test_df <- data.frame(age = c(10,7,12,1,4,8),
                      sex = c('M','F','F','M','M','F'),
                      ord_var_L = c(0,NA,1,NA,2,NA),
                      ord_var_R = c(2, 1,0,NA,2, 1),
                      cont_var_L = c(10.5, NA,14.0,NA,8.7, NA),
                      cont_var_R = c(12.5,3.2, 7.0,NA,8.7,1.0),
                      other_var = 1:6)

# Check the errors
expect_error(
  merge_lr_var(test_df,
               base_var="ord_var",
               side_loc="end",
               side_labels=c("_L", "_R"),
               approach="bad"),
  "Invalid approach specified. See documentation for merge_lr_var."
)

expect_error(
  merge_lr_var(test_df,
               base_var="ord_var",
               side_loc="end",
               side_labels="_L",
               "left"),
  "side_labels should be length 2"
)

expect_error(
  merge_lr_var(test_df,
               base_var="ord_var",
               side_loc="end",
               side_labels=c("_L", "_L"),
               "left"),
  "side_labels[1] should not equal side_labels[2]",
  fixed=TRUE
)

expect_error(
  merge_lr_var(test_df,
               base_var="ord_var",
               side_loc="bad_loc",
               side_labels=c("_L", "_R"),
               "left"),
  "side_loc must be either 'start' or 'end'"
)

expect_error(
  merge_lr_var(test_df,
               base_var="ord_var",
               side_loc="end",
               side_labels=c("_notL", "_R"),
               approach="left"),
  "Left variable is not in input dataframe"
)

expect_error(
  merge_lr_var(test_df,
               base_var="ord_var",
               side_loc="end",
               side_labels=c("_L", "_badR"),
               approach="left"),
  "Right variable is not in input dataframe"
)

# Test each approach (using the continuous variable to test the mean)
# approach = "left"
expect_equal(
  merge_lr_var(test_df,
               base_var="ord_var",
               side_loc="end",
               side_labels=c("_L", "_R"),
               approach="left"),
 data.frame(age = c(10,7,12,1,4,8),
            sex = c('M','F','F','M','M','F'),
            ord_var   = c(0,1,1,NA,2,1),
            cont_var_L = c(10.5, NA,14.0,NA,8.7, NA),
            cont_var_R = c(12.5,3.2, 7.0,NA,8.7,1.0),
            other_var = 1:6)
)

# approach = "right"
expect_equal(
  merge_lr_var(test_df,
               base_var="ord_var",
               side_loc="end",
               side_labels=c("_L", "_R"),
               approach="right"),
 data.frame(age = c(10,7,12,1,4,8),
            sex = c('M','F','F','M','M','F'),
            ord_var   = c(2,1,0,NA,2,1),
            cont_var_L = c(10.5, NA,14.0,NA,8.7, NA),
            cont_var_R = c(12.5,3.2, 7.0,NA,8.7,1.0),
            other_var = 1:6)
)

# approach = "mean"
expect_equal(
  merge_lr_var(test_df,
               base_var="cont_var",
               side_loc="end",
               side_labels=c("_L", "_R"),
               approach="mean"),
 data.frame(age = c(10,7,12,1,4,8),
            sex = c('M','F','F','M','M','F'),
            ord_var_L = c(0,NA,1,NA,2,NA),
            ord_var_R = c(2, 1,0,NA,2, 1),
            cont_var = c(11.5, 3.2,10.5,NA,8.7, 1.0),
            other_var = 1:6)
)

# approach = "lowest"
expect_equal(
  merge_lr_var(test_df,
               base_var="ord_var",
               side_loc="end",
               side_labels=c("_L", "_R"),
               approach="lowest"),
 data.frame(age = c(10,7,12,1,4,8),
            sex = c('M','F','F','M','M','F'),
            ord_var   = c(0,1,0,NA,2,1),
            cont_var_L = c(10.5, NA,14.0,NA,8.7, NA),
            cont_var_R = c(12.5,3.2, 7.0,NA,8.7,1.0),
            other_var = 1:6)
)

# approach = "highest"
expect_equal(
  merge_lr_var(test_df,
               base_var="ord_var",
               side_loc="end",
               side_labels=c("_L", "_R"),
               approach="highest"),
 data.frame(age = c(10,7,12,1,4,8),
            sex = c('M','F','F','M','M','F'),
            ord_var   = c(2,1,1,NA,2,1),
            cont_var_L = c(10.5, NA,14.0,NA,8.7, NA),
            cont_var_R = c(12.5,3.2, 7.0,NA,8.7,1.0),
            other_var = 1:6)
)

# Check merge if the merged variables are at the end of the data frame
expect_equal(
  merge_lr_var(test_df[,1:(ncol(test_df)-1)],
               base_var="ord_var",
               side_loc="end",
               side_labels=c("_L", "_R"),
               approach="lowest"),
 data.frame(age = c(10,7,12,1,4,8),
            sex = c('M','F','F','M','M','F'),
            ord_var   = c(0,1,0,NA,2,1),
            cont_var_L = c(10.5, NA,14.0,NA,8.7, NA),
            cont_var_R = c(12.5,3.2, 7.0,NA,8.7,1.0))
)

# Check merge for prefixing for one case
test_df <- data.frame(age = c(10,7,12,1,4,8),
                      sex = c('M','F','F','M','M','F'),
                      left_ord_var = c(0,NA,1,NA,2,NA),
                      right_ord_var = c(2, 1,0,NA,2, 1),
                      left_cont_var = c(10.5, NA,14.0,NA,8.7, NA),
                      right_cont_var = c(12.5,3.2, 7.0,NA,8.7,1.0),
                      other_var = 1:6)

expect_equal(
  merge_lr_var(test_df[,1:(ncol(test_df)-1)],
               base_var="ord_var",
               side_loc="start",
               side_labels=c("left_", "right_"),
               approach="lowest"),
 data.frame(age = c(10,7,12,1,4,8),
            sex = c('M','F','F','M','M','F'),
            ord_var   = c(0,1,0,NA,2,1),
            left_cont_var = c(10.5, NA,14.0,NA,8.7, NA),
            right_cont_var = c(12.5,3.2, 7.0,NA,8.7,1.0))
)

# Test merge_multiple_lr_var
test_df <- data.frame(age = c(10,7,12,1,4,8),
                      sex = c('M','F','F','M','M','F'),
                      ord_var_L = c(0,NA,1,NA,2,NA),
                      ord_var_R = c(2, 1,0,NA,2, 1),
                      cont_var_L = c(10.5, NA,14.0,NA,8.7, NA),
                      cont_var_R = c(12.5,3.2, 7.0,NA,8.7,1.0),
                      other_var = 1:6)

# Check that an error is thrown if approach and base_var are not the same length
expect_error(
  merge_multiple_lr_var(test_df,
                        base_var="ord_var",
                        side_loc="end",
                        side_labels=c("_L", "_R"),
                        approach=c("left","mean")),
 "approach and base_var should have the same length"
)

# Check that merge_multiple_lr_var gives the correct result for one case
expect_equal(
  merge_multiple_lr_var(test_df,
                        base_var=c("ord_var", "cont_var"),
                        side_loc="end",
                        side_labels=c("_L", "_R"),
                        approach=c("left","mean")),
  data.frame(age = c(10,7,12,1,4,8),
             sex = c('M','F','F','M','M','F'),
             ord_var   = c(0,1,1,NA,2,1),
             cont_var = c(11.5, 3.2,10.5,NA,8.7, 1.0),
             other_var = 1:6)
)

# Test load_var_info
# Confirm an error is thrown if a two column specification is used
temp_file <- file.path(data_dir,"temp.txt")
writeLines(c("Variable,Type","var1,x"),temp_file)
expect_error(
  load_var_info(temp_file),
  "The variable info data frame should have exactly 4 or 8 columns, not 2"
)
success <- file.remove(temp_file)

# Confirm an error is thrown if a four column specification is used, but with
# the wrong columns names
writeLines(c("Variable,Type,Bad,Missing_Value","var1,x,"),temp_file)
expect_error(
  load_var_info(temp_file),
  "Columns have the wrong names (see function documentation)",
  fixed=TRUE
)
success <- file.remove(temp_file)

# Confirm an error is thrown if an eight column specification is used, but with
# the wrong columns names
lines_to_write <- c(
  paste("Variable",
        "Type",
        "Categories",
        "Missing_Value",
        "Left_Right_Side",
        "Left_Label",
        "Right_Label",
        "Bad",
        sep=","),
   paste("var1",
        "x",
        "",
        "",
        "",
        "",
        "",
        sep=",")
)
writeLines(lines_to_write,temp_file)
expect_error(
  load_var_info(temp_file),
  "Columns have the wrong names (see function documentation)",
  fixed=TRUE
)
success <- file.remove(temp_file)

# Confirm an error is thrown if no x variable is specified
lines_to_write <- c(
  paste("Variable",
        "Type",
        "Categories",
        "Missing_Value",
        sep=","),
   paste("ord1",
        "ordinal",
        "0 1",
        "-1",
        sep=",")
)
writeLines(lines_to_write,temp_file)
expect_error(
  load_var_info(temp_file),
  "No x variable is specified"
)
success <- file.remove(temp_file)

# Confirm an error is thrown if more than one x variable is specified
lines_to_write <- c(
  paste("Variable",
        "Type",
        "Categories",
        "Missing_Value",
        sep=","),
  paste("x1",
        "x",
        "",
        "",
        sep=","),
  paste("x2",
        "x",
        "",
        "-1",
        sep=",")
)
writeLines(lines_to_write,temp_file)
expect_error(
  load_var_info(temp_file),
  "More than one x variable is specified"
)
success <- file.remove(temp_file)

# Confirm an error is thrown if an invalid variable type is used
lines_to_write <- c(
  paste("Variable",
        "Type",
        "Categories",
        "Missing_Value",
        sep=","),
  paste("x",
        "x",
        "",
        "",
        sep=","),
  paste("var",
        "bad",
        "",
        "-1",
        sep=",")
)
writeLines(lines_to_write,temp_file)
expect_error(
  load_var_info(temp_file),
  "Invalid variable type, bad, used in row 2"
)
success <- file.remove(temp_file)

# Confirm an error is thrown if no category specification is given for an
# ordinal variable
lines_to_write <- c(
  paste("Variable",
        "Type",
        "Categories",
        "Missing_Value",
        sep=","),
  paste("x",
        "x",
        "",
        "",
        sep=","),
  paste("ord1",
        "ordinal",
        "",
        "",
        sep=",")
)
writeLines(lines_to_write,temp_file)
expect_error(
  load_var_info(temp_file),
  "A category specification must be given for each ordinal variable"
)
success <- file.remove(temp_file)

# Confirm an error is thrown if a bad category specification is given for an
# ordinal variable
lines_to_write <- c(
  paste("Variable",
        "Type",
        "Categories",
        "Missing_Value",
        sep=","),
  paste("x",
        "x",
        "",
        "",
        sep=","),
  paste("ord1",
        "ordinal",
        "1 1",
        "",
        sep=",")
)
writeLines(lines_to_write,temp_file)
expect_error(
  load_var_info(temp_file),
  "Error parsing cat_spec = '1 1' for row 2"
)

success <- file.remove(temp_file)

# Confirm the correct result is returned for a four column file
lines_to_write <- c(
  paste("Variable",
        "Type",
        "Categories",
        "Missing_Value",
        sep=","),
  paste("x",
        "x",
        "",
        "",
        sep=","),
  paste("ord1",
        "ordinal",
        "0 1",
        "-1",
        sep=","),
  paste("cont1",
        "continuous",
        "",
        "",
        sep=",")
)
writeLines(lines_to_write,temp_file)
expect_equal(
  load_var_info(temp_file),
  read.csv(temp_file,colClasses="character")
)
success <- file.remove(temp_file)

# Confirm the correct result is returned for an eight column file
lines_to_write <- c(
  paste("Variable",
        "Type",
        "Categories",
        "Missing_Value",
        "Left_Right_Side",
        "Left_Label",
        "Right_Label",
        "Left_Right_Approach",
        sep=","),
  paste("x",
        "x",
        "",
        "",
        "",
        "",
        "",
        "",
        sep=","),
  paste("ord1",
        "ordinal",
        "0 1",
        "-1",
        "end",
        "_L",
        "_R",
        "left",
        sep=","),
  paste("cont1",
        "continuous",
        "",
        "",
        "",
        "",
        "",
        "",
        sep=",")
)
writeLines(lines_to_write,temp_file)
expect_equal(
  var_info <- load_var_info(temp_file),
  read.csv(temp_file,colClasses="character")
)

# Test build_var_names
expect_equal(
  build_var_names(var_info),
  c("x","ord1_L","ord1_R","cont1")
)
success <- file.remove(temp_file)

# Test parse_NA_values
expect_equal(
  parse_NA_values("NA,-1,99"),
  c("NA","-1","99")
)

expect_equal(
  parse_NA_values(""),
  character(0)
)

# Test load_cp_data
lines_to_write <- c(
  paste("x",
        "ord1_L",
        "cont1",
        "",
        sep=","),
  paste("10",
        "1",
        "1.5",
        "-1",
        sep=","),
  paste("4",
        "0",
        "0.5",
        "",
        sep=","),
  paste("14",
        "1",
        "2.5",
        "99",
        sep=",")
)
writeLines(lines_to_write,temp_file)
expect_error(
  load_cp_data(temp_file,var_info),
  "ord1_R is not a column in the data file"
)
success <- file.remove(temp_file)

# Since var_info is needed to test load_cp_data, write a new variable info
# file and make sure it can be loaded
cat_spec <- "{b a} -2 {c d e} 4"
lines_to_write <- c(
  paste("Variable",
        "Type",
        "Categories",
        "Missing_Value",
        "Left_Right_Side",
        "Left_Label",
        "Right_Label",
        "Left_Right_Approach",
        sep=","),
  paste("x",
        "x",
        "",
        "",
        "",
        "",
        "",
        "",
        sep=","),
  paste("ord1",
        "ordinal",
        cat_spec,
        "-1",
        "end",
        "_L",
        "_R",
        #"left",
        "",
        sep=","),
  paste("cont1",
        "continuous",
        "",
        "",
        "",
        "",
        "",
        "",
        sep=",")
)

writeLines(lines_to_write,temp_file)
expect_error(
  var_info <- load_var_info(temp_file),
  NA
)
success <- file.remove(temp_file)

lines_to_write <- c(
  paste("x",
        "ord1_L",
        "ord1_R",
        "cont1",
        sep=","),
  paste("1",
        "b",
        "4",
        "1.5",
        sep=","),
  paste("2",
        "-1",
        "a",
        "2.5",
        sep=","),
  paste("3",
        "c",
        "-2",
        "3.5",
        sep=","),
  paste("4",
        "e",
        "d",
        "4.5",
        sep=",")
)
writeLines(lines_to_write,temp_file)
expect_error(
  cp_data <- load_cp_data(temp_file,var_info),
  NA
)
success <- file.remove(temp_file)

# Create the expected matrix for cp_data$problem$Y with a for loop so that
# different attributes do not create a failure in expect_equal.
Y_expected <- matrix(NA,4,3)
for (row in 1:4) {
  Y_expected[row,] <- as.numeric(cp_data$cp_df[row,
                                              c("ord1_L","ord1_R","cont1")])
}
Y_expected <- t(Y_expected)

expect_equal(
  cp_data,
  list(
    cp_df=data.frame(x=c(1,2,3,4),
                     ord1_L=c(0,NA,2,2),
                     ord1_R=c(3,0,1,2),
                     cont1=c(1.5,2.5,3.5,4.5),
                     stringsAsFactors=FALSE),
    problem=list(x=as.vector(cp_data$cp_df$x),
                 Y=Y_expected,
                 var_names=c("ord1_L","ord1_R","cont1"),
                 mod_spec=list(J=2,K=1,M=c(3,3))
                 )
  )
)

# Test generate_cv_problems
expect_error(
  cv_list <- generate_cv_problems(cp_data$problem, 2, seed=234227327),
  NA
)

expect_equal(
  names(cv_list),
  c("test_list","train_list","seed")
)

expect_equal(
  length(cv_list$test_list),
  2  # 2 folds
)

expect_equal(
  cv_list$seed,
  234227327
)

expect_equal(
  dim(cv_list$train_list[[1]]$Y),
  c(3,2)
)

expect_error(
  cv_list <- generate_cv_problems(cp_data$problem, 2, seed=234227327),
  NA
)

expect_error(
  cv_list2 <- generate_cv_problems(cp_data$problem, 2, seed=234227327),
  NA
)

expect_equal(
  cv_list,
  cv_list2
)

expect_error(
  cv_list3 <- generate_cv_problems(cp_data$problem, 2, seed=234227328),
  NA
)

# In principle cv_list3 could yield the same fold partition as cv_list since
# the fold sizes are small, but the preceding seeds that is not the case.
expect_false(
  all(cv_list$test_list[[1]]$x == cv_list3$test_list[[1]]$x)
)

# Test save_problem
expect_error(
  save_problem(data_dir,"temp",cp_data$problem),
  NA
)

expect_true(
  file.exists(file.path(data_dir,'problem_temp.rds'))
)
success <- file.remove(file.path(data_dir,'problem_temp.rds'))

expect_error(
  save_problem(data_dir,"temp",cv_list, is_folds=T),
  NA
)

expect_true(
  file.exists(file.path(data_dir,'train_temp_fold1.rds'))
)
success <- file.remove(file.path(data_dir,'train_temp_fold1.rds'))

expect_true(
  file.exists(file.path(data_dir,'train_temp_fold2.rds'))
)
success <- file.remove(file.path(data_dir,'train_temp_fold2.rds'))

expect_true(
  file.exists(file.path(data_dir, 'test_temp_fold1.rds'))
)
success <- file.remove(file.path(data_dir,'test_temp_fold1.rds'))

expect_true(
  file.exists(file.path(data_dir, 'test_temp_fold2.rds'))
)
success <- file.remove(file.path(data_dir,'test_temp_fold2.rds'))

expect_false(
  file.exists(file.path(data_dir,'train_temp_fold4.rds'))
)

# Test build_file_path
analysis_name <- "US-analysis"

expect_error(
  build_file_path(data_dir,analysis_name,"bad_file_type"),
  "Unrecognized file_type = bad_file_type"
)

expect_equal(
  build_file_path(data_dir,analysis_name,"main_problem"),
  file.path(tempdir(),"problem_US-analysis.rds")
)

expect_equal(
  build_file_path(data_dir,analysis_name,"test_problem",fold=2),
  file.path(tempdir(),"test_US-analysis_fold2.rds")
)

expect_equal(
  build_file_path(data_dir,analysis_name,"training_problem",fold=2),
  file.path(tempdir(),"train_US-analysis_fold2.rds")
)

expect_equal(
  build_file_path(data_dir,
                  analysis_name,
                  "univariate_ord_soln",
                  j=2,
                  var_name="var_name",
                  mean_spec="log_ord",
                  noise_spec="const"),
  file.path(tempdir(),paste0("solutiony_US-analysis_ord_j_2_",
                             "var_name_log_ord_const.rds"))
)

expect_equal(
  build_file_path(data_dir,
                  analysis_name,
                  "univariate_ord_soln",
                  j=2,
                  var_name="var_name",
                  mean_spec="log_ord",
                  noise_spec="const",
                  fold=2),
  file.path(tempdir(),paste0("solutiony_US-analysis_fold2_ord_j_2_",
                             "var_name_log_ord_const.rds"))
)

expect_equal(
  build_file_path(data_dir,
                  analysis_name,
                  "ordinal_ci",
                  j=2,
                  var_name="var_name"),
  file.path(tempdir(),paste0("ordinal_ci_US-analysis_j_2_",
                             "var_name.rds"))
)

expect_equal(
  build_file_path(data_dir,
                  analysis_name,
                  "univariate_ord_rmd",
                  j=2,
                  var_name="var_name"),
  file.path(tempdir(),"US-analysis_ord_j_2_var_name.Rmd")
)

expect_equal(
  build_file_path(data_dir,
                  analysis_name,
                  "univariate_cont_soln",
                  k=2,
                  var_name="var_name",
                  mean_spec="pow_law",
                  noise_spec="const"),
  file.path(tempdir(),paste0("solutiony_US-analysis_cont_k_2_",
                             "var_name_pow_law_const.rds"))
)

expect_equal(
  build_file_path(data_dir,
                  analysis_name,
                  "univariate_cont_soln",
                  k=2,
                  var_name="var_name",
                  mean_spec="pow_law",
                  noise_spec="const",
                  fold=2),
  file.path(tempdir(),paste0("solutiony_US-analysis_fold2_cont_k_2_",
                             "var_name_pow_law_const.rds"))
)

expect_equal(
  build_file_path(data_dir,
                  analysis_name,
                  "univariate_cont_rmd",
                  k=2,
                  var_name="var_name"),
  file.path(tempdir(),"US-analysis_cont_k_2_var_name.Rmd")
)

expect_equal(
  build_file_path(data_dir,analysis_name,"solutionx"),
  file.path(tempdir(),"solutionx_US-analysis.rds")
)

expect_equal(
  build_file_path(data_dir,analysis_name,"solutionx",fold=2),
  file.path(tempdir(),"solutionx_US-analysis_fold2.rds")
)

expect_equal(
  build_file_path(data_dir,analysis_name,"eval_data"),
  file.path(tempdir(),"eval_data_univariate_US-analysis.rds")
)

expect_equal(
  build_file_path(data_dir,analysis_name,"mcp_inputs"),
  file.path(tempdir(),"mcp_inputs_US-analysis.rds")
)

expect_equal(
  build_file_path(data_dir,analysis_name,"mcp_inputs",fold=2),
  file.path(tempdir(),"mcp_inputs_US-analysis_fold2.rds")
)

expect_equal(
  build_file_path(data_dir,analysis_name,"cindep_model"),
  file.path(tempdir(),"cindep_model_US-analysis.rds")
)

expect_equal(
  build_file_path(data_dir,analysis_name,"hjk_progress"),
  file.path(tempdir(),"hjk_progress_US-analysis.rds")
)

expect_equal(
  build_file_path(data_dir,analysis_name,"hjk_progress",fold=2),
  file.path(tempdir(),"hjk_progress_US-analysis_fold2.rds")
)

expect_equal(
  build_file_path(data_dir,analysis_name,"cdep_model"),
  file.path(tempdir(),"cdep_model_US-analysis.rds")
)

expect_equal(
  build_file_path(data_dir,analysis_name,"cdep_model",fold=2),
  file.path(tempdir(),"cdep_model_US-analysis_fold2.rds")
)

# Test yada::build_univariate_ord_problems
problem <- list()
problem$x <- 0:9
problem$Y <- t(as.matrix(cbind(c(0,0,0,0,1,1,1,2,2,2),
                               c(0,0,0,0,1,1,1,2,2,2),
                               2*problem$x+1 + 0.1*rnorm(10),
                               2*problem$x+1 + 0.1*rnorm(10),
                               2*problem$x+1 + 0.1*rnorm(10))))
problem$var_names <- c('ord1','ord2','cont1','cont2','cont3')
problem$mod_spec$J <- 2
problem$mod_spec$K <- 3
problem$mod_spec$M <- c(2,2)

# Make a reduced-size fold problem
fold_problem <- list()
fold_problem$x <- 0:7
fold_problem$Y <- t(as.matrix(cbind(c(0,0,0,0,1,1,1,2),
                                    c(0,0,0,0,1,1,1,2),
                                    2*fold_problem$x+1 + 0.1*rnorm(8),
                                    2*fold_problem$x+1 + 0.1*rnorm(8),
                                    2*fold_problem$x+1 + 0.1*rnorm(8))))
fold_problem$var_names <- c('ord1','ord2','cont1','cont2','cont3')
fold_problem$mod_spec$J <- 2
fold_problem$mod_spec$K <- 3
fold_problem$mod_spec$M <- c(2,2)

saveRDS(problem, build_file_path(data_dir,analysis_name, "main_problem"))
expect_error(
  ord_problems <- build_univariate_ord_problems(data_dir, analysis_name),
  NA
)

for (i in 1:length(ord_problems)) {
  expect_equal(
    length(ord_problems[[i]]$x),
    10
  )
  expect_equal(
    length(ord_problems[[i]]$v),
    10
  )
}

expect_equal(
  length(ord_problems),
  6*2
)

expect_error(
  ord_problems <- build_univariate_ord_problems(data_dir,
                                                analysis_name,
                                                mean_specs="lin_ord",
                                                noise_specs="const"),
  NA
)

for (i in 1:length(ord_problems)) {
  expect_equal(
    length(ord_problems[[i]]$x),
    10
  )
  expect_equal(
    length(ord_problems[[i]]$v),
    10
  )
}

expect_equal(
  length(ord_problems),
  1*2
)

saveRDS(fold_problem, build_file_path(data_dir,
                                 analysis_name,
                                 "training_problem",fold=1))
saveRDS(fold_problem, build_file_path(data_dir,
                                 analysis_name,
                                 "training_problem",fold=2))
expect_error(
  ord_problems <- build_univariate_ord_problems(data_dir,
                                                analysis_name,
                                                add_folds=TRUE),
  NA
)

for (i in 1:length(ord_problems)) {
  if (i <= 12) {
    # Main problem has ten observations
    num_obs <- 10
  } else {
    # Fold problems have eight observations (which would not be the number of
    # observations, 5, if the cross-validation were generated with
    generate_cv_problems
    num_obs <- 8
  }
  expect_equal(
    length(ord_problems[[i]]$x),
    num_obs
  )
  expect_equal(
    length(ord_problems[[i]]$v),
    num_obs
  )
}

expect_equal(
  length(ord_problems),
  6*2*3
)

expect_error(
  ord_problems <- build_univariate_ord_problems(data_dir,
                                                analysis_name,
                                                mean_specs="lin_ord",
                                                noise_specs="const",
                                                add_folds=TRUE),
  NA
)

for (i in 1:length(ord_problems)) {
  if (i <= 2) {
    # Main problem has ten observations
    num_obs <- 10
  } else {
    # Fold problems have eight observations (which would not be the number of
    # observations, 5, if the cross-validation were generated with
    generate_cv_problems
    num_obs <- 8
  }
  expect_equal(
    length(ord_problems[[i]]$x),
    num_obs
  )
  expect_equal(
    length(ord_problems[[i]]$v),
    num_obs
  )
}

expect_equal(
  length(ord_problems),
  1*2*3
)

# Test yada::solve_ord_problem
expect_equal(
  solve_ord_problem(data_dir, analysis_name, ord_problems[[1]]),
  TRUE
)

# The functioning of the seed is checked elsewhere
expect_equal(
  solve_ord_problem(data_dir, analysis_name, ord_problems[[1]],10),
  TRUE
)

# Test yada::build_univariate_cont_problems
expect_error(
  cont_problems <- build_univariate_cont_problems(data_dir, analysis_name),
  NA
)

for (i in 1:length(cont_problems)) {
  expect_equal(
    length(cont_problems[[i]]$x),
    10
  )
  expect_equal(
    length(cont_problems[[i]]$w),
    10
  )
}

expect_equal(
  length(cont_problems),
  2*3
)

expect_error(
  cont_problems <- build_univariate_cont_problems(data_dir,
                                                  analysis_name,
                                                  mean_specs="pow_law",
                                                  noise_specs="const"),
  NA
)

for (i in 1:length(cont_problems)) {
  expect_equal(
    length(cont_problems[[i]]$x),
    10
  )
  expect_equal(
    length(cont_problems[[i]]$w),
    10
  )
}

expect_equal(
  length(cont_problems),
  1*3
)

expect_error(
  cont_problems <- build_univariate_cont_problems(data_dir,
                                                  analysis_name,
                                                  add_folds=TRUE),
  NA
)

for (i in 1:length(cont_problems)) {
  if (i <= 6) {
    # Main problem has ten observations
    num_obs <- 10
  } else {
    # Fold problems have eight observations (which would not be the number of
    # observations, 5, if the cross-validation were generated with
    generate_cv_problems
    num_obs <- 8
  }
  expect_equal(
    length(cont_problems[[i]]$x),
    num_obs
  )
  expect_equal(
    length(cont_problems[[i]]$w),
    num_obs
  )
}

expect_equal(
  length(cont_problems),
  2*3*3
)

expect_error(
  cont_problems <- build_univariate_cont_problems(data_dir,
                                                  analysis_name,
                                                  mean_specs="pow_law",
                                                  noise_specs="const",
                                                  add_folds=TRUE),
  NA
)

for (i in 1:length(cont_problems)) {
  if (i <= 3) {
    # Main problem has ten observations
    num_obs <- 10
  } else {
    # Fold problems have eight observations (which would not be the number of
    # observations, 5, if the cross-validation were generated with
    generate_cv_problems
    num_obs <- 8
  }
  expect_equal(
    length(cont_problems[[i]]$x),
    num_obs
  )
  expect_equal(
    length(cont_problems[[i]]$w),
    num_obs
  )
}

expect_equal(
  length(cont_problems),
  1*3*3
)

# Test yada::solve_cont_problem
expect_equal(
  solve_cont_problem(data_dir, analysis_name, cont_problems[[1]]),
  TRUE
)

# Test yada::build_model_vec
expect_equal(
  build_model_vec(c('pow_law_ord','log_ord'),'const'),
  c('pow_law_ord_const','log_ord_const')
)

expect_equal(
  build_model_vec('pow_law',c('const','lin_pos_int')),
  c('pow_law_const','pow_law_lin_pos_int')
)




