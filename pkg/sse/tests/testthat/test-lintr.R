## context("lintr")



## # "lintr_trailing_semicolon_linter" fails in bad1
## # "lintr_attach_detach_linter"  fails in bad2
## # "lintr_setwd_linter"  fails in bad2
## # "lintr_sapply_linter"  fails in bad2
## # "lintr_library_require_linter" fails in bad2
## # "lintr_seq_linter" fails in bad3
## # not failing yet:
## # "lintr_assignment_linter"
## # "lintr_line_length_linter" 

## gp_lintrs <- c("lintr_assignment_linter", "lintr_line_length_linter", 
##                "lintr_trailing_semicolon_linter", 
##                "lintr_attach_detach_linter", "lintr_setwd_linter", 
##                "lintr_sapply_linter", "lintr_library_require_linter", 
##                "lintr_seq_linter")

## bad1 <- system.file("bad1", package = "goodpractice")
## gp_bad1 <- gp(bad1, checks = gp_lintrs)
## res_bad1 <- results(gp_bad1)

## gp_bad2 <- gp("bad2", checks = gp_lintrs)
## res_bad2 <- results(gp_bad2)

## gp_bad3 <- gp("bad3", checks = gp_lintrs)
## res_bad3 <- results(gp_bad3)

## get_result <- function(res, check) res$result[res$check == check]



## test_that("lintr_assignment_linter", {
  
##   expect_true(get_result(res_bad1, "lintr_assignment_linter"))
##   # TODO expectation/example where the check fails
  
## })


## library(lintr)
## test_that("code quality and style", {
## #  skip_if_not_installed("lintr")
## #  skip_on_travis()
## #  skip_on_cran()
##   lintr::expect_lint_free(linters = with_defaults(
##                               line_length_linter(120)
##                           ))
## })


## context("lints")

## ## #if (requireNamespace("lintr", quietly = TRUE)) {


## ## library(lintr)

## ## # enforce camelCase rather than snake_case
## ## with_defaults(camel_case_linter = NULL,
## ##               snake_case_linter)
## ##                                         # change the default line length cutoff
## ## with_defaults(line_length_linter = line_length_linter(120))

## ## lint("power.R")
## ## , linters =  list(line_length_linter(80)))


## test_that("Package Style", {
## ##     lintr::expect_lint_free(#linters = list(
## ##                             #    e = lintr::line_length_linter(100))
## ##                             )
## ##   })
## ## #}
## lintr::lint_package("sse", 
##              linters = with_defaults(
##                  ## absolute_paths_linter = NULL, # checks that no absolute paths are used.
##                  ## assignment_linter = NULL, #checks that '<-' is always used for  assignment.
##                  ## closed_curly_linter = NULL, #check that closed curly braces should always be on their own line unless they follow an else.
##                  ## commas_linter = NULL, #check that all commas are followed byspaces, but do not have spaces before them.
##                  commented_code_linter = NULL, #checks that there is no commented code outside roxygen blocks
##                  ## infix_spaces_linter = NULL, #check that all infix operators have spaces around them.
##    #              line_length_linter(120), #check the line length of both comments and code is less than length.
##                  ## no_tab_linter = NULL, #check that only spaces are used, never tabs.
##                  ## object_usage_linter = NULL, #checks that closures have the proper usage using ‘checkUsage’.  Note this runs ‘eval’ on the code, so do not use with untrusted code.
##                  camel_case_linter = NULL, #check that objects are not in camelCase.
##                  snake_case_linter = NULL, #check that objects are not in snake_case.
##                  multiple_dots_linter = NULL, #check that objects do not have multiple.dots.
##    #              object_length_linter = NULL, #check that objects do are not very long.not have.multiple.dots.
##                  ## open_curly_linter = NULL, #check that opening curly braces are never on their own line and are always followed by a newline.
##                  ## single_quotes_linter = NULL, #checks that only single quotes are used to delimit string contestants.
##                  ## spaces_inside_linter = NULL, #check that parentheses and square brackets do not have spaces directly inside them.
##                  ## spaces_left_parentheses_linter = NULL, #check that all left parentheses have a space before them unless they are in a function call.
##                  ## trailing_blank_lines_linter = NULL, #check there are no trailing blank lines.
##                  ## trailing_whitespace_linter = NULL, #check there are no trailing whitespace characters.
##                  object_camel_case_linter = NULL
##   )
## )
## })
