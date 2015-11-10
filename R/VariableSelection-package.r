#' Identify the set of predictors of a binary outcome for diagnostic/ptognostic situations.
#' @name VariableSelection-package
#' @description Several methods for variable selection in diagnostic/prognostic classification have been proposed in the literature. This package enables users to identify the appropriate set of predictors of a binary outcome of interest for diagnostic/ptognostic situations. Various approaches for selecting variables have been implemented, including Filter andWrapper approaches. Numerical and graphical output can be easily obtained. The package attempts to provide user-friendly outputs for those who are interested only in the results.
#' @details
#' \tabular{ll}{
#' Package: \tab VariableSelection \cr
#' Type: \tab Package\cr
#' Version: \tab 1.0-0\cr
#' Date: \tab 2015-10-25\cr
#' License: \tab GPL-2\cr
#' }
#' Selecting a small number of highly predictive variables generally helps to avoid over fitting and results in more efficient, applicable and comprehensible model. A large number of variable selection methods have been developed; nevertheless, there is no general consensus on the one which performs well under all conditions. Thus, for each given dataset, the best method should be chosen specifically. Unfortunately, due to unavailability of a lot of these methods in user-friendly programs, applied researchers mostly resort to easy-to-run methods and many of the effective methods remain unused.
#' The VariableSelection package has been designed to incorporate many such methods with a clear and user-friendly output for the end-user. The only requirement to run the command is a data frame with diagnostic/prognostic predictors/markers and the outcome as columns and records/cases/individuals/patients as the rows. As the applied researcher gets more familiar with the package and the methodology, s/he can tune various available parameters used in methods. Although the package was developed for clinical practice in the first place, it can be applied to data sets from all fields.
#' The most important functions in the package are the \code{variable.selection()}, \code{control.selection()}, \code{print.variable.selection()} and \code{plot.variable.selection()} functions. The \code{variable.selection()} function select the appropriate sets of variables according to the specified variable selection methods. More than one method can be chosen for selecting the set of variables. The \code{control.selection()} function is used to set several parameters that are specific of each method, such as number of trees in the Random Forest approach or the classification model for which the variables are selected. The \code{print.variable.selection()} and \code{plot.variable.selection()} functions produce visual friendly outputs.
#' @author Farideh Bagherzadeh-Khiabani, Davood Khalili
#' @docType package
#' @import discretization
NULL
