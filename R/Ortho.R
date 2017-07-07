#' Ortho
#'
#' Providing common functions for working with financial data
#' at Ortho Clinical Diagnostics. The financial data is often 
#' exported from FIT- an Oracle general ledger system. The
#' extracts often do not have formatting conducive to analysis
#' in R so require additional munging. This package includes 
#' the convenience functions to perform this common task.
#'
#' @docType package
#' @name Ortho
#' @author Ryan Kuhn
#' @importFrom readxl "read_excel"
#' @import dplyr
#' @import reshape2
#' @import ggplot2
#' @import grDevices
NULL