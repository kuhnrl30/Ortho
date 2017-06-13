#' Process raw FIT data for computation
#' 
#' Preprocess an extract from the FIT financial reporting system and prepare
#' it for further analysis.  The basic premise is to clean the column headers
#' and remove any extraneous leading characters.
#' 
#' The data extract is created by using transaction code \code{
#' s_pl0_86000030}.  In the search paramenter input screen, use the following 
#' settings:
#' 
#' \itemize{
#' \item{Company Code: }{1001, 1002}
#' \item{Function: }{2001}
#' \item{Ledger: }{0L}
#' \item{Currency Type: }{10}
#' }
#' 
#' @param file path to .csv file extracted from FIT.
#' 
#' @export
#' @import dplyr

FormatFITReport<- function(file){

  dat<- readxl::read_excel(file)
  
  # The column headers span 2 rows in the excel extract so must also be
  # picked up from the column names and the first row of the dataframe.
  heads<- c(unlist(dat[1,1:10]),colnames(dat[,11:18]))
  dat2<- as.data.frame(dat[-1,])
  
  # Clean up headers and save as column names
  heads[14:15]<- c("debits","credits")
  heads<- gsub("[[:space:]]", "", heads)
  heads<- gsub("/","",heads, fixed=T)
  names(dat2)<- heads
  
  dat2[,11:18]<- lapply(dat2[,11:18], as.numeric)
  
  dat3<- dat2 %>%
    mutate(Month= debits - credits) %>%
    filter(Month != 0) %>%
    mutate(AccountNumber= substr(AccountNumber, 6, 13),
           ProfitCenter= substr(ProfitCenter, 6,13),
           CompanyCode= substr(CompanyCode, 1,4),
           FunctionalArea= substr(FunctionalArea, 1,4),
           CostCenterNo= gsub("[[:alpha:]]|[[:punct:]]|[[:blank:]]", "",CostCenter),
           CostCenter= trimws(gsub("OCDG/","",CostCenter)),
           Period= month.abb[as.numeric(gsub("/.*","",Periodyear))]) %>%
    select(CompanyCode, FunctionalArea, AccountNumber,   
           ProfitCenter, CostCenter, CostCenterNo, Month, Period)
  dat3
}
