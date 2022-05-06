#' Computing Present Value of a Single Cash Flow for Quarterly Compounding.
#'@details
#'As explained by \insertCite{humm;textual}{tvmComp}, in simple interest calculations, the stated rate of interest, r, is usually expressed as an annual rate and that is also called the nominal rate of interest. In Compound Interest calculations, the periodic rate of interest is determined by dividing the nominal annual rate of interest by the compounding periods per year.The compounding periods commonly used in business and finance cover a number of months, or a number of days, quarters, or semi-annual periods or annual periods. Mortgages and car loans include weekly and bi-weekly payment options.
#'In this light, the method \code{pvQuarterlyCompSingleCF()} was developed for Quarterly compounding scenario and it gives Present Value when values are passed to its three arguments. Here \code{r} is annual rate, \code{n} is number of years and \code{fv} is Future Value
#'@param r A number.
#'@param n A number.
#'@param fv A number.
#'@return Input values to three arguments \code{r} , \code{n} and \code{fv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'pvQuarterlyCompSingleCF(0.08,10,2000)
#'pvQuarterlyCompSingleCF(0.08,10,-2000)
#'@export
pvQuarterlyCompSingleCF <- function (r, n, fv)
{
  pv_QuarterlyCompsingleCF <- ((fv/(1 + r/4)^(n*4)) * (-1))
  (pv_QuarterlyCompsingleCF=round(pv_QuarterlyCompsingleCF, digits=2))
}



