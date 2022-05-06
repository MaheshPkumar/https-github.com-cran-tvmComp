#' Computing Present Value of a Single Cash Flow with Yearly Compounding.
#'@details
#'As \insertCite{brook;textual}{tvmComp} points out that when we are interested in computing the current value of something that we will receive in the future for that the concept called Present Value is used. He further comments that Present Value is a bit trickier to understand, but it helps us put a price or value today on a future cash flow. To get quantitative estimate of the Present Value, you can use the method \code{pvYearlyCompSingleCF()} that computes for you Present Value of a Single Cash Flow with Yearly Compounding Frequency.
#'The method \code{pvYearlyCompSingleCF()} gives Present Value when values are passed to its three arguments. Here \code{r} is annual rate,  \code{n} is number of years and \code{fv} is Future Value.
#'@param r A number.
#'@param n A number.
#'@param fv A number.
#'@return Input values to three arguments \code{r} , \code{n} and \code{fv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{brook}{tvmComp}
#'@examples
#'pvYearlyCompSingleCF(0.08,10,2000)
#'pvYearlyCompSingleCF(0.08,10,-2000)
#'@export
pvYearlyCompSingleCF <- function (r, n, fv)
{
  pv_ysingleCF <- ((fv/(1 + r)^n) * (-1))
  (pv_ysingleCF= round(pv_ysingleCF, digits=2))
}



