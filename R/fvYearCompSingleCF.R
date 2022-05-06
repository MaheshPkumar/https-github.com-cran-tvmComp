#' Computing Future Value of a Single Cash Flow with Yearly Compounding Frequency.
#'@details
#'As \insertCite{brook;textual}{tvmComp} explains Future Value  is the cash value of an money or an asset in the future that is equivalent in value to a specific amount today. This, acts as basis of understanding how interest accumulates (and gets compounded) and how the Time Value of Money equation can be used for estimating growth. On these lines, \code{fvYearlyCompSingleCF()} was developed and it calculates Future Value of a Single Cash Flow with Yearly Compounding Frequency.
#'\code{fvYearlyCompSingleCF()} gives Future Value when values are passed to its three arguments. Here \code{r} is annual rate,  \code{n} is number of years and \code{pv} is Present Value
#'@param r A number.
#'@param n A number.
#'@param pv A number.
#'@return Input values to three arguments \code{r} , \code{n} and \code{pv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{brook}{tvmComp}
#'@examples
#'fvYearlyCompSingleCF(0.08,10,2000)
#'fvYearlyCompSingleCF(0.08,10,-2000)
#'@export
fvYearlyCompSingleCF <- function (r, n, pv)
{
  fv_YearlyCompSingleCF <- ((pv * (1 + r)^n) * (-1))
  (fv_YearlyCompSingleCF=round(fv_YearlyCompSingleCF, digits=2))
}



