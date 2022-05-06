#'Computing Present Value of a Single Cash Flow for Semi-Annual Compounding.
#'@details
#'The method \code{pvSemiAnnualCompSingleCF()} is developed to compute Present Value of a Single Cash Flow for Semi-Annual Compounding Frequency and gives Present Value when values are passed to its three arguments. Here \code{r} is annual rate, \code{n} is number of years and \code{fv} is Future Value.
#'@param r A number.
#'@param n A number.
#'@param fv A number.
#'@return Input values to three arguments \code{r} , \code{n} and \code{fv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'pvSemiAnnualCompSingleCF(0.08,10,2000)
#'pvSemiAnnualCompSingleCF(0.08,10,-2000)
#'@export
pvSemiAnnualCompSingleCF <- function (r, n, fv)
{
  pv_SemiAnnualCompsingleCF <- ((fv/(1 + r/2)^(n*2)) * (-1))
  (pv_SemiAnnualCompsingleCF= round(pv_SemiAnnualCompsingleCF, digits=2))
}

