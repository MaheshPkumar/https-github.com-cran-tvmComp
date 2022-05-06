#' Computing Future Value of a Single Cash Flow for Semi-Annual Compounding Frequency.
#' @details
#'The method \code{fvSemiAnnualCompSingleCF()} is developed to Compute Future Value of a Single Cash Flow under Semi-Annual Compounding Frequency.The method gives FVFuture Value when values are passed to its three arguments. Here \code{r} is annual rate, \code{n} is number of years and \code{pv} is Present Value.
#'@param r A number.
#'@param n A number.
#'@param pv A number.
#'@return Input values to three arguments \code{r} , \code{n} and \code{pv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'fvSemiAnnualCompSingleCF(0.08,10,2000)
#'fvSemiAnnualCompSingleCF(0.08,10,-2000)
#'@export
fvSemiAnnualCompSingleCF <- function (r, n, pv)
{
  fv_SemiAnnualSingleCF <- ((pv * (1 + r/2)^(n*2)) * (-1))
   (fv_SemiAnnualSingleCF=round(fv_SemiAnnualSingleCF, digits=2))
}

