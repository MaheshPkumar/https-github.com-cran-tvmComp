#' Computing Future Value of a Single Cash Flow for Quarterly Compounding Frequency.
#' @details
#' According to \insertCite{brook;textual}{tvmComp},Future Value is the value of an asset at a specific point in time in the future that is equivalent in value to a specific amount today. So there exists a direct relationship between the Future Value of an asset and the asset’s Present Value, growth rate and time to the future point. Future values grow faster due to the fact that interest also keeps on earning interest, a phenomenon called Compounding of Interest.
#'The method \code{fvQuarterlyCompSingleCF()} is developed to compute Future Value of a Single Cash Flow for Quarterly Compounding Frequency.The method gives Future Value when values are passed to its three arguments. Here \code{r} is annual rate, \code{n} is number of years and \code{pv} is Present Value.
#'@param r A number.
#'@param n A number.
#'@param pv A number.
#'@return Input values to three arguments \code{r} , \code{n} and \code{pv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{brook}{tvmComp}
#'@examples
#'fvQuarterlyCompSingleCF(0.08,10,2000)
#'fvQuarterlyCompSingleCF(0.08,10,-2000)
#'@export
fvQuarterlyCompSingleCF <- function (r, n, pv)
{
  fv_QuarterlySingleCF <- ((pv * (1 + r/4)^(n*4)) * (-1))
   (fv_QuarterlySingleCF=round(fv_QuarterlySingleCF, digits=2))
}


