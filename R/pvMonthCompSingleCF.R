#' Computing Present Value of a Single Cash Flow under Monthly Compounding Frequency.
#'@details
#'The method \code{pvMonthlyCompSingleCF()} is developed to compute Present Value of a Single Cash Flow under Monthly Compounding Frequency.The method gives Present Value when values are passed to its three arguments. Here \code{r} is annual rate, \code{n} is number of years and \code{fv} is Future Value.
#'@param r A number.
#'@param n A number.
#'@param fv A number.
#'@return Input values to three arguments \code{r} , \code{n} and \code{fv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'pvMonthlyCompSingleCF(0.08,10,2000)
#'pvMonthlyCompSingleCF(0.08,10,-2000)
#'@export
pvMonthlyCompSingleCF <- function (r, n, fv)
{
  pv_MonthlyCompsingleCF <- ((fv/(1 + r/12)^(n*12)) * (-1))
   (pv_MonthlyCompsingleCF = round(pv_MonthlyCompsingleCF, digits=2))
}


