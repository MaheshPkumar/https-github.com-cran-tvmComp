#'Calculates Present Value of a Single Cash Flow for Daily Compounding Frequency.
#'@details
#'As defined by \insertCite{brook;textual}{tvmComp}, Present Value is the value today of tomorrow’s cash flow. Computation of the equivalent value of a Future Value in today’s dollars, is done by discounting the Future Value back to the Present Value at a given discount rate.
#'The method \code{pvDailyCompSingleCF()} is developed to calculates Present Value of a Single Cash Flow for daily Compounding Frequency.The method gives Present Value when values are passed to its three arguments. Here \code{r} is annual rate,  \code{n} is number of years and \code{fv} is Future Value
#'@param r A number.
#'@param n A number.
#'@param fv A number.
#'@return Input values to three arguments  \code{r} , \code{n} and \code{fv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{brook}{tvmComp}
#'@examples
#'pvDailyCompSingleCF(0.08,10,-2000)
#'pvDailyCompSingleCF(0.08,10,2000)
#'@export
pvDailyCompSingleCF <- function (r, n, fv)
{
  pv_DailyCompsingleCF <- ((fv/(1 + r/365)^(n*365)) * (-1))
  (pv_DailyCompsingleCF= round(pv_DailyCompsingleCF, digits=2))
}


