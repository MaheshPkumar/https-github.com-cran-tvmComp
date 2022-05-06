#'Calculates Present Value of a Single Cash Flow that is Compounded Continuously.
#'@details
#'According to \insertCite{tman;textual}{tvmComp}, a continuously compounding scenario is that where the time intervals between interest payments are infinitely small.
#'The Method \code{pvContinuousCompSingleCF()} was developed for this scenario and calculates Present Value of a Single Cash Flow that is Compounded Continuously. The method gives Present Value when values are passed to its three arguments. Here \code{r} is annual rate, \code{n} is number of years and \code{fv} is Future Value
#'@param r A number.
#'@param n A number.
#'@param fv A number.
#'@return Input values to three arguments \code{r} , \code{n} and \code{fv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'pvContinuousCompSingleCF(0.1854,1,3250)
#'pvContinuousCompSingleCF(0.1854,1,-3250)
#'@export
pvContinuousCompSingleCF <- function (r, n, fv)
{
  pv_ContinuousCompsingleCF <- (1/(exp(r*n) )*fv* (-1))
  (pv_ContinuousCompsingleCF= round(pv_ContinuousCompsingleCF, digits=2))
}


