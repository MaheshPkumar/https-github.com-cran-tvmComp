#'Calculates Future Value of a Single Cash Flow for Continuous Compounding.
#'@details
#'According to \insertCite{tman;textual}{tvmComp}, when interest is earned on both the initial principal and the reinvested interest during prior periods, the result is called Compound Interest. This process of accumulating interest on an investment over multiple time periods is called Compounding. As the number of compounding periods per year increases so does the effective annual rate (EAR). This is so because the greater the number of compounding periods is, the more often interest is earned on interest. Based on this, you can easily compute the EAR when interest is compounded daily (that is compounded 365 times). We can just as easily calculate the EAR if the interest is compounded every hour (that is compounded 8,760 times), every minute (that is compounded 525,600 times), or every second (that is compounded 31,536,000 times). We can even calculate the EAR when interest is continuously compounded, that is, when the time intervals between interest payments are infinitely small.
#'The method \code{fvContinuousCompSingleCF()} is developed to calculate Future Value of a Single Cash Flow  under continuous compounding and gives you a future value when values are passed to its three arguments. Here, \code{r} is annual rate, \code{n} is number of years and \code{pv} is Present Value.
#'The output gives Future Value of a Single Cash Flow under continuous compounding, that is rounded off to two decimal places. A minus value like -206.09 represents Outflow of the Cash. Whereas a positive value like 206.09 represents an Inflow of the cash.
#'@param r A number.
#'@param n A number.
#'@param pv A number.
#'@return Input values to three arguments \code{r} , \code{n} and \code{pv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{tman}{tvmComp}
#'@examples
#'fvContinuousCompSingleCF(0.015,2,200)
#'fvContinuousCompSingleCF(0.015,2,-200)
#'@export
fvContinuousCompSingleCF <- function (r, n, pv)
{
  fv_ContinuousSingleCF <- (exp(r*n)*pv* (-1))
  (fv_ContinuousSingleCF = round(fv_ContinuousSingleCF, digits=2))
}


