#' Computing Present Value of Yearly Paid Ordinary Annuity or Annuity Due that is Compounded Monthly.
#'@details
#'As explained by \insertCite{humm;textual}{tvmComp}, the description of the Compounding Frequency is usually contained within the phrase that describes the nominal interest rate. An example would be 24 percent compounded monthly. This means that the nominal, or annual, interest rate of 24 percent is compounded 12 times each year at 24/12 that is 2 percent, each period.
#'The method \code{pvYearlyAnnuityCompMonthly()} is developed to compute Present Value of Yearly-Paid Ordinary Annuity or Annuity Due that is Compounded Monthly.The method gives Present Value when values are passed to its four arguments. Here \code{r} is annual rate,\code{n} is number of years,\code{pmt} is amount of one annuity and \code{bgn} is the computational mode. (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments).
#'@param r A number.
#'@param n A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pmt} and \code{bgn}
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'pvYearlyAnnuityCompMonthly(0.11,4,-300,0)
#'pvYearlyAnnuityCompMonthly(0.11,4,-300,1)
#'pvYearlyAnnuityCompMonthly(0.11,4,300,0)
#'pvYearlyAnnuityCompMonthly(0.11,4,300,1)
#'@export
pvYearlyAnnuityCompMonthly <-function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
        r <-(1+r/12) ^ 12 -1
        (pv_yAnnuitymonthly=round((pmt/r * (1 - 1/(1 + r)^n)) * (1 + r)^bgn * (-1), digits=2))
  }
}


