#'Calculates Future Value of Yearly Paid Ordinary Annuity or Annuity Due that is Compounded Monthly.
#'@details
#'According to \insertCite{humm;textual}{tvmComp} Annuity is a series of equal payments, made at periodic intervals. The length of time between the successive payments is called the payment interval or payment period. The length of time from the beginning of the first payment interval to the end of the last payment interval is called the term of an annuity. The amount of each of the regular payments is called the periodic payment or periodic rent.
#'The method \code{fvYearlyAnnuityCompMonthly()}is developed to calculate Future Value of Yearly-Paid Ordinary Annuity or Annuity Due that is Compounded Monthly.The method gives Future Value when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pmt} is amount of one annuity and \code{bgn} is the computational mode. (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments)
#'@param r A number.
#'@param n A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pmt} and \code{bgn}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'fvYearlyAnnuityCompMonthly(0.11,4,-300,0)
#'fvYearlyAnnuityCompMonthly(0.11,4,-300,1)
#'fvYearlyAnnuityCompMonthly(0.11,4,300,0)
#'fvYearlyAnnuityCompMonthly(0.11,4,300,1)
#'@export
fvYearlyAnnuityCompMonthly <- function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
    # compute Effective Annual Rate(yield) for yearly payments that are compounded Monthly
     eay <-(1+r/12) ^ 12 -1
   (fv_yAnnuityMonthly=round((pmt/eay * ((1 + eay)^n - 1)) * (1 + eay)^bgn * (-1), digits=2))
  }
}




