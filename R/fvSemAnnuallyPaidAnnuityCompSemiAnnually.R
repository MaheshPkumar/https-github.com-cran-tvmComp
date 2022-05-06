#'Calculates Future Value of Semi-Annual Paid Ordinary Annuity or Annuity Due that is Compounded Semi-Annually.
#'@details
#'According to \insertCite{humm;textual}{tvmComp}, an Annuity is a series of payments, usually of equal size, made at periodic time intervals. Various types of annuities are identified based on the term of an annuity, the date of payment and the length of the interest compounding or conversion period. The term Annuity applies to all periodic payment plans, the most frequent of which require annual, semi-annual, quarterly, or monthly payments. Practical applications of annuities are widely encountered in the finances of both businesses and individuals. Periodic contributions to student loan payments, car loan payments, and mortgage payments are common examples of Annuities in personal finance. Businesses may encounter Annuities in the form of equipment loans, mortgages, lease contracts and bond interest payments.
#'The method \code{fvSemiAnnuallyPaidAnnuityCompSemiAnnually()} is developed to calculates Future Value of Semi-Annually Paid Ordinary Annuity or Annuity Due that is Compounded Semi-Annually. The method gives Future Value when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pmt} is amount of one annuity and \code{bgn} is the mode (1 when annuity payment occurs at the beginning of the period; 0 for end of period payments)
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
#'fvSemiAnnuallyPaidAnnuityCompSemiAnnually(0.08,10,-50,1)
#'fvSemiAnnuallyPaidAnnuityCompSemiAnnually(0.08,10,-50,0)
#'fvSemiAnnuallyPaidAnnuityCompSemiAnnually(.08,10,50,1)
#'fvSemiAnnuallyPaidAnnuityCompSemiAnnually(.08,10,50,0)
#'@export
fvSemiAnnuallyPaidAnnuityCompSemiAnnually <- function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
    (fv_saAnnuitySa=round((pmt/(r/2) * ((1 + r/2)^(n*2) - 1)) * (1 + r/2)^bgn * (-1), digits=2))
  }
}



