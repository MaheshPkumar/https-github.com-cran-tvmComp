#' Computing Present Value of an Ordinary Perpetuity or Perpetuity Due.
#' @details
#' According to \insertCite{humm;textual}{tvmComp}, a Perpetuity is an annuity for which the payments continue forever. When the size of the periodic payment from a fund is equal to the periodic interest earned by the fund, such as an endowment fund to a university or a continuous benefit from a capital investment, a Perpetuity gets resulted.
#'The method \code{pvAnytypePerpetuity()} is developed to compute Present Value(PV) of an Ordinary Perpetuity or Perpetuity Due.The method gives Present Value when values are passed to its three arguments. Here \code{r} is annual rate, \code{pmt} is amount of one annuity and \code{bgn} is the computational mode. (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments)
#'@param r A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to three arguments \code{r} , \code{pmt} and \code{bgn}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'pvAnytypePerpetuity( 0.08, 4.5, 1)
#'pvAnytypePerpetuity( 0.08, 4.5, 0)
#'@export
pvAnytypePerpetuity <- function (r, pmt,bgn)
{
   if (bgn != 0 && bgn != 1) {
   message("Error: bgn should be 0 or 1")
  }
  else {
    (pv_AnnuityAny=round((pmt/r ) * ((1 + r)^bgn) * (-1), digits=2))
  }
}

