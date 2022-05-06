#' Computing Rate of Return of a Perpetuity.
#'@details
#'As defined by \insertCite{humm;textual}{tvmComp}, a Perpetuity is an annuity in which the periodic payments begin on a fixed date and continue indefinitely. Interest payments on permanently invested sums of money are prime examples of Perpetuity. Dividends on preferred shares fall into this category if the issuing corporation has an indefinite life. Scholarships and trust funds paid perpetually from an endowment also fit the definition of Perpetuity.
#'The method \code{rorPerpetuity()} is developed to compute Rate of Return(r) of a Perpetuity and gives \code{r} when values are passed to its two arguments. Here \code{yearendCF} is Cash Flow occurring at the Year-End and \code{deposit} stands for amount of the Deposit
#'@param yearendCF A number.
#'@param deposit A number.
#'@return Input values to two arguments \code{yearendCF} and \code{deposit}
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'rorPerpetuity(15, 180)
#'rorPerpetuity(10, 120)
#'@export
rorPerpetuity <- function (yearendCF, deposit)
{
  (thisror = round((yearendCF/deposit ), digits=3))
}


