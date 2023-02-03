#' Restricts `scan()` input to a specified format
#'
#' @param prompt string: prompt for user input
#' @param type string: required format for input
#'
#' @return user input in specified format
#' @export
#'
#' @examples
#' \dontrun{  #Because scan2() is interactive
#' character <- scan2(prompt = "Type any character", type = "character")
#' numeric <- scan2(prompt = "Type any number", type = "numeric")
#' integer <- scan2(prompt = "Type any number", type = "integer")
#' custom <- scan2(prompt = "Type any number", type = c("Y","N"))
#' }
scan2 <- function(prompt, type) {
  cat(prompt)
  answer <- scan(nmax = 1, what = "character", sep = "#", quiet = TRUE)

  #Character input
  if (length(type)==1) {if (type == "character") {
    return(answer)
    }}

  #Numeric input
  if (length(type)==1) {if (type == "numeric") {
    while (!suppressWarnings(!is.na(as.numeric(answer))) &  #Not a number
           !is.na(answer)) {                                #Not NA
    cat("Please enter a number.")
    answer <- scan(nmax = 1, what = "character", sep = "#", quiet = TRUE)
    }
  answer <- as.numeric(answer)
  return(answer)
  }}

  #Integer input
  if (length(type)==1) {if (type == "integer") {
    while ((!suppressWarnings(!is.na(as.numeric(answer))) |  #Not a number
           suppressWarnings(as.numeric(answer))%%1!=0) &     #Not an integer
           !is.na(answer)) {                                 #Not NA
      cat("Please enter an integer.")
      answer <- scan(nmax = 1, what = "character", sep = "#", quiet = TRUE)
    }
  answer <- as.numeric(answer)
  return(answer)
  }}

  #Custom input list
  if (length(type) > 1) {
    while (!(answer %in% type)) {  #Not one of allowable responses
      cat("Please enter one of these options:",type)
      answer <- scan(nmax = 1, what = "character", sep = "#", quiet = TRUE)
    }
  return(answer)
  }
}

#' Returns `menu()` response as choice text
#'
#' @param choices a character vector of choices
#' @param title a character string to be used as the title of the menu. `NULL` is also accepted.
#'
#' @return string: the chosen option
#' @export
#'
#' @examples
#' \dontrun{  #Because menu2() is interactive
#' choice <- menu2(choices = c("Y", "N"), title = "Choose Y or N")
#' }
menu2 <- function(choices, title) {answer <- choices[utils::menu(choices = choices, title = title)]}


