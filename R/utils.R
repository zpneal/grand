.onAttach <- function(lib,pkg) {
  local_version <- utils::packageVersion("grand")
  packageStartupMessage("+-------+  grand v",local_version)
  packageStartupMessage("| GRAND |  Cite: Neal, Z. P., (2023). grand: An R package for using the Guidelines for")
  packageStartupMessage("| ~~~~~ |        Reporting About Network Data. GitHub. https://github.com/zpneal/grand/")
  packageStartupMessage("| ~~~~~ |")
  packageStartupMessage("| ~~~~~ |  Help: type vignette(\"grand\"); email zpneal@msu.edu; github zpneal/grand")
  packageStartupMessage("+-------+  Beta: type devtools::install_github(\"zpneal/grand\", ref = \"devel\")")
}

#' Restricts `scan()` input to a specified format
#'
#' @param prompt string: prompt for user input
#' @param type string: required format for input
#'
#' @return user input in specified format
#' @export
#'
#' @examples
#' character <- scan2(prompt = "Type any character", type = "character")
#' numeric <- scan2(prompt = "Type any number", type = "numeric")
#' integer <- scan2(prompt = "Type any number", type = "integer")
#' custom <- scan2(prompt = "Yes or No?", type = c("Y","N"))
scan2 <- function(prompt, type) {

  if (!interactive()) {
    warning("scan2() requires that R is running interactively")
    return(NULL)
    }

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
#' @param loop boolean: should the menu loop to allow multiple choices?
#'
#' @return string: the chosen option
#' @export
#'
#' @examples
#' choice <- menu2(choices = c("A", "B", "C"), title = "Choose an option", loop = TRUE)
menu2 <- function(choices, title, loop = FALSE) {

  if (!interactive()) {
    warning("menu2() requires that R is running interactively")
    return(NULL)
  }

  answer <- choices[utils::menu(choices = choices, title = title)]  #Ask question

  if (!loop | length(answer)==0) {return(answer)}

  if (loop & length(answer)!=0) {
    answers <- answer  #Start a vector of answers
    choices <- choices[choices!=answer]  #Remove answer from remaining choices
    while (length(choices!=0)) {  #While there are remaining choices
      answer <- choices[utils::menu(choices = choices, title = title)]  #Ask question again
      if (length(answer)!=0) {  #If an option was selected
        answers <- c(answers, answer)  #Add it to the vector
        choices <- choices[choices!=answer]  #Remove it from remaining choices
      } else {choices <- character()} #If an option was not selected, remove all remaining choices
    }
    return(answers)
  }
  }


