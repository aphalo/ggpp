#' Wrap character strings in a vector
#'
#' Wrap the members of a character vector to a given maximum width by inserting
#' new line characters at word boundaries.
#'
#' @param x character vector, or an object which can be converted to a character
#'   vector by \code{as.character}.
#' @param width a positive integer giving the target column for wrapping lines
#'   in the output.
#' @param indent a positive or negative integer giving the indentation of the
#'   first line in a member character string.
#' @param new.line character sting; use \code{"<br>"} for HTML encoded strings.
#'
#' @details Function \code{wrap_labels()} is a wrapper on \code{link{strwrap}}
#' that returns a vector of character strings instead of a list of vectors. In
#' addition to wrapping, indentation is supported. Wrapping is always at white
#' space, so \code{width = 0} wraps word by word.
#'
#' Because the returned value is a character vector of the same length as the
#' input, this function can be used within a call to \code{aes()} when mapping a
#' character vector to the \code{label} aesthetic, as long as the character
#' strings will not be parsed into R expressions. It can be also used to wrap
#' the strings in a variable stored in a data frame.
#'
#' @return A character vector of the same length as \code{x}, with new line
#'   characters inserted to wrap text lines longer than \code{width}. Names in
#'   \code{x} are preserved in the returned value, no names are added if none
#'   are present in \code{x}.
#'
#' @examples
#' my.text <- c(A = "This is the first string",
#'              B = "This is the second string, which is longer")
#'
#' wrap_labels(my.text, width = 20)
#' wrap_labels(unname(my.text), width = 20)
#'
#' cat(wrap_labels(my.text, width = 20), sep = "\n--\n")
#' cat(wrap_labels(my.text, width = 20, indent = 2), sep = "\n--\n")
#' cat(wrap_labels(my.text, width = 20, indent = -2), sep = "\n--\n")
#'
#' @export
#'
wrap_labels <- function(x,
                        width = 20,
                        indent = 0,
                        new.line = "\n") {
  if (length(x) == 0L) {
    # avoid returning list() as returned by sapply()
    return(character(0L))
  }
  if (indent < 0) {
    exdent = - indent
    indent = 0
  } else {
    exdent = 0
  }
  sapply(X = x,
         FUN = function(x) {
           paste(
             strwrap(x,
                     width = width,
                     prefix = new.line,
                     initial = "",
                     indent = indent,
                     exdent = exdent),
             collapse = "")
         },
         USE.NAMES = FALSE)
}
