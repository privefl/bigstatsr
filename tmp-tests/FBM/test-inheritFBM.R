FBM.code256_R6Class <- R6::R6Class(

  "FBM.code256",

  inherit = FBM_R6Class,

  public = list(
    initialize = function(nrow, ncol, code,
                          init = NULL,
                          backingfile = tempfile(),
                          save = TRUE) {

      if (length(code) != 256)
        stop2("'code' has to be of length 256")

      super$initialize(nrow, ncol, "unsigned char",  # raw (int in [0:255])
                       init, backingfile = tempfile(), save)

      self$code <- code
    }
  ),

  active = list(
    code = function(code) {
      if (missing(code)) {
        return(private$code256)
      } else {
        if (length(code) != 256)
          stop2("'code' has to be of length 256")

        private$code256 <- code
        return(invisible(self))
      }
    }
  ),

  private = list(
    code = NULL
  ),

  lock_class = TRUE
)

setOldClass(c("FBM.code256", "FBM"))

FBM.code256 <- function(nrow, ncol, code, ...) {

  do.call(FBM.code256_R6Class$new, args = c(as.list(environment()), list(...)))
}

tmp <- FBM.code256(10, 10, code = numeric(256), init = 1:100)
class(tmp)
tmp$code <- 1

