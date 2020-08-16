#' Install the TensorFlow backend
#'
#' TensorFlow will be installed into an "r-tensorflow" virtual or conda
#' environment. Note that "virtualenv" is not available on Windows (as this isn't
#' supported by TensorFlow).
#'
#' @inheritParams tensorflow::install_tensorflow
#'
#' @param method Installation method ("conda" or "virtualenv").
#'
#' @param version TensorFlow version to install ( by default, "2.0.0").
#'
#' @param extra_packages Additional PyPI packages to install along with TensorFlow.
#'
#' @param ... Other arguments passed to [reticulate::py_install()].
#'
#' @section Custom Installation:
#'
#' Custom installations of TensorFlow are not supported yet by deepredeff.
#'
#' @section Additional Packages:
#'
#' If you wish to add additional PyPI packages to your TensorFlow environment you
#' can either specify the packages in the `extra_packages` argument of `install_tensorflow()`,
#' or alternatively install them into an existing environment using the
#' [reticulate::py_install()] function.
#'
#' Notice that this may have undesired side-effects on Windows installations.
#'
#' @examples
#' \dontrun{
#' # Default installation
#' library(deepredeff)
#' install_tensorflow()
#' }
#'
#' @export
install_tensorflow <- function(method = c("conda", "virtualenv"),
                               conda = "auto",
                               version = "2.0.0",
                               extra_packages = NULL,
                               ...) {

  # Verify method
  method <- match.arg(method)

  # Install Miniconda
  tryCatch(
    reticulate::install_miniconda(),
    error = function(cond) {
      message(cond)
    },
    finally = message("\n\nWill proceed to install TensorFlow")
  )

  # Install TensorFlow on Linux and macOS
  if (!is_windows()) {
    tryCatch(
      tensorflow::install_tensorflow(
        method = method,
        conda = conda,
        version = version,
        extra_packages = extra_packages,
        pip_ignore_installed = FALSE,
        ...
      ),
      error = function(cond) {
        message(cond)
      }
    )
  }

  # Install TensorFlow on Windows
  if (is_windows()) {
    # Avoid DLL in use errors
    if (reticulate::py_available()) {
      stop(
        "You should call install_tensorflow() only in a fresh ",
        "R session that has not yet initialized TensorFlow (this is ",
        "to avoid DLL in use errors during installation)"
      )
    }

    package <- paste0("tensorflow==", version)

    tryCatch(
      reticulate::py_install(
        packages = c(package, extra_packages),
        envname = NULL,
        method = method,
        conda = conda,
        python_version = "3.6",
        pip = TRUE,
        ...
      ),
      error = function(cond) {
        message(cond)
      }
    )
  }
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}
