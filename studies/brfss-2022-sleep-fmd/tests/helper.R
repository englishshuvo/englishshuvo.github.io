# Tiny test helper. Failures stop the process with a non-zero status.

test_failures <- character()

expect_true <- function(cond, label) {
  if (!isTRUE(cond)) {
    test_failures <<- c(test_failures, label)
    message("FAIL: ", label)
  } else {
    message("PASS: ", label)
  }
  invisible(cond)
}

expect_equal <- function(got, expected, label, tol = 1e-8) {
  same <- isTRUE(all.equal(got, expected, tolerance = tol, check.attributes = FALSE))
  if (!same) {
    test_failures <<- c(test_failures, label)
    message("FAIL: ", label)
    message("  got: ", paste(format(got), collapse = ", "))
    message("  expected: ", paste(format(expected), collapse = ", "))
  } else {
    message("PASS: ", label)
  }
  invisible(same)
}

finish_tests <- function() {
  if (length(test_failures)) {
    message("\n", length(test_failures), " test(s) failed.")
    quit(status = 1)
  }
  message("\nAll tests in this file passed.")
}
