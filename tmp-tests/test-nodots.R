assert_nodots <- function() {

  print(list_dots <- eval(parse(text = "list(...)"), parent.frame()))
  if (!identical(list_dots, list())) {
    bigstatsr:::stop2("Argument '%s' not used.", names(list_dots[1]))
  }
}

test <- function(...) {
  assert_nodots()
}
test()
test(a = 2)
