test_that("arguments have the correct class", {
  expect_error(
    db_area(data = "testing",
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png"),
    "data has to be a data.frame or similar"
  )
  expect_error(
    db_area(data = c(1,2),
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png"),
    "data has to be a data.frame or similar"
  )
  expect_error(
    db_area(data = matrix(c(1,2)),
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png"),
    "data has to be a data.frame or similar"
  )
})

test_that("data has the needed variables", {
  expect_error(
    db_area(data = data.frame(order=1:2, cat1=1:2, cat2=2:3),
            order = "test",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png"),
    "Needed variable absent: order"
  )
  expect_error(
    db_area(data = data.frame(order=1:2, cat1=1:2, cat2=2:3),
            order = "order",
            cat1 = "cat1",
            cat2 = "test",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png"),
    "Needed variable absent: cat2"
  )
})

test_that("category in the data is numerical", {
  expect_error(
    db_area(data = data.frame(order=1:2, cat1=1:2, cat2=c("a","f")),
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png"),
    "Variable has to be numeric: cat2"
  )
})

test_that("dpi and seed are integers", {
  expect_warning(
    db_area(data = data.frame(order=1:2, cat1=1:2, cat2=3:4),
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png",
            dpi = 10.1),
    "dpi was not an integer"
  )
  expect_warning(
    db_area(data = data.frame(order=1:2, cat1=1:2, cat2=3:4),
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png",
            seed = 10.1),
    "seed was not an integer"
  )
})

test_that("limits and names have the length equal to two", {
  expect_error(
    db_area(data = data.frame(order=1:2, cat1=1:2, cat2=3:4),
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png",
            limits = 1:3),
    "limits must be a vector with two items"
  )
  expect_error(
    db_area(data = data.frame(order=1:2, cat1=1:2, cat2=3:4),
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png",
            limits = 1),
    "limits must be a vector with two items"
  )
  expect_error(
    db_area(data = data.frame(order=1:2, cat1=1:2, cat2=3:4),
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png",
            names = c("a","b","c")),
    "names must be a vector with two items"
  )
  expect_error(
    db_area(data = data.frame(order=1:2, cat1=1:2, cat2=3:4),
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png",
            names = "a"),
    "names must be a vector with two items"
  )
})

test_that("limits are given in the correct order", {
  expect_error(
    db_area(data = data.frame(order=1:2, cat1=1:2, cat2=3:4),
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.png",
            limits = c(2,-1)),
    "limits must be given in the following order: inferior, superior"
  )
})

test_that("filename has a valid extension for ggplot2::ggsave to use", {
  file_exts <- c(".eps", ".ps", ".tex", ".pdf", ".jpeg",
                 ".tiff", ".png", ".bmp", ".svg", ".wmf")
  expect_error(
    db_area(data = data.frame(order=1:2, cat1=1:2, cat2=3:4),
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test"),
    paste0("filename must contain one of these extensions: ",
           paste0(file_exts, collapse = ", "))
  )
  expect_error(
    db_area(data = data.frame(order=1:2, cat1=1:2, cat2=3:4),
            order = "order",
            cat1 = "cat1",
            cat2 = "cat2",
            title = "title",
            subtitle = "subtitle",
            message = "message",
            filename = "test.txt"),
    paste0("filename must contain one of these extensions: ",
           paste0(file_exts, collapse = ", "))
  )
})
