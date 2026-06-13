test_that(
  "semantic_stabilization detects invalid labels",
  {
    
    x <- structure(
      c("a", "b"),
      labels = list(
        a = "label_a",
        b = "label_b"
      ),
      class = "prelabelled"
    )
    
    result <- semantic_stabilization(x)
    
    expect_false(result$valid)
    expect_type(result$message, "character")
  }
)


test_that(
  "semantic_stabilization returns expected structure",
  {
    
    x <- prelabel(
      c("r", "png"),
      labels = c(
        r = "source_code",
        png = "visualisation"
      )
    )
    
    result <- semantic_stabilization(x)
    
    expect_named(
      result,
      c("valid", "message")
    )
    
    expect_type(result$valid, "logical")
  }
)
