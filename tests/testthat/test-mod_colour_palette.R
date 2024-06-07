test_that("colour_palette module output updates correctly", {
  
  expect_snapshot(mod_colour_palette_ui("id"))
  
  testServer(
    mod_colour_palette_server, 
    args = list(
      event = shiny::reactive(c("pain_intensity")),
      level = shiny::reactive(c("1 - mild","2 - moderate","3 - severe")),
      color = shiny::reactive(c("#a6cee3","#cab2d6","#b2df8a","#fb9a99","#fdbf6f","#1f78b4","#6a3d9a","#ff7f00"))
    ), {
      session$setInputs(
        col1 = "#a6cee3",
        col2 = "#cab2d6",
        col3 = "#b2df8a",
        col4 = "#fb9a99",
        col5 = "#fdbf6f",
        col6 = "#1f78b4",
        col7 = "#6a3d9a",
        col8 = "#ff7f00"
      )
      expect_equal(output$header,"pain_intensity")
      return_value <- session$getReturned()
      expect_equal(return_value$colors(),c("#a6cee3","#cab2d6","#b2df8a","#fb9a99","#fdbf6f","#1f78b4","#6a3d9a","#ff7f00"))
      session$setInputs(
        col1 = "#ccebc5",
        col2 = "#fdb462",
        col3 = "#b3de69",
        col4 = "#fccde5",
        col5 = "#d9d9d9",
        col6 = "#bc80bd",
        col7 = "#80b1d3",
        col8 = "#ffed6f"
      )
      expect_equal(return_value$colors(), c("#ccebc5","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#80b1d3","#ffed6f"))
    }
  )
})