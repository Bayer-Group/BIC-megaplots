# test_that("seriation module output updates correctly", {
#
#   expect_snapshot(make_parameter_ui("id", var = "pain_intensity", name = "pain_intensity"))
#   expect_snapshot(make_parameter_ui("id", var = "rm", name = ""))
#
#   expect_snapshot(seriation_ui("id"))
#
#   testServer(
#     seriation_server,
#      args = list(
#       varSeq = shiny::reactive("Pain intensity"),
#       multiple_distmeasures = shiny::reactive(FALSE),
#       data_saved = shiny::reactive(NULL),
#       select_data = shiny::reactive("Use demo data")
#     ), {
#
#       return_value <- session$getReturned()
#       expect_equal(
#         names(return_value()),
#         c("distmeasure","sm","smDHD","norm", "norm2","indel","indel_numeric",
#           "expcost", "context", "link", "h_OMslen", "transindel", "tpow", "otto",
#           "previous", "add.column", "overlap", "step", "weighted", "methMissing"
#         )
#       )
#       expect_equal(class(return_value()),"list")
#       expect_equal(length(return_value()),20)
#
#     }
#   )
# })
