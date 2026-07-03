$(document).on("shiny:connected", function(e) {
  dimension[0] = window.innerHeight;
  Shiny.onInputChange("dimension", dimension);
});


$(window).resize(function(e) {
  dimension[0] = window.innerHeight;
  Shiny.onInputChange("dimension", dimension);
});
