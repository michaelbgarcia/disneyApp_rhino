shinyjs.backgroundCol = function(params) {
  var defaultParams = {
    id : null,
    col : "red",
    ns : null
  };
  params = shinyjs.getParams(params, defaultParams);

  var el = $("#" + params.ns + params.id);
  el.css("background-color", params.col);
}
