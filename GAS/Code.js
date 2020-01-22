function doGet() {
  var Html = HtmlService.createTemplateFromFile('html').evaluate();
  return Html.setXFrameOptionsMode(HtmlService.XFrameOptionsMode.ALLOWALL);
}
function include(filename) {
  return HtmlService.createHtmlOutputFromFile(filename)
      .getContent();
}
