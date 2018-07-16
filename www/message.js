$(document).ready(function() {
  
  // initialize a counter
  var n = 0;
  
  // create a click handler which listens for a click on the element with id equal to RStudio
  $("#RStudio").on("click", function(){
  
    // increment the counter each time we click on the Rstudio logo
    n++;
    
    // send message to Shiny
    Shiny.onInputChange("count", n);
  });

});