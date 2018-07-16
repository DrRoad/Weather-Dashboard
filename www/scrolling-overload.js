$ = jQuery

Shiny.addCustomMessageHandler("messageHandler", sendMessage );

var total_out_of_waiting_list = 0;
var selected = [];

$(document).ready(function(){
  console.log('ready!');
  //highlight column on click
  $("#table").on("click", "td", function() {
    clear_color();
    selector = '#table tr td:nth-child(' + ($(this).index() + 1) + ')';
    selector_header = '#table tr th:nth-child(' + ($(this).index() + 1) + ')';
    $(selector).css('background','rgba(0,87,184,.5)'); //this is BY royal blue
    $(selector_header).css('background','rgba(0,87,184,.5)');

    //add the column name to the column name box
    $('#colname').val($(selector_header).text());
    //change the selected col number in the tobe hidden field so it can be passes back to shiny
    $('#selected_col').val($(this).index());
    Shiny.onInputChange("col", $(this).index());
    Shiny.onInputChange("name", $(selector_header).text());
   });
  /*Code For Controling the active and inactive state of files in the cabinet*/
  $("#file_waiting_list").on("click", ".inactive_file", function() {
    console.log('inactive');
    $(this).addClass('active_file');
    $(this).removeClass('inactive_file');
    var index = $(this).index();
    selected.push(index)
    Shiny.onInputChange("waitinglist", selected);
    console.log($(this).index());
    
  })
  $("#file_waiting_list").on("click", ".active_file", function() {
    console.log('active');
    $(this).addClass('inactive_file');
    $(this).removeClass('active_file');
    Shiny.onInputChange("waitinglist", 'active');
    //remove the click on thing from the selected list
    var index = $(this).index();
    selected.splice(index,1);
    Shiny.onInputChange("waitinglist", selected);
  })
})

//function for clearing all of the current color markings on the page
function clear_color(){
  $('#table td').css('background','none');
  $('#table th').css('background','none');
}

// this function is called by the handler, which passes the message
function sendMessage(message){
  // show the messsage as an alert
  alert(message);
}

//this function returns a list of all the indexes that have been selected
function getSelectedIndexes(){
  console.log('Herro')
  console.log($('.active_file').index())
}