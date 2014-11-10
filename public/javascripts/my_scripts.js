$(document).ready(function() {
  console.log("loaded");

  $('#add_participant').bind('click', function() {
    var participant_name = $('#name_entry').val();
    if (participant_name) {
      console.log(participant_name);
      $('#participants').append("<div class='part_row'><span class='participant'>" + participant_name +
                                "</span><span class='remove_participant button'>-</span></div>");
    }
  });

  $('#participants').on('click', '.remove_participant', function(event) {
    console.log($(this).text());
    $(this).parent('div').remove();
  });


});
