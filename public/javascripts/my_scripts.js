$(document).ready(function() {
  console.log("loaded");

  var names = [];

  $('#add_participant').bind('click', function() {
    var participant_name = $('#name_entry').val();
    if (names.indexOf(participant_name) != -1) {
      alert("Name '" + participant_name + "' is already in use."
            + "\nPlease pick a new name.");
    } else if (participant_name) {
      console.log(participant_name);

      names.push(participant_name);

      updateNames();

      $('#name_entry').val("");
    }
  });

  $('#name_entry').keyup(function(event) {
    if (event.keyCode == 13) {
      $('#add_participant').click();
    }
  });

  $('#participants').on('click', '.remove_participant', function(event) {
    console.log($(this).siblings('span').text());
    names.splice([names.indexOf($(this).siblings('span').text())], 1);
    $(this).parent('div').remove();
  });

  function updateNames() {
    names.sort();

    $('.part_row').remove();

    for (var i = 0; i < names.length; i++) {
      var p = names[i];
      $('#participants').append("<div class='part_row'>"
                                + "<button class='remove_participant'>-</button>"
                                + "<span class='participant'>" + p + "</span>"
                                + "<input type='hidden' name='name' value='" + p + "'/>"
                                + "</div>");
    }

  }

});
