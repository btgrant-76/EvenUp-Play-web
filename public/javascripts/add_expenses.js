$(document).ready(function() {

  console.log('loaded add_expenses.js');

  var participants = {
    names : []
  };

  $('#add_expense').bind('click', function() {
    // TODO validate the numeric input
    var amount = parseFloat($('#amount').val());

    if (isNaN(amount)) {
      alert('Amount "' + $('#amount').val() + '" is not numeric.');
    } else {
      var participant_name = $('#participant').val();
      console.log('adding expense for ' + participant_name);

      var participant = getParticipant(participant_name);
      var description = $('#description').val();

      var expense = {
        'description' : description, 
        'amount' : amount 
      };

      participant['expenses'].push(expense);

      console.log(JSON.stringify(participant));

      updateExpenses();

      resetExpenseEntryFields();
    }
  });

  resetExpenseEntryFields();

  function resetExpenseEntryFields() {
    $('#amount').val(0);
    var descriptionField = $('#description');
    descriptionField.val('');
    descriptionField.focus();
  }

  function updateExpenses() {
    console.log('updating expenses...');
    $('.expense_row').remove();

    var participantsElem = $('#expenses');
    // FIXME participants is an object, not an array...
    for (var i = 0; i < participants.names.length; i++) {
      var p = participants[participants.names[i]];
      console.log(JSON.stringify(p));
      participantsElem.append("<div class='expense_row'>"
                              + "<button class='remove_expense'>-</button>"
                              + "<span class='expense'>" + p['name'] + "</span>"
                              + "</div>"
      );
    }
    console.log('expenses updated');

  }

  function addExpenseOnKeyPress(event) {
    if (event.keyCode == 13) {
      $('#add_expense').click();
    }
  }

  $('#amount').keyup(addExpenseOnKeyPress);
  $('#description').keyup(addExpenseOnKeyPress);
  $('#participant').keyup(addExpenseOnKeyPress);

  function getParticipant(name) {
    var participant = participants[name];

    if (participant === undefined) {
      console.log('creating new object for ' + name);

      participant = {
        'name' : name, 
        'expenses' : []
      };

      participants[name] = participant;
      participants.names.push(name);
    } 

    return participant;
  }


});
