$(document).ready(function() {

  console.log('loaded add_expenses.js');

  var participants = {
    names : []
  };

  resetExpenseEntryFields();

  $('#add_expense').bind('click', function() {
    var amount = parseFloat($('#amount').val());
    var description = $('#description').val();

    if (inputsAreValid(amount, description)) {
      var participant_name = $('#participant').val();
      console.log('adding expense for ' + participant_name);
      var participant = getParticipant(participant_name);

      var expense = {
        'description' : description, 
        'amount' : amount,
        display : function() {
          return "$" + amount.toFixed(2) + " for " + description;
        }
      };

      participant['expenses'].push(expense);

      console.log(JSON.stringify(participant));

      updateExpenses();

      resetExpenseEntryFields();
    }
  });

  function inputsAreValid(amount, description) {
    // FIXME when the dialog appears, hitting enter re-triggers the alert.
    var areInputsValid = true;
    var message = ''
    if (description == '') {
      message = 'Please add a description.';
      $('#description').focus();
      areInputsValid = false;
    }

    if (isNaN(amount) || amount == 0) {
      zeroOutAmountField();
      if (message != '') {
        message += '\n';
      }
      message += 'Amount must be a number greater than 0.';
      areInputsValid = false;
    }

    if (message.length > 0) {
      alert(message);
    }

    return areInputsValid;
  }

  function zeroOutAmountField() {
    $('#amount').val(0);
  }

  function resetExpenseEntryFields() {
    zeroOutAmountField();
    var descriptionField = $('#description');
    descriptionField.val('');
    descriptionField.focus();
  }

  function updateExpenses() {
    console.log('updating expenses...');
    $('.part_row').remove();

    var participantsElem = $('#expenses');
    for (var i = 0; i < participants.names.length; i++) {
      var p = participants[participants.names[i]];
      console.log(JSON.stringify(p));
      participantsElem.append("<div class='part_row'>"
                              + "<span class='participant'>" + p['name'] + "</span>"
                              + htmlForExpenses(p['expenses'])
                              + "</div>"
      );
    }
    console.log('expenses updated');
  }

  function htmlForExpenses(expenseArray) {
    var stringBuilder = "";

    for (var i = 0; i < expenseArray.length; i++) {
      var expense = expenseArray[i];

      stringBuilder += "<div class='expense_row'>"
                       + "<button class='remove_expense'>-</button>"
                       + "<span class='expense'>" + expense.display() + "</span>"
                       + "</div>";
    }

    return stringBuilder;
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
