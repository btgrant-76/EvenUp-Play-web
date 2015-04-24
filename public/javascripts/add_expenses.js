$(document).ready(function() {

  console.log('loaded add_expenses.js');

  var participants = {
    names : [],

    getParticipants: function(name) {
          var participant = participants[name];

          if (participant === undefined) {
            console.log('creating new object for ' + name);

            participant = {
              'name' : name,
              'expenses' : []
            };

            participants[name] = participant;
            participants.names.push(name);
            participants.names.sort();
          }

          return participant;
        }
  };

  resetExpenseEntryFields();

  $('#add_expense').bind('click', function() {
    var amount = parseFloat($('#amount').val());
    var description = $('#description').val();

    if (inputsAreValid(amount, description)) {
      var participant_name = $('#participant').val();
      console.log('adding expense for ' + participant_name);
      var participant = participants.getParticipants(participant_name);

      var expense = {
        'description' : description,
        'amount' : amount,

        display : function() {
          return "$" + amount.toFixed(2) + " for " + description;
        },

        matches : function(description, amount) {
          console.log('matching on ' + description + ' and ' + amount);
          console.log('my values are ' + this.description + ' and ' + this.amount);
          return this.description == description && this.amount == amount;
        }
      };

      participant['expenses'].push(expense);
      console.log(JSON.stringify(participant));
      updateExpenses();
      resetExpenseEntryFields();
    }
  });

  $('#expenses').on('click', '.remove_expense', function() {
    console.log($(this).text());

    var theExpense = $(this).siblings('.expense');
    var theAmount = theExpense.attr('exp_amount');
    var theDescription = theExpense.attr('exp_description');
    var participantExpenses = participants.getParticipants(theExpense.attr('part_name')).expenses;

    var removalIndex = -1;
    for (var i = 0; i < participantExpenses.length; i++) {
      var expense = participantExpenses[i];
      if (expense.matches(theDescription, theAmount)) {
        removalIndex = i;
      }
    }

    if (removalIndex == -1) {
      throw 'matching expense was not found';
    }

    participantExpenses.splice(removalIndex, 1);

    updateExpenses();
  });

  function inputsAreValid(amount, description) {
    // FIXME when the dialog appears, hitting enter re-triggers the alert.
    var areInputsValid = true;
    var message = '';
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

      var expenses = p['expenses'];
      if (expenses.length > 0) {
          participantsElem.append("<div class='part_row'>"
                                  + "<span class='participant'>" + p['name'] + "</span>"
                                  + htmlForExpenses(expenses, p['name'])
                                  + "</div>"
          );
      }
    }
    console.log('expenses updated');
  }

  function htmlForExpenses(expenseArray, participantName) {
    var stringBuilder = "";

    for (var i = 0; i < expenseArray.length; i++) {
      var expense = expenseArray[i];

      stringBuilder += "<div class='expense_row'>"
                       + "<button class='remove_expense'>-</button>"
                       + "<span class='expense' part_name='" + participantName
                       + "' exp_description='" + expense.description
                       + "' exp_amount='" + expense.amount + "'>" + expense.display() + "</span>"
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
  $('#submit_expenses').bind('click', function() {
    var expenses = [];

    for (var i = 0; i < participants.names.length; i++) {
      var p = participants[participants.names[i]];

      if (p === undefined) {
        // ignore it
      } else {
        expenses.push(p);
      }
    }

    console.log(JSON.stringify(expenses));

    $.ajax({
      url:  '/calculations',
      data: JSON.stringify(expenses),
      contentType: 'application/json',
      type: 'POST',
      success: function(noBody, textStatus, jqXHR) {
        window.location = jqXHR.getResponseHeader('Location')
      }
    });
  });

});
