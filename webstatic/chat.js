var refresh_rate = 200;
var refresh_id;

var last_id = 1;

var even_odd_row = true;

function log_event(ev) {
  var log_entry_id = String(Math.random()).substring(2);
  var log_entry_string = '#log_' + log_entry_id;

  var hours = ev.timestamp.getHours();
  var minutes = "0" + ev.timestamp.getMinutes();
  var seconds = "0" + ev.timestamp.getSeconds();

  var formattedTime = hours + ':' + minutes.substr(-2) + ':' + seconds.substr(-2);

  var row_classes = even_odd_row ? "even" : "odd";
  even_odd_row = !even_odd_row;

  $('#log').append('<li id="log_' + log_entry_id + '" data-message-id="' + ev.id + '" class="' + row_classes + '"><span class="timestamp"></span><span class="author"></span><span class="contents ' + ev.classes + '"></span><span class="roundtrip"></span></li>');

  $(log_entry_string + ' .timestamp').text(formattedTime);
  $(log_entry_string + ' .contents').text(ev.contents);
  if (ev.author) {
    $(log_entry_string + ' .author').text(ev.author + " >");
  }
  $(log_entry_string + ' .roundtrip').text(ev.roundtrip);

  $(log_entry_string)[0].scrollIntoView();
}

function close_ui_connection() {
  clearInterval(refresh_id);
  refresh_id = null;
}

function handle_event(ev) {
  if (ev.type == "message") {
    log_event({
      timestamp: new Date(ev.value.time * 1000),
      author: ev.value.author,
      id: ev.value.author + ev.value.id,
      contents: ev.value.contents
    });
  } else if (ev.type == "connection_error") {
    log_event({
      timestamp: new Date(),
      contents: "Error: " + ev.value + " (connection closed)",
      classes: "error"
    });
    close_ui_connection();
  } else if (ev.type == "connection_warning") {
    log_event({
      timestamp: new Date(),
      contents: "Warning: " + ev.value,
      classes: "warning"
    });
  } else if (ev.type == "connection_closed") {
    log_event({
      timestamp: new Date(),
      contents: "Remote connection closed",
      classes: "closed"
    });
    close_ui_connection();
  } else if (ev.type == "message_confirmation") {
    var timestamp = ((ev.value.time_delta / 1000000) + '').substring(0, 5);
    $('[data-message-id="Us' + ev.value.id + '"] .roundtrip').text(timestamp + 'ms');
  }
  else {
    alert("unrecognized event type: " + JSON.stringify(ev));
  }
}

function send_and_update(events) {
  var events = events || [];
  $.ajax({
    url: "/api/update",
    data: JSON.stringify(events),
    method: "POST",
    dataType: "json",
    success: function(result) {
      for (var idx = 0; idx < result.length; idx++) {
        handle_event(result[idx]);
      }
    },
    error: function() {
      log_event({
        timestamp: new Date(),
        contents: "Error from /api/update, quitting the update cycle",
        classes: "error"
      });
      close_ui_connection();
    }
  });
}

function send_message() {
  var message = {
    "type": "send_message",
    "value": {
      id: last_id,
      message: $('#chat_input').val()
    }
  };
  $('#chat_input').val('')
  last_id = last_id + 1;
  send_and_update([message]);
  log_event({
    timestamp: new Date(),
    contents: message.value.message,
    id: 'Us' + message.value.id,
    author: "Us"
  });
}

$(document).ready(function() {
  $('#send').click(send_message);
  $('#chat_input').keypress(function(e) {
    if (e.which == 13) {
      send_message();
      return false;
    }
  });

  $('#disconnect').click(function() {
    var message = {
      "type": "connection_closed",
      "value": null
    };
    send_and_update([message]);
    log_event({
      timestamp: new Date(),
      contents: "Closed connection",
      classes: "closed"
    });
    close_ui_connection();
  });

  refresh_id = setInterval(function() {
    send_and_update();
  }, refresh_rate);
});
