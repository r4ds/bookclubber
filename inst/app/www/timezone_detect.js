$( document ).ready(function() {
  var d = new Date();
  var target = $('#client_time');
  var timezone = $('#timezone-client_zone');

  timezone.val(Intl.DateTimeFormat().resolvedOptions().timeZone);
  timezone.trigger("change");
});
