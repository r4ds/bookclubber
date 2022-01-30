$( document ).ready(function() {
  var d = new Date();
  var target = $('#clientTime');
  var timezone = $('#clientZone')

  timezone.val(Intl.DateTimeFormat().resolvedOptions().timeZone);
  timezone.trigger("change")
});
