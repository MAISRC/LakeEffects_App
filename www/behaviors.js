// JavaScript function to apply fade in/out on UI output elements
    Shiny.addCustomMessageHandler('fade', function(message) {
      var el = document.getElementById(message.id);
      if (message.action === 'insert') {
        el.classList.add('fade-enter-active');
        setTimeout(function() {
          el.classList.remove('fade-enter', 'fade-enter-active');
        }, 500);
      } else if (message.action === 'remove') {
        el.classList.add('fade-leave-active', 'fade-leave-to');
        setTimeout(function() {
          el.parentNode.removeChild(el);
        }, 500);
      }
    });

// Prevent invalid characters in the input widget
$(document).on("keydown", "#fetch_lake_search", function (e) {
  // Define allowed keys
  const allowedKeys = [
    ...Array(26).fill().map((_, i) => String.fromCharCode(i + 65)), // A-Z
    ...Array(26).fill().map((_, i) => String.fromCharCode(i + 97)), // a-z
    ...Array(10).fill().map((_, i) => String.fromCharCode(i + 48)), // 0-9
    "Backspace", "Tab", "ArrowLeft", "ArrowRight", "Delete", "Shift", "Control", "Alt"
  ];

  // If the key is not allowed, prevent the default action
  if (!allowedKeys.includes(e.key)) {
    e.preventDefault();
    return false;
  }
});