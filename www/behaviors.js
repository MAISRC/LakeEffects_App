// Prevent invalid characters in the fetch lake search box widget
$(document).on("keydown", "#fetch_lake_search", function (e) {
  // Define allowed key codes
  const allowedKeys = [
    8,  // Backspace
    9,  // Tab
    16, // Shift
    17, // Control
    18, // Alt
    32, // Space
    37, // ArrowLeft
    39, // ArrowRight
    46  // Delete
  ];

  // Include A-Z, a-z, 0-9
  for (let i = 48; i <= 90; i++) {
    allowedKeys.push(i);
  }

  // If the key is not allowed, prevent the default action
  if (!allowedKeys.includes(e.keyCode)) {
    e.preventDefault();
    return false;
  }
});

//Auto-scroll to the bottom of the sidebar, as needed, to ensure the most recent widget is the one users can see.
Shiny.addCustomMessageHandler('scrollSidebar', function(message) {
        setTimeout(function() {
          var sidebar = document.getElementById('sidebar');
          sidebar.scrollTop = sidebar.scrollHeight;
        }, 100); // Delay of 100 milliseconds to ensure new widget has been drawn before the scroll occurs.
      });