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