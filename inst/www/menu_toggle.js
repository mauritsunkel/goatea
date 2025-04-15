$(document).ready(function() {
    // keep all menu items visible
    $('.treeview-menu').css('display', 'block');
  
    // prevent menu toggle-to-close behavior
    $('.sidebar-menu li.treeview > a').off('click').on('click', function(e) {
      e.stopImmediatePropagation();
    });
  });