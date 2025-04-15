Shiny.addCustomMessageHandler('update_css_colors', function(colors) {
    document.documentElement.style.setProperty('--main-bg-color', colors.main_bg);
    document.documentElement.style.setProperty('--darker-bg-color', colors.darker_bg);
    document.documentElement.style.setProperty('--focus-color', colors.focus);
    document.documentElement.style.setProperty('--hover-color', colors.hover);
    document.documentElement.style.setProperty('--border-color', colors.border);
    document.documentElement.style.setProperty('--text-color', colors.text);
  });