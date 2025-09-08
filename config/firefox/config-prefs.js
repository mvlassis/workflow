// config-prefs.js - a minimal bootstrap to restore load config.js
// based on https://github.com/AveYo/fox/tree/main
// create in Firefox defaults pref directory - for Linux: /usr/lib/firefox/defaults/pref/config-prefs.js
// must also create /usr/lib/firefox/config.js
pref("general.config.filename", "config.js");    
pref("general.config.obscure_value", 0);  
pref("general.config.sandbox_enabled", false);  
