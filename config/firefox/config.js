// ==UserScript==
// @name            SimpleHotkeysOverride v2
// @author          Manos
// @description     see Firefox-Install-Directory/browser/omni.ja/chrome/browser/content/browser/browser.xhtml - 'mainKeyset'
// @include         main
// @onlyonce
// ==/UserScript==
// based on https://github.com/AveYo/fox/tree/main
function UserChromeJS() { Services.obs.addObserver(this, 'chrome-document-global-created', false); } ; UserChromeJS.prototype = {
	observe:function(s) {s.addEventListener('DOMContentLoaded', this, {once:true});}, handleEvent: async function(evt) {
		let browser = evt.originalTarget, document = browser, window = browser.defaultView, console = window.console;
		if (window.gBrowserInit && !window.gBrowserInit.delayedStartupFinished) { await window.delayedStartupPromise; }
		if (!window.gBrowserInit || !window.docShell) { /* console.info(window.location.href); */ return; }

		if (typeof UC === 'undefined') UC = {};

		UC.SimpleHotkeysOverride = {
			init: function() {
				let hotkeys = function (id, modifiers, key, cmd, oncmd) {
					const k = window.document.createXULElement("key"); k.id = id;
					if (!key.startsWith("VK_")) {k.setAttribute("key", key);} else {k.setAttribute("keycode", key); k.setAttribute("event", "keydown");}
					k.setAttribute("modifiers", modifiers); if (cmd) k.setAttribute("command", cmd); if (oncmd) k.setAttribute("oncommand", oncmd);
					window.document.getElementById("mainKeyset").appendChild(k);
				};
				// Remove new window application built-in shortcut
				let key_quit = document.getElementById("key_newNavigator"); if (key_quit) key_quit.remove();
				console.info('\u2713 SimpleHotkeysOverride v2');
			}
		};

		UC.SimpleHotkeysOverride.init();

	} }; if (!Services.appinfo.inSafeMode) new UserChromeJS();
