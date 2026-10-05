require("./config/constants")

if hl.plugin.hy3 ~= nil then
   local hy3 = hl.plugin.hy3
   -- Keybindings
   hl.unbind(mainMod .. " + W")
   hl.bind(mainMod .. " + W", hy3.make_group("tab", { toggle = true }))

   hl.unbind(mainMod .. " + J")
   hl.unbind(mainMod .. " + K")
   hl.unbind(mainMod .. " + L")
   hl.unbind(mainMod .. " + Semicolon")
   
   hl.bind(mainMod .. " + J",  hy3.move_focus("l"))
   hl.bind(mainMod .. " + K", hy3.move_focus("d"))
   hl.bind(mainMod .. " + L",    hy3.move_focus("u"))
   hl.bind(mainMod .. " + Semicolon",  hy3.move_focus("r"))

   hl.unbind(mainMod .. " + SHIFT + J")
   hl.unbind(mainMod .. " + SHIFT + K")
   hl.unbind(mainMod .. " + SHIFT + L")
   hl.unbind(mainMod .. " + SHIFT + Semicolon")
   
   hl.bind(mainMod .. " + SHIFT + J",         hy3.move_window("l"))
   hl.bind(mainMod .. " + SHIFT + K",         hy3.move_window("d"))
   hl.bind(mainMod .. " + SHIFT + L",         hy3.move_window("u"))
   hl.bind(mainMod .. " + SHIFT + Semicolon", hy3.move_window("r"))
   
   hl.config({
		 general = {
			layout = "hy3",
		 },
		 plugin = {
			hy3 = {
			   no_gaps_when_only = 0,
			}
		 }
   })
end
