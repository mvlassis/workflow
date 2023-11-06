#!/usr/bin/env python
# i3format.py
# Use i3ipc to set the following layout on all workspaces:
# One window on the left, then tabbed layout on the right

import i3ipc
from i3ipc import Event

i3 = i3ipc.Connection()

def on_new_window(i3, e):
    # print(f'a new window opened: {e.container.name}')
    current_workspace = i3.get_tree().find_focused().workspace()
    window_count = len(current_workspace.leaves())
    if window_count == 2:
        nodes = current_workspace.nodes
        if nodes[0].layout != "tabbed" and nodes[0].layout != "stacked":
            i3.command('split h; split v; layout tabbed')

# def on_workspace_focus(i3, e):
#     # print(f'workspace just got focus: {e.current.name}')
#     current_workspace = e.current
#     window_count = len(current_workspace.leaves())

i3.on(Event.WINDOW_NEW, on_new_window)
# i3.on(Event.WORKSPACE_FOCUS, on_workspace_focus)

i3.main()
