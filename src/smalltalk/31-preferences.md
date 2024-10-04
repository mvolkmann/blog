---
eleventyNavigation:
  key: Preferences
  order: 31
  parent: Smalltalk
layout: topic-layout.njk
---

Many supported preferences are not directly on
the Preferences submenu of the World menu.
To access those, click "All preferences..."
which opens a Preferences window.
Click a preference symbol in the left pane
to display its current value in the right pane.

To change the value of a preference:

- Select it in the left pane.
- Press cmd-i (inspect) to open an Inspect window.
- In the bottom pane, enter `value := {new-value}` and "Do it".
- Close the Inspect window.
- Close the Preferences window.

## PreferenceSet

Your packages can store user preferences in a `PreferenceSet`.

The first step is to ensure that a default value for the preference is set.
To do this, add the class method `initialize` to one of your classes.
For example:

```smalltalk
initialize
    | prefs |
    prefs := PreferenceSet sysPreferences.
    prefs
        name: #backgroundEffect
        description: 'determines how backgroundImage is applied'
        category: #gui
        type: Symbol
        value: #stretch.
```

The provided category symbols can be retrieved with
`PreferenceSet sysPreferences categories`.
By default that returns `#(#font #gui #programming #system)`.

To change the preference value:

```smalltalk
PreferenceSet sysPreferences at: #backgroundEffect put: #tile.
```

To retrieve the preference value:

```smalltalk
effect := PreferenceSet sysPreferences at: #backgroundEffect.
```
