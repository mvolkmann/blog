---
eleventyNavigation:
  key: Editor Customization
  order: 50
  parent: Smalltalk
layout: topic-layout.njk
---

I would like to implement a subset of the Vim keybindings
for editing code in System Browsers.
Here is what I have tried so far.

- Create a subclass of the `SmalltalkEditor` class called `VimEditor`.

- Add the instance variable `mode`
  that will hold the string "normal" or "insert".

- Initialize `mode` to `'insert'` by adding the following instance method:

  ```smalltalk
  initialize
      mode := 'insert'.
  ```

- Implement the class method `initializeShortcuts` as follows:

  ```smalltalk
  initializeShortcuts
      super initializeShortcuts.
      shortcuts
          at: 27 + 1
          put: #switchToCommandMode:.
      "escape key - when in insert mode, switch to command mode"
      shortcuts
          at: 105 + 1
          put: #xKey:.
      "i key - when in command mode, switch to insert mode"
      shortcuts
          at: 120 + 1
          put: #xKey:.
      "x key - when in command mode, delete character under cursor"
  ```

- Implement the following instance methods:

  ```smalltalk
  escapeKey: aKeyboardEvent
      | inCommandMode |
      inCommandMode := mode = 'command'.
      inCommandMode ifFalse: [mode := 'command'].
      "Hopefully returning false means that a superclass can process the event."
      ^ inCommandMode not.

  iKey: aKeyboardEvent
      | inCommandMode |
      inCommandMode := mode = 'command'.
      inCommandMode ifTrue: [mode := 'insert'].
      "Hopefully returning false means that a superclass can process the event."
      ^ inCommandMode.

  xKey: aKeyboardEvent
      | inCommandMode |
      inCommandMode := mode = 'command'.
      inCommandMode ifTrue: ['delete character under cursor' print].
      "Hopefully returning false means that a superclass can process the event."
      ^ inCommandMode.
  ```

- TODO: Determine how to modify the System Browser to use `VimEditor`
  instead of `SmalltalkEditor` in its bottom pane.

- TODO: Determine how to modify the `xKey:` method to really delete a character.

Changes will likely have no effect until the image is saved and restarted.
