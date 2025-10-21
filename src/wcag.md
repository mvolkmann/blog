---
eleventyNavigation:
  key: WCAG
layout: topic-layout.njk
---

This page copies and summarizes the Web Content Accessibility Guidelines (WCAG)
version 2.2 which were last updated on 22 Sep 2025.
For the full content, see {% aTargetBlank
"https://www.w3.org/WAI/WCAG22/quickref/?versions=2.0",
"How to Meet WCAG (Quick Reference)" %}.

There are three levels to consider, A (easiest), AA, and AAA (hardest).
Each application must choose the levels to support.

The term "synchronized media" refers to audio or video content that is
synchronized with other time-based content, typically another visual or
audio track. For example, most videos include an audio track.

## Principle 1 - Perceivable

### Guideline 1.1 – Text Alternatives

Provide text alternatives for any non-text content so that it can be changed
into other forms people need, such as large print, braille, speech, symbols
or simpler language.

#### 1.1.1 Non-text content - Level A

All non-text content that is presented to the user has a text alternative
that serves the equivalent purpose, except for the situations listed below.

- Controls, Input: If non-text content is a control or accepts user input,
  then it has a name that describes its purpose.
- Time-Based Media: If non-text content is time-based media, then
  text alternatives at least provide descriptive identification of the
  non-text content.
- Test: If non-text content is a test or exercise that would be invalid
  if presented in text, then text alternatives at least provide
  descriptive identification of the non-text content.
- Sensory: If non-text content is primarily intended to create a specific
  sensory experience, then text alternatives at least provide
  descriptive identification of the non-text content.
- CAPTCHA: If the purpose of non-text content is to confirm that content is
  being accessed by a person rather than a computer, then text alternatives
  that identify and describe the purpose of the non-text content are provided,
  and alternative forms of CAPTCHA using output modes for different types of
  sensory perception are provided to accommodate different disabilities.
- Decoration, Formatting, Invisible: If non-text content is pure decoration,
  is used only for visual formatting, or is not presented to users, then it
  is implemented in a way that it can be ignored by assistive technology.

Non-text content includes icons, images, and video
that are not purely decorative.
This is typically satisfied by adding the attributes
alt, aria-label, aria-labelledby.

### Guideline 1.2 – Time-based Media

Provide alternatives for time-based media.

#### 1.2.1 Audio-only and Video-only (Prerecorded) - Level A

For prerecorded audio-only and prerecorded video-only media, the following
are true, except when the audio or video is a media alternative for text
and is clearly labeled as such:

- Prerecorded Audio-only: An alternative for time-based media is provided
  that presents equivalent information for prerecorded audio-only content.
- Prerecorded Video-only: Either an alternative for time-based media
  or an audio track is provided that presents equivalent information
  for prerecorded video-only content.

#### 1.2.2 Captions (Prerecorded) - Level A

Captions are provided for all prerecorded audio content in synchronized media,
except when the media is a media alternative for text and is clearly labeled
as such.

#### 1.2.3 Audio Description or Media Alternative (Prerecorded) - Level A

An alternative for time-based media or audio description of the prerecorded
video content is provided for synchronized media, except when the media is a
media alternative for text and is clearly labeled as such.

#### 1.2.4 Captions (Live) - Level AA

Captions are provided for all live audio content in synchronized media.

#### 1.2.5 Audio Description (Prerecorded) - Level AA

Audio description is provided for all prerecorded video content in
synchronized media.

#### 1.2.6 Sign Language (Prerecorded) - Level AAA

Sign language interpretation is provided for all prerecorded audio content
in synchronized media.

#### 1.2.7 Extended Audio Description (Prerecorded) - Level AAA

Where pauses in foreground audio are insufficient to allow audio descriptions
to convey the sense of the video, extended audio description is provided for
all prerecorded video content in synchronized media.

#### 1.2.8 Media Alternative (Prerecorded) - Level AAA

An alternative for time-based media is provided for all prerecorded
synchronized media and for all prerecorded video-only media.

#### 1.2.9 Audio-only (Live) - Level AAA

An alternative for time-based media that presents equivalent information
for live audio-only content is provided.

### Guideline 1.3 – Adaptable

Create content that can be presented in different ways (for example
simpler layout) without losing information or structure.

#### 1.3.1 Info and Relationships - Level A

Information, structure, and relationships conveyed through presentation
can be programmatically determined or are available in text.

#### 1.3.2 Meaningful Sequence - Level A

When the sequence in which content is presented affects its meaning,
a correct reading sequence can be programmatically determined.

#### 1.3.3 Sensory Characteristics - Level A

Instructions provided for understanding and operating content do not
rely solely on sensory characteristics of components such as
shape, color, size, visual location, orientation, or sound.

#### 1.3.4 Orientation - Level AA

Content does not restrict its view and operation to a single
display orientation, such as portrait or landscape,
unless a specific display orientation is essential.

#### 1.3.5 Identify Input Purpose - Level AA

The purpose of each input field collecting information about the user
can be programmatically determined when:

- The input field serves a purpose identified in the Input Purposes
  for user interface components section; and
- The content is implemented using technologies with support for
  identifying the expected meaning for form input data.

#### Identify Purpose - Level AAA

In content implemented using markup languages, the purpose of user interface
components, icons, and regions can be programmatically determined.

### Guideline 1.4 – Distinguishable

Make it easier for users to see and hear content
including separating foreground from background.

#### 1.4.1 Use of Color - Level A

Color is not used as the only visual means of conveying information,
indicating an action, prompting a response, or distinguishing a visual element.

#### 1.4.2 Audio Control - Level A

If any audio on a web page plays automatically for more than 3 seconds,
either a mechanism is available to pause or stop the audio, or
a mechanism is available to control audio volume independently
from the overall system volume level.

#### 1.4.3 Contrast (Minimum) - Level AA

The visual presentation of text and images of text has a contrast ratio
of at least 4.5:1, except for the following:

- Large Text: Large-scale text and images of large-scale text have a
  contrast ratio of at least 3:1;
- Incidental: Text or images of text that are part of an inactive user
  interface component, that are pure decoration, that are not visible to
  anyone, or that are part of a picture that contains significant other
  visual content, have no contrast requirement.
- Logotypes: Text that is part of a logo or brand name has no
  contrast requirement.

#### 1.4.4 Resize Text - Level AA

Except for captions and images of text, text can be resized without
assistive technology up to 200 percent without loss of content
or functionality.

#### 1.4.5 Images of Text - Level AA

If the technologies being used can achieve the visual presentation, text is
used to convey information rather than images of text except for the following:

- Customizable: The image of text can be visually customized to the
  user's requirements.
- Essential: A particular presentation of text is essential to the
  information being conveyed.

#### 1.4.6 Contrast (Enhanced) - Level AAA

The visual presentation of text and images of text has a contrast ratio
of at least 7:1, except for the following:

- Large Text: Large-scale text and images of large-scale text have a
  contrast ratio of at least 4.5:1;
- Incidental: Text or images of text that are part of an inactive
  user interface component, that are pure decoration, that are not visible
  to anyone, or that are part of a picture that contains significant
  other visual content, have no contrast requirement.
- Logotypes: Text that is part of a logo or brand name has no
  contrast requirement.

#### 1.4.7 Low or No Background Audio - Level AAA

For prerecorded audio-only content that
(1) contains primarily speech in the foreground,
(2) is not an audio CAPTCHA or audio logo, and
(3) is not vocalization intended to be primarily musical expression
such as singing or rapping, at least one of the following is true:

- No Background: The audio does not contain background sounds.
- Turn Off: The background sounds can be turned off.
- 20 dB: The background sounds are at least 20 decibels lower than the
  foreground speech content, with the exception of occasional sounds
  that last for only one or two seconds.

#### 1.4.8 Visual Presentation - Level AAA

For the visual presentation of blocks of text, a mechanism is available
to achieve the following:

- Foreground and background colors can be selected by the user.
- Width is no more than 80 characters or glyphs (40 if CJK).
- Text is not justified (aligned to both the left and the right margins).
- Line spacing (leading) is at least space-and-a-half within paragraphs,
  and paragraph spacing is at least 1.5 times larger than the line spacing.
- Text can be resized without assistive technology up to 200 percent
  in a way that does not require the user to scroll horizontally
  to read a line of text on a full-screen window.

#### 1.4.9 Images of Text - Level AAA

Images of text are only used for pure decoration or where a particular
presentation of text is essential to the information being conveyed.

Note: Logotypes (text that is part of a logo or brand name)
are considered essential.

#### 1.4.10 Reflow - Level AA

Content can be presented without loss of information or functionality,
and without requiring scrolling in two dimensions for:

- Vertical scrolling content at a width equivalent to 320 CSS pixels;
- Horizontal scrolling content at a height equivalent to 256 CSS pixels.

Except for parts of the content which require two-dimensional layout
for usage or meaning.

#### 1.4.11 Non-text Contrast - Level AA

The visual presentation of the following have a contrast ratio
of at least 3:1 against adjacent color(s):

- User Interface Components: Visual information required to identify
  user interface components and states, except for inactive components or
  where the appearance of the component is determined by the user agent
  and not modified by the author;
- Graphical Objects: Parts of graphics required to understand the content,
  except when a particular presentation of graphics is essential to the
  information being conveyed.

#### 1.4.12 Text Spacing - Level AA

In content implemented using markup languages that support the following
text style properties, no loss of content or functionality occurs by
setting all of the following and by changing no other style property:

- Line height (line spacing) to at least 1.5 times the font size;
- Spacing following paragraphs to at least 2 times the font size;
- Letter spacing (tracking) to at least 0.12 times the font size;
- Word spacing to at least 0.16 times the font size.

Exception: Human languages and scripts that do not make use of one or more
of these text style properties in written text can conform using
only the properties that exist for that combination of language and script.

#### 1.4.13 Content on Hover or Focus - Level AA

Where receiving and then removing pointer hover or keyboard focus triggers
additional content to become visible and then hidden, the following are true:

- Dismissible: A mechanism is available to dismiss the additional content
  without moving pointer hover or keyboard focus, unless the additional content
  communicates an input error or does not obscure or replace other content;
- Hoverable: If pointer hover can trigger the additional content,
  then the pointer can be moved over the additional content
  without the additional content disappearing;
- Persistent: The additional content remains visible until
  the hover or focus trigger is removed, the user dismisses it,
  or its information is no longer valid.

Exception: The visual presentation of the additional content is controlled by
the user agent and is not modified by the author.

## Principle 2 - Operable

User interface components and navigation must be operable.

### Guideline 2.1 – Keyboard Accessible

Make all functionality available from a keyboard.

#### 2.1.1 Keyboard - Level A

All functionality of the content is operable through a keyboard interface
without requiring specific timings for individual keystrokes, except where
the underlying function requires input that depends on the path of the
user's movement and not just the endpoints.

Note 1: This exception relates to the underlying function, not the
input technique. For example, if using handwriting to enter text,
the input technique (handwriting) requires path-dependent input
but the underlying function (text input) does not.

Note 2: This does not forbid and should not discourage providing mouse input
or other input methods in addition to keyboard operation.

#### 2.1.2 No Keyboard Trap - Level A

If keyboard focus can be moved to a component of the page using a keyboard
interface, then focus can be moved away from that component using only a
keyboard interface, and if it requires more than unmodified arrow or tab keys
or other standard exit methods, the user is advised of the method for
moving focus away.

Note: Since any content that does not meet this success criterion can
interfere with a user's ability to use the whole page, all content on the
web page (whether it is used to meet other success criteria or not)
must meet this success criterion.

#### 2.1.3 Keyboard - Level AAA

All functionality of the content is operable through a keyboard interface
without requiring specific timings for individual keystrokes.

#### 2.1.4 Character Key Shortcuts - Level A

If a keyboard shortcut is implemented in content using only letter
(including upper- and lower-case letters), punctuation, number, or
symbol characters, then at least one of the following is true:

- Turn off: A mechanism is available to turn the shortcut off;
- Remap: A mechanism is available to remap the shortcut to include
  one or more non-printable keyboard keys (e.g., Ctrl, Alt);
- Active only on focus: The keyboard shortcut for a user interface component
  is only active when that component has focus.

### Guideline 2.2 – Enough Time

Provide users enough time to read and use content.

#### 2.2.1 Timing Adjustable - Level A

For each time limit that is set by the content,
at least one of the following is true:

- Turn off: The user is allowed to turn off the time limit
  before encountering it; or
- Adjust: The user is allowed to adjust the time limit
  before encountering it over a wide range that is at least
  ten times the length of the default setting; or
- Extend: The user is warned before time expires and given at least
  20 seconds to extend the time limit with a simple action
  (for example, "press the space bar"), and the user is allowed to
  extend the time limit at least ten times; or
- Real-time Exception: The time limit is a required part of a
  real-time event (for example, an auction), and
  no alternative to the time limit is possible; or
- Essential Exception: The time limit is essential and extending it
  would invalidate the activity; or
- 20 Hour Exception: The time limit is longer than 20 hours.

#### 2.2.2 Pause, Stop, Hide - Level A

For moving, blinking, scrolling, or auto-updating information,
all of the following are true:

- Moving, blinking, scrolling: For any moving, blinking or scrolling
  information that
  (1) starts automatically,
  (2) lasts more than five seconds, and
  (3) is presented in parallel with other content, there is a mechanism for
  the user to pause, stop, or hide it unless the movement, blinking, or
  scrolling is part of an activity where it is essential; and
- Auto-updating: For any auto-updating information that
  (1) starts automatically and
  (2) is presented in parallel with other content, there is a mechanism for
  the user to pause, stop, or hide it or to control the frequency of the
  update unless the auto-updating is part of an activity where it is essential.

#### 2.2.3 No Timing - Level AAA

Timing is not an essential part of the event or activity presented by the
content, except for non-interactive synchronized media and real-time events.

#### 2.2.4 Interruptions - Level AAA

Interruptions can be postponed or suppressed by the user,
except interruptions involving an emergency.

#### 2.2.5 Re-authenticating - Level AAA

When an authenticated session expires, the user can continue the activity
without loss of data after re-authenticating.

#### 2.2.6 Timeouts - Level AAA

Users are warned of the duration of any user inactivity
that could cause data loss, unless the data is preserved
for more than 20 hours when the user does not take any actions.

### Guideline 2.3 – Seizures and Physical Reactions

Do not design content in a way that is known to
cause seizures or physical reactions.

#### 2.3.1 Three Flashes or Below Threshold - Level A

Web pages do not contain anything that flashes more than
three times in any one second period, or the flash is
below the general flash and red flash thresholds.

#### 2.3.2 Three Flashes - Level AAA

Web pages do not contain anything that flashes more than three times
in any one second period.

#### 2.3.3 Animation from Interactions - Level AAA

Motion animation triggered by interaction can be disabled,
unless the animation is essential to the functionality
or the information being conveyed.

### Guideline 2.4 – Navigable

Provide ways to help users navigate, find content, and determine where they are.

#### 2.4.1 Bypass Blocks - Level A

A mechanism is available to bypass blocks of content that are
repeated on multiple web pages.

#### 2.4.2 Page Titled - Level A

Web pages have titles that describe topic or purpose.

#### 2.4.3 Focus Order - Level A

If a web page can be navigated sequentially and the navigation sequences
affect meaning or operation, focusable components receive focus in an
order that preserves meaning and operability.

#### 2.4.4 Link Purpose (In Context) - Level A

The purpose of each link can be determined from the link text alone or
from the link text together with its programmatically determined link context,
except where the purpose of the link would be ambiguous to users in general.

#### 2.4.5 Multiple Ways - Level AA

More than one way is available to locate a web page within a set of web pages
except where the web page is the result of, or a step in, a process.

#### 2.4.6 Headings and Labels - Level AA

Headings and labels describe topic or purpose.

#### 2.4.7 Focus Visible - Level AA

Any keyboard operable user interface has a mode of operation where
the keyboard focus indicator is visible.

#### 2.4.8 Location - Level AAA

Information about the user's location within a set of web pages is available.

#### 2.4.9 Link Purpose (Link Only) - Level AAA

A mechanism is available to allow the purpose of each link to be identified
from link text alone, except where the purpose of the link would be
ambiguous to users in general.

#### 2.4.10 Section Headings - Level AAA

Section headings are used to organize the content.
"Heading" is used in its general sense and includes titles and
other ways to add a heading to different types of content.

#### 2.4.11 Focus Not Obscured (Minimum) - Level AA

When a user interface component receives keyboard focus,
the component is not entirely hidden due to author-created content.

#### 2.4.12 Focus Not Obscured (Enhanced) - Level AAA

When a user interface component receives keyboard focus,
no part of the component is hidden by author-created content.

#### 2.4.13 Focus Appearance - Level AAA

When the keyboard focus indicator is visible,
an area of the focus indicator meets all the following:

- is at least as large as the area of a 2 CSS pixel thick perimeter
  of the unfocused component or sub-component, and
- has a contrast ratio of at least 3:1 between the same pixels
  in the focused and unfocused states.

Exceptions:

- The focus indicator is determined by the user agent
  and cannot be adjusted by the author, or
- The focus indicator and the indicator's background color
  are not modified by the author.

### Guideline 2.5 – Input Modalities

Make it easier for users to operate functionality through various inputs
beyond keyboard.

#### 2.5.1 Pointer Gestures - Level A

All functionality that uses multipoint or path-based gestures for operation
can be operated with a single pointer without a path-based gesture,
unless a multipoint or path-based gesture is essential.

#### 2.5.2 Pointer Cancellation - Level A

For functionality that can be operated using a single pointer,
at least one of the following is true:

- No Down-Event: The down-event of the pointer is not used to
  execute any part of the function;
- Abort or Undo: Completion of the function is on the up-event,
  and a mechanism is available to abort the function before completion
  or to undo the function after completion;
- Up Reversal: The up-event reverses any outcome of the preceding down-event;
- Essential: Completing the function on the down-event is essential.

#### 2.5.3 Label in Name - Level A

For user interface components with labels that include text or images of text,
the name contains the text that is presented visually.

Note: A best practice is to have the text of the label at the start of the name.

#### 2.5.4 Motion Actuation - Level A

Functionality that can be operated by device motion or user motion can also be
operated by user interface components and responding to the motion
can be disabled to prevent accidental actuation, except when:

- Supported Interface: The motion is used to operate functionality
  through an accessibility supported interface;
- Essential: The motion is essential for the function
  and doing so would invalidate the activity.

#### 2.5.5 Target Size (Enhanced) - Level AAA

The size of the target for pointer inputs is at least 44 by 44 CSS pixels
except when:

- Equivalent: The target is available through an equivalent link or control
  on the same page that is at least 44 by 44 CSS pixels;
- Inline: The target is in a sentence or block of text;
- User Agent Control: The size of the target is determined by the user agent
  and is not modified by the author;
- Essential: A particular presentation of the target is essential
  to the information being conveyed.

#### 2.5.6 Concurrent Input Mechanisms - Level AAA

Web content does not restrict use of input modalities available on a platform
except where the restriction is essential, required to ensure the security
of the content, or required to respect user settings.

#### 2.5.7 Dragging Movements - Level AA

All functionality that uses a dragging movement for operation can be achieved
by a single pointer without dragging, unless dragging is essential or
the functionality is determined by the user agent and
not modified by the author.

Note: This requirement applies to web content that interprets pointer actions
(i.e., this does not apply to actions that are required to operate
the user agent or assistive technology).

#### 2.5.8 Target Size (Minimum) - Level AA

The size of the target for pointer inputs is at least 24 by 24 CSS pixels,
except when:

- Spacing: Undersized targets (those less than 24 by 24 CSS pixels) are
  positioned so that if a 24 CSS pixel diameter circle is centered on
  the bounding box of each, the circles do not intersect another target
  or the circle for another undersized target;
- Equivalent: The function can be achieved through a different control
  on the same page that meets this criterion;
- Inline: The target is in a sentence or its size is otherwise constrained
  by the line-height of non-target text;
- User Agent Control: The size of the target is determined by the user agent
  and is not modified by the author;
- Essential: A particular presentation of the target is essential or
  is legally required for the information being conveyed.

## Principle 3 – Understandable

Information and the operation of the user interface must be understandable.

### Guideline 3.1 – Readable

Make text content readable and understandable.

#### 3.1.1 Language of Page - Level A

The default human language of each web page can be programmatically determined.

#### 3.1.2 Language of Parts - Level AA

The human language of each passage or phrase in the content can be
programmatically determined except for proper names, technical terms,
words of indeterminate language, and words or phrases that have become
part of the vernacular of the immediately surrounding text.

#### 3.1.3 Unusual Words - Level AAA

A mechanism is available for identifying specific definitions of words or
phrases used in an unusual or restricted way, including idioms and jargon.

#### 3.1.4 Abbreviations - Level AAA

A mechanism for identifying the expanded form or meaning of abbreviations
is available.

#### 3.1.5 Reading Level - Level AAA

When text requires reading ability more advanced than the lower secondary
education level after removal of proper names and titles, supplemental content,
or a version that does not require reading ability more advanced than the
lower secondary education level, is available.

#### 3.1.6 Pronunciation - Level AAA

A mechanism is available for identifying specific pronunciation of words
where meaning of the words, in context, is ambiguous without knowing
the pronunciation.

### Guideline 3.2 – Predictable

Make web pages appear and operate in predictable ways.

#### 3.2.1 On Focus - Level A

When any user interface component receives focus, it does not initiate
a change of context. A "change of context" means a significant change
in the content that, if unexpected, can disorient the user.

#### 3.2.2 On Input - Level A

Changing the setting of any user interface component does not automatically
cause a change of context unless the user has been advised of the behavior
before using the component.

"Changing the setting" here means altering the state or value
of a form control or interactive element.
An example of this would be automatically navigating to different page
if the user enters the name of a color in an input.

#### 3.2.3 Consistent Navigation - Level AA

Navigational mechanisms that are repeated on multiple web pages within a set
of web pages occur in the same relative order each time they are repeated,
unless a change is initiated by the user.

#### 3.2.4 Consistent Identification - Level AA

Components that have the same functionality within a set of web pages
are identified consistently.

#### 3.2.5 Change on Request - Level AAA

Changes of context are initiated only by user request or a mechanism is
available to turn off such changes.

#### 3.2.6 Consistent Help - Level A

If a web page contains any of the following help mechanisms, and
those mechanisms are repeated on multiple web pages within a set of web pages,
they occur in the same order relative to other page content,
unless a change is initiated by the user:

- Human contact details;
- Human contact mechanism;
- Self-help option;
- A fully automated contact mechanism.

#### 3.2.7 Redundant Entry - Level A

Information previously entered by or provided to the user that is
required to be entered again in the same process is either:

- auto-populated, or
- available for the user to select.

Except when:

- re-entering the information is essential,
- the information is required to ensure the security of the content, or
- previously entered information is no longer valid.

#### 3.2.8 Accessible Authentication (Minimum) - Level AA

A cognitive function test (such as remembering a password or solving a puzzle)
is not required for any step in an authentication process
unless that step provides at least one of the following:

- Alternative: Another authentication method that does not rely on
  a cognitive function test.
- Mechanism: A mechanism is available to assist the user in completing
  the cognitive function test.
- Object Recognition: The cognitive function test is to recognize objects.
- Personal Content: The cognitive function test is to identify non-text content
  the user provided to the website.

#### 3.2.9 Accessible Authentication (Enhanced) - Level AAA

A cognitive function test (such as remembering a password or solving a puzzle)
is not required for any step in an authentication process
unless that step provides at least one of the following:

- Alternative: Another authentication method that does not rely on
  a cognitive function test.
- Mechanism: A mechanism is available to assist the user in completing
  the cognitive function test.

### Guideline 3.3 – Input Assistance

Help users avoid and correct mistakes.

#### 3.3.1 Error Identification - Level A

If an input error is automatically detected, the item that is in error
is identified and the error is described to the user in text.

#### 3.3.2 Labels or Instructions - Level A

Labels or instructions are provided when content requires user input.

#### 3.3.3 Error Suggestion - Level AA

If an input error is automatically detected and suggestions for
correction are known, then the suggestions are provided to the user,
unless it would jeopardize the security or purpose of the content.

#### 3.3.4 Error Prevention (Legal, Financial, Data) - Level AA

For web pages that cause legal commitments or financial transactions
for the user to occur, that modify or delete user-controllable data
in data storage systems, or that submit user test responses,
at least one of the following is true:

- Reversible: Submissions are reversible.
- Checked: Data entered by the user is checked for input errors
  and the user is provided an opportunity to correct them.
- Confirmed: A mechanism is available for reviewing, confirming,
  and correcting information before finalizing the submission.

#### 3.3.5 Help - Level AAA

Context-sensitive help is available.

#### 3.3.6 Error Prevention (All) - Level AAA

For web pages that require the user to submit information,
at least one of the following is true:

- Reversible: Submissions are reversible.
- Checked: Data entered by the user is checked for input errors
  and the user is provided an opportunity to correct them.
- Confirmed: A mechanism is available for reviewing, confirming,
  and correcting information before finalizing the submission.

## Principle 4 – Robust

Content must be robust enough that it can be interpreted by
a wide variety of user agents, including assistive technologies.

### Guideline 4.1 – Compatible

Maximize compatibility with current and future user agents,
including assistive technologies.

#### 4.1.1 Parsing - Level A

In content implemented using markup languages, elements have complete
start and end tags, elements are nested according to their specifications,
elements do not contain duplicate attributes, and any IDs are unique,
except where the specifications allow these features.

#### 4.1.2 Name, Role, Value - Level A

For all user interface components (including but not limited to:
form elements, links and components generated by scripts),
the name and role can be programmatically determined;
states, properties, and values that can be
set by the user can be programmatically set;
and notification of changes to these items is available to user agents,
including assistive technologies.

Note: This success criterion is primarily for web authors who
develop or script their own user interface components.
For example, standard HTML controls already meet this success criterion
when used according to specification.

#### 4.1.3 Status Messages - Level AA

In content implemented using markup languages, status messages can be
programmatically determined through role or properties
such that they can be presented to the user
by assistive technologies without receiving focus.
