---
eleventyNavigation:
  key: WCAG
layout: topic-layout.njk
---

This page copies and summarizes the Web Content Accessibility Guidelines (WCAG)
version 2.0 which were last updated on 22 Sep 2025.
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

## Principle 2 – Operable

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

### Guideline 2.5 – Input Modalities

Make it easier for users to operate functionality through various inputs
beyond keyboard.

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
