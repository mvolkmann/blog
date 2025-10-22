---
eleventyNavigation:
  key: ROVE
layout: topic-layout.njk
---

{% aTargetBlank "https://buttonshygames.com/products/rove", "ROVE" %}
"is a solo spatial puzzle game" consisting of only 18 cards.
The object of the game is to complete at least seven "missions".

The maximum possible missions that can be completed is 12.
There are likely many ways to accomplish this,
but discovering even one is quite challenging.
This document describes one way by specifying the following:

- a starting module arrangement
- a starting deck order of movement cards
- an order of playing the movement cards
- the module movements to make

## Key

The module cards are referred to by their first letter:

- B = brain
- C = coil
- G = gripper
- L = laser
- M = motor
- S = sensor

The movement cards are referred to by their first two letters:

- BO = BOOST
- BR = BRIDGE
- CL = CLIMB
- DR = DRILL
- HA = HAUL
- JU = JUMP
- LE = LEARN
- NA = NAVIGATE
- RE = RECHARGE
- RO = ROLL
- SC = SCAN
- TR = TRANSMIT

Moves are described by a module letter, a direction,
and a number of spaces that defaults to 1.

The directions are ←, →, ↑, ↓, ↖, ↗, ↘, and ↙.

For example:

- M↓ means to move the MOTOR down one space.
- S↖2 means to move the SENSOR two spaces in the northwest direction.

## Setup

The starting module arrangement is:

```text
G L C
B S M
```

The order of the movement cards is:

HA RO BO JU CL SC LE BR DR TR RE NA

## Missions

The missions are solved as follows:

### Mission 1 - HAUL

play BO  
L↖

### Mission 2 - LEARN

play RO  
L→2  
G→

### Mission 3 - DRILL

play CL for 4  
B→3  
L↘  
M↓

### Mission 4 - RECHARGE

play TR  
M↑  
G↑  
L↑

### Mission 5 - BOOST

play BR and JU  
M↑  
L↓  
B←

### Mission 6 - CLIMB

play SC and NA  
L↑  
M←  
B←

### Mission 7 - BRIDGE

play TR  
G↘  
B↑3  
B→

### Mission 8 - SCAN

play NA and JU  
G↓  
G↓  
B↓4

### Mission 9 - TRANSMIT

play RO  
L↓  
L↙2  
B↑

### Mission 10 - JUMP

play NA and RO  
S↗  
L↗  
M↑  
B↑2

### Mission 11 - NAVIGATE

play RO  
B↓2  
L←  
G←  
use B to place C under B

### Mission 12 - ROLL

use L to swap B and L  
use C to place C ↖ of B  
use M to shift G ←  
use G to activate B to place L ↘ of B
