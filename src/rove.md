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

HAUL ROLL BOOST JUMP CLIMB SCAN LEARN BRIDGE DRILL TRANSMIT RECHARGE NAVIGATE

Make HAUL the first mission and deal yourself
five movement cards from the top of the deck.

## Missions

The missions are solved as follows:

### Mission 1 - HAUL

play BOOST  
L↖

### Mission 2 - LEARN

play ROLL  
L→2  
G→

### Mission 3 - DRILL

play CLIMB for 4 (the only time the higher move number on a card is used)  
B→3  
L↘  
M↓

### Mission 4 - RECHARGE

play TRANSMIT  
M↑  
G↑  
L↑

### Mission 5 - BOOST

play BRIDGE and JUMP  
M↑ pushing C  
L↓  
B←

### Mission 6 - CLIMB

play SCAN and NAVIGATE  
L↑  
M←  
B↓

### Mission 7 - BRIDGE

play TRANSMIT  
G↘  
B↑4  
B→

### Mission 8 - SCAN

play NAVIGATE and JUMP  
G↓  
G↓  
B↓4

### Mission 9 - TRANSMIT

play ROLL  
L↓  
L↙2  
B↑

### Mission 10 - JUMP

play NAVIGATE and ROLL  
S↗  
L↗  
M↑  
B↑2

### Mission 11 - NAVIGATE

play ROLL  
B↓2  
L←  
G←  
use B to place C under B

### Mission 12 - ROLL

use L to swap B and L  
use C to place C ↖ of B  
use M to shift G ←  
use G to activate B to place L ↘ of B

The S ability is never used.
