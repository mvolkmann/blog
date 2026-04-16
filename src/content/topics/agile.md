---
eleventyNavigation:
  key: Agile
layout: topic-layout.njk
---

This post presents my thoughts on the agile process.
It is a work in progress while I continue gathering my thoughts.
I believe I have worked on at least 30 software projects
that have used some variation of the agile process,
so I speak from experience.

## Good Qualities

The agile process has many good qualities.
These include:

- breaking up a project into stories,
  which helps to understand the scope of the work
- allowing the customer to decide the order in which stories should be worked
- allowing the customer to decide which stories should be in the next sprint
- allowing the customer to stop the project if it isn’t going well
- providing a forum to share status, ask questions, and raise issues

## Cost of Potential Benefits

All the agile practices have potential benefits.
But they also have real costs that the customer is paying.
We should seek to minimize these costs
when the potential benefits do not outweigh their cost.

## Daily Standup Meetings

Agile meetings for software projects typically
include EVERYONE involved on the team.
This might include the following roles:

- customer
- business project lead
- business analyst
- technical product lead
- user interface designer
- user experience designer
- user interface developer
- API (service) developer
- database administrator
- DevOps architect (responsible for cloud infrastructure,
  continuous integration, and continuous deployment)
- developer of automated tests
- manual tester

In daily standup meetings it is common for people in many of these roles
to report that they did not work on the project yesterday
and have no tasks planned for today.
They remain in the meeting to learn what others are doing
because there is a possibility that it will affect their future tasks.
These meetings typically take 15 to 20 minutes.
Even though they are relatively short, given the number of participants
and the fact that they occur daily, they are expensive.

Consider this analogy. You have decided to remodel your kitchen.
It's a complete overhaul which will require a carpenter, electrician,
plumber, cabinet installer, counter-top installer, painter, flooring installer,
and interior decorator. There is a also a person that is coordinating
all of the work.

The project coordinator informs you that they are going to use an
agile process that includes having a standup meeting every morning.
How do you feel about paying the hourly rate of each of these people
to discuss your remodeling project for 20 minutes every morning?

How do feel about paying for the entire team
participate in meetings to plan the tasks (stories)
and estimate how long they will take?
Do you want the painter to participate in estimating
how long it will take to wire the garbage disposal?

How do feel about paying for the entire team
have a meeting at the end of every two week period of work (sprint) to
discuss what went well, what went wrong, and what practices should continue
(retrospective)?

I imagine some readers will claim that a home remodeling project
is very different from a software project,
but I think they are more similar than some are willing to admit.
I invite explanations about how they are different.

## Project Estimates

One way to address the cost of including all team members
in all agile meetings it to include it in initial project estimates.
Doing so will necessarily increase the estimated project cost.
It will likely also result in some project proposals being rejected
because the cost is higher than the customer is willing to pay.

We need to find a way to reduce these costs in order to
increase the likelihood of project proposals being accepted.

## Story points

Story points were invented because humans are bad
at estimating the actual time required for tasks.
They are fictional units of measure that
are meant to represent both time and complexity.
The theory is that over time each team will learn what a "point"
actually represent for them.

The idea that a team can learn over time the value of a point
seems no more realistic that the idea that over time a team can
become better at estimating the actual time it will take to complete tasks.
And those estimates would be more meaningful to project stake holds than points.

Story points also serve as a way to avoid criticism for bad estimates.
When a story runs long, a team can just say they are still determining
what a point means for their team.

## Velocity

One claimed benefit of using story points is that they can be used to measure
the velocity of a team which helps in predicting the amount of work that
can be completed in a future sprint.
However, this isn't as accurate as one might hope.

One reason is that story point estimates typically are not allowed
to assume the experience level of the person that will work on the story.
For example, suppose there is a story to implement an elaborate bar chart
that supports data filtering and sorting. If this story can be assigned
to anyone on the team, the time required to complete it can vary significantly.
Someone that is new to this kind of development might require two weeks
to complete the story, while someone that has done something similar many times
might complete the story in one day.
So how many points should be assigned to the story?
If a middle ground is selected then when the story is completed,
the team velocity will rise or fall depending on the experience level
of the developer that works on the story.
So what is the value of the calculated velocity?

## Recommendations

I know these will be perceived as agile blasphemy, but
here are my recommendations for changes to the typical agile process.

### Standup meetings

Only team members that are actively working on the project should be invited.
This means the participants will change over the life of the project.
If there has been a change in direction that will affect someone not attending
or if there is question that requires input from someone not attending,
plan a separate meeting to discuss the issue among the affected parties.

### Story writing and elaboration

For each story, identify the team member who is most familiar with
the required work and ask them to write the first draft of the
story description that includes acceptance criteria and an estimate.
Do not make it a team exercise to write stories
because the cost of doing that is too high.

Once there is an initial draft of many stories,
have a separate meeting to review
each set of related stories and provide input.
Only invite those familiar with work to be done
and those that will be affected by it.
Do not force team members to provide estimates for stories
that are outside of their expertise.

### Retrospectives

These meetings occur at the end of each sprint and ask each member to identify
things that went well, went poorly, and should continue.
This sounds good in theory, but consider how often something of consequence
comes out of these meetings and measure that against the cost of running them.

Team members should be encouraged to raise an issues
at the end of every daily standup meeting.
There is no reason to wait until the end of a sprint to raise an issue.

## Conclusion

Time is money and the customer is paying for it.
Don't spend time on things you would not be willing to pay for
if you were the one footing the bill.
