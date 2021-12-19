# Twenty one with Jokers

For the general rules of the game refer to [Twenty one](../README.md). Here we will lay out how this version works the files that were added modified for this version to work.

## Added rules

We are going to change the rules by adding two jokers to the deck. 

1. A **joker** can be **worth** any number of points **from 1 to 11**.

---

## File structure

Some files/procdures were explained in [Twenty one](../README.md). Those that were not explained are 
the following:

- `twenty-one.scm`: the **functionality** of this file remains the **same**, however we **added two jokers to the deck**, on the function that creates the deck (`make-ordered-deck`)
- `10-jokers.scm`: defines a **procedure that generates** all of the possible **combinations** for all of the **values the jokers can be worth** and **calculates the total of each combination**.
- `10-aces.scm`: define a **procedure** that **generates** all of the **possible combinations of values for the aces**, and **for each combination** it calls `10-jokers.scm` to **generate** all of the 
possible **combinations for the jokers**.
