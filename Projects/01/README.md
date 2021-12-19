# Twenty one

## Rules of the game

The rules of twenty-one (“blackjack”) are as follows. 

1. There are two players: the **customer** and the **dealer**. 
2. The object of the game is to be dealt a **set of cards** that comes as **close to 21** as possible **without going over 21** (“busting”). 
3. A card is represented as a word, such as `10s` for the ten of spades. (Ace, jack, queen, and king are `a,
j, q` and `k`.) **Picture** cards (jack, queen and king) are worth **10 points**; an **ace** is worth either **1 or 11** at the player’s
option.
4. We **reshuffle** the deck after **each round**, so strategies based on remembering which cards were dealt earlier are not possible.
5. Each player is dealt **two cards**, with **one of the dealer’s cards face up**.
6. The **dealer** always takes **another card** (“hits”) if he has **16 or less**, and always stops (“stands”) with 17 or more. 
7. The customer can play however s/he chooses,
but must play before the dealer. If the **customer exceeds 21, s/he immediately loses** (and the dealer doesn’t bother to take any cards). 
8. In case of a **tie**, **neither** player **wins**.

---

## Customer's strategy

The **customer’s strategy** of when to take another card is represented as a **function**.

Arguments:
- The **customer’s hand** so far: represented as a **sentence** in which each word is a card
- The **dealer's card** that is face up: represented as a **single word**

The strategy function should **return true or false** output, which tells **whether or not the customer wants another card**.

---

## File structure

There are two versions of this game, one without jokers and one with jokers. Here we will layout the version of the game that includes no 
joker in the deck. For the version that does include them refer to [Twenty one with Jokers](./joker/README.md)

- `twenty-one.scm`: contains all of the **game functionality**, **excluding the strategies**, and how to **obtain the total of a hand**.
- `01-best-total.scm`: calculates the **best score** out of **all** of the possible **combinations** of cards, where the combination can be of **any
size**. In **this game** the **combination size** will always be **equal** to the **size of the hand**.
- `01-aces.scm`: obtains the **best score** out of **all** of the possible **combinations of ace values**, it uses `01-best-total.scm`
- `01-main-best-total.scm`: **starts** the **generation process** by calling `01-aces.scm`
- `02-stop-at-17-strategy.scm`: defines a **strategy** where the **player** only **takes a card** if the **total** so far is **less than 17**.
- `03-play-n.scm`: defines a process that **plays n times with a given strategy**.
- `04-dealer-sentitive.scm`: defines a **strategy** where the **player takes a card** if:
  - The **dealer** has an **ace, 7, 8, 9, 10 or picture** card **showing** and the **customer** has **less than 17** or
  - the **dealer** has a **2, 3, 4, 5 or 6 showing** and the **customer** has **less than 12**.
- `05-stop-at-n.scm`: defines a **strategy** where the **player takes a card** if the hand **total is less than n**.
- `06-valentine.scm`: defines a **strategy** where the **plater takes a card** if
  - The **player's hand's total is less than 17** and the player does **not have a heart** in her/his hand.
  - The **player's hand's total is less than 19** and the player does **have a heart** in her/his hand.
- `07-suit.scm`: defines a **strategy** that takes as arguments: 
  - **suit** (`h, s, d` or `c`), 
  - a **strategy** (in case the **player's hand** does **not have** a **card of the suit**), and 
  - another **strategy** (in case the **player's hand** does **have** a **card of the suit**)
- `08-majority.scm`: defines a **strategy** that takes **three strategies** as arguments, where the **result strategy** decides whether or not to take a card by **consulting** the **three other strategies** and
going with the **majority**.
- `09-reckless.scm`: defines a **strategy** that takes **another strategy as argument** and always decides to **take one more card than the original strategy** would.
- `tests.scm`: a **collection of tests** of the different **strategies**.


