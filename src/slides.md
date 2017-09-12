---
title: Finite State Machines?
subtitle: Your compiler wants in!
author: Oskar Wickström
date: |
  \includegraphics[width=1.5cm]{../../src/codemesh.png}
  \vspace{.25cm}
  \includegraphics[width=3cm]{../../src/mpowered.png}

theme: Boadilla
classoption: dvipsnames
---

## Today's Journey

\begin{textblock*}{0cm}(-0.5cm,-4.05cm)
\includegraphics[width=\paperwidth,height=\paperheight]{../../src/map.png}
\end{textblock*}

\notelist{
  \item Today, I'm taking you on this journey.
  \item We'll start out in the Valley of Programmer Death...
  \item (all the parts)
  \item This will be a fast-paced tour of state machines and type systems
  \item I'll show examples of what your compiler can help you with
  \item Hopefully I can inspire you to learn more on your own
}

# State

## Stateful Programs

* The program *remembers* previous events
* It may transition to another state based on its current state

\notelist{
  \item A stateful program remembers previous events
  \item We call that the program state
  \item It may transition to another state...
}

## Implicit State

- The program does not explicitly define the set of legal
  states
- State is scattered across many mutable variables
- Hard to follow and to ensure the integrity of state transitions
- Runtime checks "just to be sure"

\notelist{
  \item One problem with stateful programs is implicit state
  \item With "implicit", I mean ... (read list)
  \item We have to do runtime checks of variables all over the place.
}

## Making State Explicit

* Instead, we can make states *explicit*
* It is clearer how we transition between states
* Make stateful programming less error-prone

\notelist{
  \item Instead, we can make states \textbf{explicit}
  \item which makes it more clear how we transition ...
  \item It makes stateful programming less error-prone
}

# Finite-State Machines

## Finite-State Machines

- We model a program as an abstract *machine*
- The machine has a finite set of *states*
- The machine is in one state at a time
- *Events* trigger state transitions
- From each state, there's a set of legal transitions, expressed as
  associations from events to other states

\notelist{
  \item With finite-state machines, ...
  \item (rest of items)
  \item Many of you have surely heard of finite-state machines before
}

## Our Definition

> \Large{State(S) $\times$ Event(E) $\to$ Actions (A), State(S$\prime$)}

\vfill

> If we are in state S and the event E occurs, we should perform
> the actions A and make a transition to the state S$\prime$.
>
> — Erlang FSM Design Principles \fnote{\url{http://erlang.org/documentation/doc-4.8.2/doc/design_principles/fsm.html}}

\vfill

\notelist{
  \item For this talk, we will borrow the definition from Erlang's documentation
  \item (read definition)
  \item This is a conceptual model
  \item We won't necessarily write everything as functions, from state
    and event, to actions and state
}

## Excluded

* Not strictly Mealy or Moore machines
* No hierarchical machines
* No guards in our models
* No UML statecharts

\notelist{
  \item We are not talking about Mealy or Moore machines strictly
  \item We are not doing explicit hierarchies of state machines
  \item We won't use guards on transitions
  \item We won't go crazy with UML statecharts
}

## States as Data Types

![](../../src/haskell.png){width=20%}

* We model the set of legal states as a data type
* Each state has its own value constructor
* You can do this in most programming languages
* We'll use Haskell to start with

\notelist{
  \item (read list)
}

# Encoding with Algebraic Data Types

## Example: Checkout Flow

![](../uml/checkout.png){height=65%}

\notelist{
  \item Our first real-world example is a shopping cart checkout
  \item The checkout begins with no items
  \item One or more items can be added
  \item Then the \textbf{checkout} can start
  \item All states in the big box can be cancelled from
  \item Inside the checkout we start with no card
  \item We select a card, ending up in CardSelected
  \item The card is confirmed, and then we place the order
  \item This is simplified, but illustrates some important points
}

## States as an ADT

```{.haskell include=src/listings/haskell-examples/src/Checkout/ADT.hs snippet=states}
```

\notelist{
  \item Here's our state data type
  \item (explain constructors)
}

## Events as an ADT

```{.haskell include=src/listings/haskell-examples/src/Checkout/ADT.hs snippet=events}
```

\notelist{
  \item The events are also a data type
  \item (explain constructors)
}

## FSM Type

```{.haskell include=src/listings/haskell-examples/src/Checkout/ADT.hs snippet=pure-fsm}
```

```{.haskell include=src/listings/haskell-examples/src/Checkout/ADT.hs snippet=checkout-signature}
```

\notelist{
  \item With that in place, our first attempt at an FSM might be this
  \item A function from state, to event, to the next state
  \item Doesn't look too bad, very close the the Erlang definition
  \item We can pattern match on the current state and the event
  \item The problem, in our case, is that we cannot interleave side-effects
}

## State Machine with IO

```{.haskell include=src/listings/haskell-examples/src/Checkout/ADT.hs snippet=impure-fsm}
```

\notelist{
  \item So we change the FSM type to this.
  \item A function from state, to event, to IO of state.
  \item Each transition can now perform side-effects.
}

## Checkout using ImpureFSM

```{.haskell include=src/listings/haskell-examples/src/Checkout/ADT.hs snippet=checkoutImpure-signature}
```

\notelist{
  \item And we write a new version of checkout that is impure
}

## Checkout using ImpureFSM (cont.)

```{.haskell include=src/listings/haskell-examples/src/Checkout/ADT.hs snippet=checkoutImpure-definition-select}
```
```{.haskell}
...
```

\notelist{
  \item The first pattern creates the non-empty list with the first item
  \item The second adds an item to the list
}

## Checkout using ImpureFSM (cont.)

```{.haskell}
...
```
```{.haskell include=src/listings/haskell-examples/src/Checkout/ADT.hs snippet=checkoutImpure-definition-place-order}
```

\notelist{
  \item Suppose we have a payment provider module
  \item We can interleave that side-effect when an order is placed
  \item But how would you run such a thing?
}

## Impure Runner

```{.haskell include=src/listings/haskell-examples/src/Checkout/ADT.hs snippet=runImpure}
```

\notelist{
  \item A very simple runner could look like this.
}

## Logging FSM

```{.haskell include=src/listings/haskell-examples/src/Checkout/ADT.hs snippet=withLogging}
```

\notelist{
  \item We can easily decorate it with logging
  \item This takes an FSM and returns another one
  \item It logs the current state, the event, and the next state
}

## Impure Runner Example

```{.haskell include=src/listings/haskell-examples/src/Checkout/ADT.hs snippet=runImpure-example dedent=2}
```

\notelist{
  \item When we run it, from NoItems and with these events...
}

## Impure Runner Example Output

\verbatimfont{\tiny}
```
- NoItems × Select "food" → HasItems ("food" :| [])
- HasItems ("food" :| []) × Select "fish" → HasItems ("fish" :| ["food"])
- HasItems ("fish" :| ["food"]) × Checkout → NoCard ("fish" :| ["food"])
- NoCard ("fish" :| ["food"]) × SelectCard "visa" → CardSelected ("fish" :| ["food"]) "visa"
- CardSelected ("fish" :| ["food"]) "visa" × Confirm → CardConfirmed ("fish" :| ["food"]) "visa"
Charging $666
- CardConfirmed ("fish" :| ["food"]) "visa" × PlaceOrder → OrderPlaced
```

\notelist{
  \item We see this output.
  \item Notice the side-effect printing on the second to last line.
}

## ADT Summary

* We have explicit states using data types
* Standardized way of running state machine programs
    - It's simple to add logging, metrics
    - Instead of a list of events, we could use conduit\fnote{\url{https://hackage.haskell.org/package/conduit}} or pipes\fnote{\url{https://hackage.haskell.org/package/pipes}}
* We still have IO coupled with transitions (harder to test)
* Legal state transitions are not enforced

\notelist{
  \item We have explicit states and events using data types
  \item We can provide standardized runners, logging, metrics, etc
  \item Instead of a list of events, we might consider the pipes library
  \item Transitions are coupled with IO
  \item We can't test the state machine in a pure setting
  \item While illegal states can't be constructed, illegal transitions
  can be made
  \item So, can we get more flexibility with regards to testing?
  \item And, can we get stronger guarantees?
}

# MTL Style and Associated Types

## MTL Style with an Associated Type

* We will write our state machines in "MTL style"
* Some extra conventions for state machines
* With MTL style, we can:
    - combine with monad transformers (error handling, logging, etc)
    - build higher-level machines out of lower-level machines

\notelist{
  \item (read list)
}

## Typeclass and Abstract Program

* A typeclass encodes the state machine transitions
* Events are represented as typeclass methods
* The current state is passed as a *value*
* The state transitioned to is returned as a value
* The state type is *abstract* using an associated type alias
* We write a program depending on the typeclass
* The typeclass and the program together form the state machine

## Instances

* An instance is required to run the state machine program
* The instance performs the state transition side-effects
* The instance chooses the concrete data type
* We can write test instances without side-effects

\notelist{
  \item (read list)
}

## States as Empty Types

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=states}
```

\notelist{
  \item We define our states as empty data types
  \item There are no values inhabiting these types, except bottom values
}

## State Machine with Class

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=state-class}
```

```{.haskell}
  ...
```

\notelist{
  \item The \textbf{Checkout} typeclass contains the events and state transitions
  \item The associated type \textbf{State} of \textbf{m} has kind \texttt{* -> *}
  \item You can think of it as type constructor, a function taking one
  type, returning another
  \item Now let's look at some of the methods
}

## State Machine with Class (cont.)

The `initial` method gives us our starting state:

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=state-class-initial dedent=2}
```

\notelist{
  \item Initial gives us the starting state
  \item Here we see the associated type being used
  \item It is parameterized by the empty type for a specific state
  \item In this case it's \textbf{NoItems}
}

## State Machine with Class (cont.)

Some events transition from exactly one state to another:

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=state-class-confirm dedent=2}
```

\notelist{
  \item Some events are simple
  \item They transition from exactly one state to another
  \item \texttt{confirm} goes from \texttt{CardSelected} to
    \texttt{CardConfirmed}
}

## The Select Event

* Some events are accepted from many states
* Both `NoItems` and `HasItems` accept the `select` event
* We could use `Either`

\notelist{
  \item Some events are more complicated
  \item Remember, both \texttt{NoItems} and \texttt{HasItems} accept
    the \texttt{select} event
  \item We have to pass in either one of
    those states to the \texttt{cancel} event
  \item We could use \texttt{Either}, but ...
}

## Selection States

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=select-state}
```

\notelist{
  \item A specialized datatype communicates intent better, I think
  \item It has one constructor for each valid select state
}

## Signature of `select`

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=state-class-select dedent=2}
```

\notelist{
  \item We pass a \texttt{SelectState} to \texttt{select}
  \item and a \texttt{CartItem}
  \item We get back the \textit{HasItems} state
}

## The Cancel Event

* There are *three* states accepting `cancel`
* `Either` would not work, only handles *two*
* Again, we create a datatype:
    ```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=cancel-state}
    ```
* And the signature of `cancel` is:
    ```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=state-class-cancel dedent=2}
    ```

\notelist{
  \item The are \textbf{three} states accepting \texttt{cancel}
  \item The Either type would be even worse now
  \item Again, we create a datatype
  \item (explain)
  \item We pass a \texttt{CancelState} to \texttt{cancel}
}

## The Complete Typeclass

```{.haskell}
class Checkout m where
  type State m :: * -> *
  initial :: m (State m NoItems)
  select ::
       SelectState m
    -> CartItem
    -> m (State m HasItems)
  checkout :: State m HasItems -> m (State m NoCard)
  selectCard ::
       State m NoCard -> Card -> m (State m CardSelected)
  confirm ::
       State m CardSelected -> m (State m CardConfirmed)
  placeOrder ::
       State m CardConfirmed -> m (State m OrderPlaced)
  cancel :: CancelState m -> m (State m HasItems)
  end :: State m OrderPlaced -> m OrderId
```

\notelist {
  \item Here's the whole typeclass
  \item This corresponds to the diagram very nicely
  \item (show diagram, next slide)
  \item Now, we will write a program using our typeclass.
}

\pause
\begin{textblock*}{0cm}(1.5cm,-6.5cm)
\fcolorbox{black}{white}{\marginbox{.5cm}{\includegraphics[width=\paperheight-2cm]{../uml/checkout.png}}}
\end{textblock*}

## A State Machine Program

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=fillCart}
```

\notelist{
  \item \texttt{fillCart} constrains \texttt{m} to be a monad with
    Checkout and IO operations
  \item Now I must point out that I'm using IO in this program to ...
  \item Its type says it transitions from \texttt{NoItems} to \texttt{HasItems}
  \item It asks for an item, and selects that.
  \item \textbf{Note} how we wrap the state in the appropriate SelectState constructor
  \item Finally, \texttt{fillCart} hands over to \texttt{selectMoreItems}
}

## A State Machine Program (cont.)

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=selectMoreItems}
```

\notelist{
  \item \texttt{selectMoreItems} says in its type that it remains in \texttt{HasItems}
  \item If the user wants one more item, it selects that, and recurses
  \item Otherwise, it returns
}

## A State Machine Program (cont.)

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=startCheckout}
```

\notelist{
  \item \texttt{startCheckout}s type says it transitions from
    \texttt{HasItems} to \textit{OrderPlaced}
  \item That matches what we have so far
  \item We ask for a card, and a confirmation
  \item As soon as we transition state, we need to use the resulting
    state value
  \item But we can't cheat and skip a step
  \item In the end, if the user confirmed, we place the order
  \item If not, we cancel, possibly select more items, and restart the checkout
}

## A State Machine Program (cont.)

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=checkoutProgram}
```

\notelist{
  \item This is the main program
  \item It creates a checkout flow, runs is, and ends it
  \item What we get back on completion is an order ID
  \item So far, we have seen the \textit{abstract} part of this program
}

## The Abstract Part

* We only depend on the `Checkout` typeclass\fnote{We do use
  \texttt{MonadIO} to drive the program, but that could be extracted.}
* Together with the typeclass, `checkoutProgram` forms the state
  machine

\notelist{
    \item (read list)
}

## A Checkout Instance

* We need an instance of the `Checkout` class
* It will decide the concrete `State` type
* The instance will perform the effects at state transitions
* We'll use it to run our `checkoutProgram`

\notelist{
  \item (read list)
}

## Concrete State Data Type

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=CheckoutState}
```

\notelist{
  \item This is our concrete state data type
  \item It may look daunting, but bear with me
  \item CheckoutState is a GADT parameterized by a type \texttt{s}
  \item It has a constructor for each state
  \item Each constructor maps to one of the empty state types
  \item \textit{NoItems} the constructor isn't the same as
    \textit{NoItems} the type
  \item CheckoutState is a concretion of the abstract State type
  \item It's not shown here explicitly, but \texttt{CheckoutState} has kind \textit{* -> *}
}

## CheckoutT

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=CheckoutT}
```

\notelist{
  \item Given that we have a CheckoutT newtype with some monad machinery...
}

## Checkout Instance

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=instance-head}
```
```{.haskell}
  ...
```

\notelist{
  \item We can define an instance of \texttt{Checkout}
  \item For this instance, we associate the \texttt{State} type with our concrete \texttt{CheckoutState} data type
  \item And here are a few of the instance methods...
}

## Initial State

```{.haskell}
  ...
```
```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=instance-initial}
```
```{.haskell}
  ...
```

\notelist{
  \item \texttt{initial} simply constructs the \texttt{NoItems} state
}

## Select

```{.haskell}
  ...
```
```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=instance-select}
```
```{.haskell}
  ...
```

\notelist{
  \item \texttt{select} prepends an item to a new or existing list, returning \texttt{HasItems}
  \item Note that we work with our concrete state data type here
  \item And we don't have to handle state-event-mismatches
  \item The compiler knows this is exhaustive
}

## Select

```{.haskell}
  ...
```
```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=instance-placeOrder}
```

\notelist{
  \item \texttt{placeOrder} generates an order ID
  \item it calculates the price
  \item it charges the customer's card
  \item and finally, returns the \texttt{OrderPlaced} state
}

## Putting it all together

```{.haskell include=src/listings/haskell-examples/src/Checkout/MTLStyle.hs snippet=example}
```

\notelist{
  \item We run our abstract \texttt{checkoutProgram} using
    \texttt{runCheckoutT}, ending up in IO
  \item We could write an instance for tests without side-effects
}

# Timeouts

## State Machine with Timeout

![](../uml/flight-booking.png){height=60%}

\notelist{
  \item This is similar to the checkout, but has a little twist
  \item We begin by selecting the flight, then selecting the seat
  \item The seat is now reserved, which it can be for 5 minutes
  \item Then it times out, going to \textbf{SeatReleased}
  \item Unless the booking has been cancelled, or the seat has been payed for
  \item In those cases we end up in terminal states
  \item If the reservation timed out, we can either cancel the
    booking, or restart with the flight selected
}

## Flight Booking States

```{.haskell include=src/listings/haskell-examples/src/FlightBooking.hs snippet=states}
```

\notelist{
  \item Like before, we use empty data types for states
}

## Cancel and End States

```{.haskell include=src/listings/haskell-examples/src/FlightBooking.hs snippet=multi-states}
```

\notelist{
  \item We model multiple states accepting an event with a data type
}

## Flight Booking Events and Transitions

```{.haskell include=src/listings/haskell-examples/src/FlightBooking.hs snippet=flight-booking-class}
```

\notelist{
  \item The \texttt{FlightBooking} class contains our events and transitions
  \item There's nothing new going here that we haven't talked about
  \item (explain quickly)
}

## Flight Booking Program

```{.haskell include=src/listings/haskell-examples/src/FlightBooking.hs snippet=book-flight}
```

\notelist{
  \item Now, let's look at the flight booking program
  \item Don't worry about \texttt{MonadBaseControl}, it's just
    plumbing we need for timeouts
  \item This program is the entry point, and we start by selecting a
    flight number
  \item Then we pass over to \texttt{withFlightSelected}
}

## Flight Booking Timeout

```{.haskell include=src/listings/haskell-examples/src/FlightBooking.hs snippet=flight-selected-start}
```

```{.haskell}
    ...
```

\notelist{
  \item In \texttt{withFlightSelected} we select a seat
  \item We're now in the \texttt{SeatReserved} state
  \item Then we start a timeout for the confirmation to buy
  \item In a real system this would be more than just a confirmation,
    but I've kept it simple
  \item Then we wait for an answer, which can be one of \textbf{three} values
}

## Flight Booking Timeout (cont.)

```{.haskell}
...
```

```{.haskell include=src/listings/haskell-examples/src/FlightBooking.hs snippet=flight-selected-booked dedent=4}
```

```{.haskell}
...
```

\notelist{
  \item \texttt{Just True} means that we got an answer, and that it
    was a confirmation
  \item So we transition into seat booked and end the booking
}

## Flight Booking Timeout (cont.)

```{.haskell}
...
```

```{.haskell include=src/listings/haskell-examples/src/FlightBooking.hs snippet=flight-selected-cancelled dedent=4}
```

```{.haskell}
...
```

\notelist{
  \item \texttt{Just False} means that we got an answer, but that is
    was negative
  \item We cancel the reservation and end the booking
}

## Flight Booking Timeout (cont.)

```{.haskell}
...
```

```{.haskell include=src/listings/haskell-examples/src/FlightBooking.hs snippet=flight-selected-timeout dedent=4}
```

\notelist{
  \item \texttt{Nothing} means that we got no answer in time, the reservation timed out
  \item We transition to \texttt{SeatReleased} and ask if the user
    wants a restart
  \item If so, we restart, and recurse
  \item If not, we cancel and end the booking
}

## Summary

* We've modeled state machines using:
    - Type classes/MTL style
    - Associated types for states
    - Explicit state values
    - "Abstract" program
    - Instances for side-effects
* Stricter than ADT-based version
* Not necessarily safe
    - State values can be *reused* and *discarded*
    - Side-effects can be reperformed *illegally*
    - Nothing enforcing transition to a terminal state

\notelist{
  \item To summarize the checkout and flight booking parts...
  \item (read list)
  \item Stricter: we encode state transitions, not only legal states
}

## Reusing State Values

```{.haskell}
payTwice seatReserved = do
  _ <- payForSeat seatReserved

  seatBooked <- payForSeat seatReserved
  log "You've payed twice, LOL."

  end (BookedEnd seatBooked)
```

\notelist{
  \item As an example of \textit{reusing} state values...
  \item Here we first pay for the seat, and ignore the resulting state
  \item Then we pay \textbf{again}, taunt the user, and end the booking
  \item This is probably not in accordance with our business model
  \item We like the compiler to tell us
}

## Monad, Carry Thy State!

* One solution would be linear types
* Another is to carry the state *inside* the monad
* No need for explicit state values:

    ```{.haskell}
    payTwice = do
        payForSeat
        payForSeat -- BOOM, type error!
        end
    ```
* We parameterize the monad, or *index* it, by the state type

\notelist{
  \item One solution would be linear types
  \item We'd then know that a state value is used exactly once
  \item But I won't talk about linear types
  \item Instead, ... (read list)
}

# Indexed Monads

## Indexed Monad Type Class

* A monad with two extra type parameters:
    - Input
    - Output
* Can be seen as type before and after the computation
* Type class:

    ```{.haskell}
    class IxApplicative m => IxMonad (m :: k -> k -> * -> *) where
        ...
    ```

\notelist{
  \item As the name suggests, it's a monad that is \textbf{indexed}
  \item By what? By two type parameters, input and output
  \item (read list)
  \item (explain typeclass)
  \item The indexed monad has an operation called \texttt{ibind}
  \item I will compare it to regular monadic bind
}

## Monad bind

![](../../src/bind.png){width=80%}

\notelist{
  \item Here is regular monadic bind
  \item How many here know how monadic bind work?
  \item (explain briefly)
}

## ibind (simplified)

![](../../src/ibind-no-arrows.png){width=80%}

\notelist{
  \item Now I have added \texttt{ibind} underneath and lined up the
    matching parts
  \item We see \textbf{i, j, and k} as new things
  \item What's going on here?
}

## ibind (simplified)

![](../../src/ibind.png){width=80%}

\notelist{
  \item The input of the first monadic value is the input of the
    resulting monadic value (red arrow)
  \item The output of the second monadic value is the output of the
    resulting monadic value (blue arrow)
  \item The \texttt{j} is the intermediate index (green arrow)
  \item This might remind you of function composition, and it's type signature
}


## Specializing `ibind`

```{.haskell}
ibind
  ::        m i      j      a
  -> (a  -> m j      k      b )
  ->        m i      k      b
```

\notelist{
  \item This is ibind again
  \item To show you how we will you use it, I'll specialize it
  \item We swap the variables for some custom state types
}

## Specializing `ibind` (cont.)

```{.haskell}
ibind
  ::        m State1 State2 ()
  -> (() -> m State2 State3 ())
  ->        m State1 State3 ()
```

\notelist{
  \item We see how the result transitions from state 1 to state 3.
}

## Indexed Bind Example

```{.haskell}
checkout :: m HasItems NoCard ()

selectCard :: m NoCard CardSelected ()

(checkout `ibind` const selectCard) :: m HasItems CardSelected ()
```

\notelist{
  \item As a last example, here we bind two transitions from the
    Checkout example
  \item The result transitions all the way from \texttt{HasItems} to
    \texttt{CardSelected}
}

## Indexed State Monad

* We hide the state *value*
* Only the state type is visible
* We cannot evaluate a computation twice *unless the type permits it*

\notelist{
  \item Using an indexed state monad, (read list)
}

## Composability

* The indexed monad describe *one* state machine
* Hard to compose
* We want *multiple* state machines in a single computation
    - Opening two files, copying from one to another
    - Ticket machine using a card reader and a ticket printer
    - A web server and a database connection
* One solution:
    - A type, mapping from names to states, as the index
    - Named state machines are independent
    - Apply events *by name*

\notelist{
  \item We have seen that ... (read list)
}

## Row Types in PureScript

* PureScript has a *row kind* (think type-level record):

    ```{.haskell}
    (out :: File, in :: Socket)
    ```
* Can be polymorphic:

    ```{.haskell}
    forall r. (out :: File, in :: Socket | r)
    ```

* Used as indices for record and effect *types*:

    ```{.haskell}
    Record (out :: File, in :: Socket)
    -- is the same as:
    { out :: File, in :: Socket }
    ```

\notelist{
  \item (explain snippets)
}

## Row Types for State Machines

```{.haskell}
-- Creating `myMachine` in its initial state:
initial
  :: forall r
   . m r (myMachine :: InitialState | r) Unit

-- Transitioning the state of `myMachine`.
someTransition
  :: forall r
   . m (myMachine :: State1 | r) (myMachine :: State2 | r) Unit

-- Deleting `myMachine` when in its terminal state:
end
  :: forall r
   . m (myMachine :: TerminalState | r) r Unit
```

\notelist{
  \item (explain snippets)
}

## Running Row Type State Machines

```{.haskell}
runIxMachines
    :: forall m
    . Monad m
    => IxMachines m () () a      -- empty rows!
    -> m a
```

\notelist {
  \item A type signature can require each created state machine to be removed:
  \item The indices in \texttt{runIxMachines} are empty rows
}

## Related Libraries

* `Control.ST` in Idris contrib library\fnote{\url{http://docs.idris-lang.org/en/latest/st/state.html}}
* "purescript-leffe" (The **L**abeled **Eff**ects **E**xtension)\fnote{\url{https://github.com/owickstrom/purescript-leffe}}
* "Motor" for Haskell\fnote{\url{http://hackage.haskell.org/package/motor}}

## More on Indexed Monads

* Read the introduction on "Kwang's Haskell Blog"\fnote{\url{https://kseo.github.io/posts/2017-01-12-indexed-monads.html}}
* Haskell package `indexed`\fnote{\url{https://hackage.haskell.org/package/indexed}}
* Also, see `RebindableSyntax` language extension
* Can be combined with session types\fnote{\href{http://users.eecs.northwestern.edu/~jesse/pubs/haskell-session-types/}{Riccardo
  Pucella and Jesse A. Tov, Haskell session types with (almost)
  no class, Haskell '08.}}

\notelist{
  \item If you want to learn more about indexed monads in general...
  \item (read list)
}

# Dependent Types in Idris

## Idris and Control.ST

![](../../src/idris.png){width=30%}

* Dependent types makes some aspects more concise
    - Multiple states accepting an event
    - Error handling
    - Dependent state types
* The `Control.ST` library in Idris supports multiple "named" resources
* "Implementing State-aware Systems in Idris: The ST Tutorial"\fnote{\url{http://docs.idris-lang.org/en/latest/st/index.html}}

\notelist{
  \item For the last section of this talk, I'll show some Idris code
  \item Idris is not currently recommended to use for anything critical
  \item But we can steal ideas!
  \item (read list)
}

## Revisiting Checkout

![](../uml/checkout.png){height=65%}

\notelist{
  \item Let's revisit our checkout state machine
  \item We had NoItems and HasItems as separate states
}

## Extended State HasItems

![](../uml/checkout-guard.png){height=65%}

\notelist{
  \item In Idris, we can combine them, using a dependent state type
  \item The \texttt{n} is a natural number, the number of items selected
  \item \texttt{select} increments that number by 1
  \item \texttt{checkout} requires it to be greater than 0
}

## Protocol Namespace

```{.idris include=src/listings/idris/Checkout.idr snippet=Protocol}
```
```{.idris}
  ...
```

\notelist{
  \item We define the abstract part, or the protocol, in a separate namespace
  \item (quick description)
}

## Checkout States

```{.idris include=src/listings/idris/Checkout.idr snippet=CheckoutState dedent=2}
```

\notelist{
  \item Our states is a union type
  \item Note that all states except the last know how many items we have
}

## Checkout Interface

```{.idris include=src/listings/idris/Checkout.idr snippet=Checkout dedent=2}
```
```{.idris}
  ...
```

\notelist{
  \item The \texttt{Checkout} interface has a State type
  \item This is very similar to our associated type in Haskell
  \item It is also parameterized by a checkout state
}

## Initial State

```{.idris include=src/listings/idris/Checkout.idr snippet=Checkout-initial dedent=4}
```

\notelist{
  \item The initial operation adds the \texttt{(HasItems 0)} state and
    returns a \texttt{Var}
  \item We don't have to create the Vars ourselves
}

## One More Item

```{.idris include=src/listings/idris/Checkout.idr snippet=Checkout-select dedent=4}
```

\notelist{
  \item \texttt{select} takes a Var and an Item
  \item It transitions from having N items, to having the successor of N items
  \item In other words: one more item
}

## Checking Out Requires Items

```{.idris include=src/listings/idris/Checkout.idr snippet=Checkout-checkout dedent=4}
```

\notelist{
  \item \texttt{checkout} takes a Var
  \item It transitions from having at least one item to the NoCard state
  \item We still keep track of the number of items
}

## States Accepting Cancel

* Again, we have *three* states accepting cancel
* In Idris we can express this using a predicate over states
* "Give me proof that your current state accepts cancel"

\notelist{
  \item (read list)
  \item (What we're saying to the caller is)
}

## Cancellable State Predicate

```{.idris include=src/listings/idris/Checkout.idr snippet=CancelState dedent=2}
```

\notelist{
  \item The proof will be a value of this type
  \item There are three constructors, one for each cancellation state
  \item Looking at the type of \texttt{CancelState}, we see that it's
    parameterized by a checkout state and a natural number
}

## Cancelling

```{.idris include=src/listings/idris/Checkout.idr snippet=Checkout-cancel dedent=4}
```

\notelist{
  \item In the \texttt{cancel} operation we take a Var
  \item We also ask for a proof, a CancelState value for the current state
  \item the \textbf{auto} keyword tells Idris to construct and pass it
    automatically for us
  \item We then transition to the \texttt{(HasItems n)} state
  \item This is why the number of items was tracked all the way through
  \item When we go back to HasItems, we need to know how many we have
}

## Console Checkout Program

```{.idris include=src/listings/idris/Checkout.idr snippet=selectMore dedent=4}
```

\notelist{
  \item Instead of showing you the boring implementation of Checkout ...
  \item Let's look at a program using it
  \item \texttt{selectMore} selects one more item
  \item Depending on the type-level Nat, we can do a value level conditional
}

## Console Checkout Program (cont.)

```{.idris include=src/listings/idris/Checkout.idr snippet=checkoutWithItems dedent=4}
```

\notelist{
  \item \texttt{checkoutWithItem} transitions from HasItems to OrderPlaced
  \item OR, stays in HasItems, if the checkout is cancelled
  \item The state change depends on the Bool return value
  \item We ask for a credit card, and eventually we place the order
  \item In between we ask if the user wants to continue or cancel
  \item We could not cancel from any other place than these ones
}

## Console Checkout Program (cont.)

```{.idris include=src/listings/idris/Checkout.idr snippet=checkoutOrShop dedent=4}
```

\notelist{
  \item \texttt{checkoutOrShop} removes a resource in state \texttt{HasItems}
  \item It will try checkoutWithItems and complete the checkout flow
  \item If the user cancels, it loops using \texttt{goShopping}
}

## Console Checkout Program (cont.)

```{.idris include=src/listings/idris/Checkout.idr snippet=goShopping dedent=4}
```

\notelist{
  \item In \texttt{goShopping} we select one more item
  \item Then we ask if the user wants to checkout
  \item If not, we go shopping again
  \item These last two functions go back and forth until the user has
    made a purchase
}

## Console Checkout Program (cont.)

```{.idris include=src/listings/idris/Checkout.idr snippet=program dedent=2}
```

\notelist{
  \item The program is started like this
  \item (explain)
  \item Note that all functions so far are total
  \item If you have a hard time convincing your boss about using
    Idris at work, tell them that it can prove your users will buy stuff.
}

## Console Checkout Program (cont.)

```{.idris include=src/listings/idris/Checkout.idr snippet=runCheckout}
```

\notelist{
  \item And to have total functions that loop, we need this runLoop forever
  \item The printing of Oops happens if we run out of foreverness.
}

# Summary

## Summary

* Implicit state is hard and unsafe when it grows
    - Very unclear, no documentation of states and transitions
    - "Better safe than sorry" checks all over the place
* Just making the states explicit is a win
    - You probably have "hidden" state machines in your code
    - Use data types for states and events (ADTs)
    - This can be done in most mainstream languages!

## Summary (cont.)

* By lifting more information to types, we can get more safety
    - You can do *a lot* in Haskell and PureScript
    - Protect side-effects with checked state transitions
    - Even better documentation
    - Make critical code testable
* Steal ideas from other languages
    - Dependent types, linear types
* No silver bullet

## Takeaway

\centering{\Large{Reify your design in code.}}

# Questions?

## Links

* Slides and code:
  [github.com/owickstrom/fsm-your-compiler-wants-in](https://github.com/owickstrom/fsm-your-compiler-wants-in)
* Website: [https://wickstrom.tech](https://wickstrom.tech)
* Twitter: [\@owickstrom](https://twitter.com/owickstrom)
