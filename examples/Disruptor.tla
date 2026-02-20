---- MODULE Disruptor ----
\* SPMC (Single Producer, Multiple Consumers) ring buffer.
\* Models the LMAX Disruptor's core gating mechanism: a single producer
\* writes events sequentially into a fixed-size ring buffer while multiple
\* independent consumers read at their own pace.  The producer must not
\* overwrite slots that the slowest consumer has not yet consumed.

EXTENDS Naturals

CONSTANTS BufSize,        \* Number of slots in the ring buffer (positive Nat)
          NumConsumers,   \* Number of independent consumers   (positive Nat)
          MaxEvents       \* Model-checking bound on total events produced

ASSUME BufSize > 0 /\ NumConsumers > 0 /\ MaxEvents > 0

Consumers == 0 .. NumConsumers - 1

\* We use MaxEvents as the sentinel for "empty" slots.  Valid event values
\* are 0 .. MaxEvents - 1, so MaxEvents is outside the data range.
Empty == MaxEvents

VARIABLES buffer,     \* Ring buffer: slot index -> Empty | event value
          published,  \* Producer cursor: next sequence number to write
          consumed    \* Per-consumer cursors: sequence number of next event to read

vars == <<buffer, published, consumed>>

\* ---- Derived operators ------------------------------------------------

MinConsumed ==
    LET S == {consumed[c] : c \in Consumers}
    IN CHOOSE x \in S : \A y \in S : x <= y

Slot(seq) == seq % BufSize

\* ---- Initial state ----------------------------------------------------

Init ==
    /\ buffer    = [i \in 0 .. BufSize - 1 |-> Empty]
    /\ published = 0
    /\ consumed  = [c \in Consumers |-> 0]

\* ---- Actions ----------------------------------------------------------

Publish ==
    /\ published < MaxEvents
    /\ published - MinConsumed < BufSize
    /\ buffer'    = [buffer EXCEPT ![Slot(published)] = published]
    /\ published' = published + 1
    /\ UNCHANGED consumed

Consume(c) ==
    /\ consumed[c] < published
    /\ consumed' = [consumed EXCEPT ![c] = consumed[c] + 1]
    /\ UNCHANGED <<buffer, published>>

Next ==
    \/ Publish
    \/ \E c \in Consumers : Consume(c)

Fairness ==
    /\ WF_vars(Publish)
    /\ \A c \in Consumers : WF_vars(Consume(c))

Spec == Init /\ [][Next]_vars /\ Fairness

\* ---- Type invariant ---------------------------------------------------

TypeOK ==
    /\ buffer    \in [0 .. BufSize - 1 -> 0 .. MaxEvents]
    /\ published \in 0 .. MaxEvents
    /\ consumed  \in [Consumers -> 0 .. MaxEvents]

\* ---- Safety invariants ------------------------------------------------

\* The producer never overwrites a slot that some consumer still needs.
NoOverwrite ==
    \A c \in Consumers :
        published - consumed[c] <= BufSize

\* No consumer reads ahead of the producer.
NoReadAhead ==
    \A c \in Consumers :
        consumed[c] <= published

\* Every consumed event was the correct value for its sequence number.
\* For every sequence number seq still visible in the buffer (between
\* MinConsumed and published - 1), the buffer slot holds the correct value.
ReadCorrectness ==
    published > 0 =>
        \A seq \in MinConsumed .. published - 1 :
            buffer[Slot(seq)] = seq

\* ---- Liveness ---------------------------------------------------------

AllConsumed ==
    \A c \in Consumers : consumed[c] = MaxEvents

Liveness == (published = MaxEvents) ~> AllConsumed

====
