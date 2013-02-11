The main idea of this package is that we can eliminate explicit substitutions and consider context as substitution. Context can be reused in type inference algorithms, some logic routunes, or another algorithms which use unification.

So we introduce three main primitives:

* fresh - _rename_ free variables of the given term.
* unify --- _unify_ the two given terms in then given context.
* reify --- make term _tractable_ in the current context.

All another operations is derived from that three.
