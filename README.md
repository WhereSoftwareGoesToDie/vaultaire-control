# vaultaire-control

This package provides some ad-hoc solutions to dealing with a transformer stack, possibly involving streaming with ``pipes``. It exists because I don't know any better. Eventually should be replaced with proper libraries.

  * ``Safe``: try to ensure resources cleanup in the face of exceptions.
  * ``Lift``: lifting operations through the stack.

A proper solution would probably involve ``pipes-safe`` for resource clean-up and something for lifting and exception handling in the stack, e.g. ``monad-control`` or ``layers``?
