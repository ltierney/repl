# An R implementation of the top level REPL

Originally put together in 2008 to find out if all the pieces needed
to build a full REPL at the R revel are available.

The public interface is

- `repl()` for running a read-eval-print loop
- `quit()` for exiting the repl.

I believe most major features available in 2008 are at least
approximately supported, except for

- catching a `Q` in a `browser`;
- top level callbacks.

`Q` could be handled either by adding an appropriate restart in
`repl()` and modifying the C internals of `browser`, or, better in the
long run, by also writing the browser code in R and modifying
`do_browser` to use the R version.  Some fiddling with context entries
is needed for that and to make sure that efficiency of `return()`
calls isn't messed up. (With most code now being byte compiled this is
less of an issue.)

Some minor issues:

- `return(1)` at repl top level does not give an error and probably
  should.

-  In gui's the gui may think it is busy and so show the hour glass
   (`Rgui`)/spinner(`AQUA`).  The spinner in AQUA seems to go away
   after a while.

- Need to set up for i18n/L10n

- Prompts should be determined by options settings and startup flags
  -- for now a fixed pair of prompts is used.

- `readline`/input editing isn't quite right on at least some
  platforms (tty in particular).  On `AQUA` `do_addhistory` doesn't
  seem to do anything.

Color support in `xterm` was provided by Deepayan Sarkar. This should
probably be made configurable in some way.
