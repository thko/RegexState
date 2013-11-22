RegexState
==========

A state-machine based regular expression matcher.
Theoretically, it should run in O(n*m), where *n* is the length
of the regular expression, and *m* is the length of the string
being matched.

`compileRegex :: String -> Either RegexError RegexState` turns
a regex string into an NFA, or issues an error when the regex
is malformed.

Based on the article [Regular Expression Matching Can Be Simple
And Fast](http://swtch.com/~rsc/regexp/regexp1.html).
