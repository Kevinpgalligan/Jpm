## On parsing J
Some precise descriptions of the grammar. Quoted all the relevant parts
in the grammar definition.

  https://wiki.jsoftware.com/wiki/Books/MathForTheLayman/Language_and_Grammar

Defines various parts of the syntax and their semantics. Some relevant quotes included below, but best to read the whole thing.

  https://code.jsoftware.com/wiki/Vocabulary/Words

> The sentence is the executable element in a J script. Basically, it is one line of code.

> Sentences cannot span more than one line. There is no "continuation character" as in Basic, and no "statement delimiter" as in Java or C. 

> The executable line is treated as a single sentence, unless it contains control words, when the control words and sections of the line between control words are each treated as separate sentences. 

> Each line is a J sentence, which may include, or be, a comment, or be blank or empty.

> Control words are allowed only inside the body of an explicit definition.

The IF statement.

  https://code.jsoftware.com/wiki/Vocabulary/ifdot

T-blocks, the "test" part of an IF. Can contain multiple sentences.

  https://code.jsoftware.com/wiki/Vocabulary/TBlock

Someone's attempt to define the J grammar. They didn't get very far.

  https://wiki.jsoftware.com/wiki/User:Ian_Clark/BackusNaurFormOfJterms

Glossary. "Explicit entities are created by the : conjunction or by its synonym define, with noun operands (m : n), or by direct definition by enclosing the text of the entity inside {{ }}."

  https://code.jsoftware.com/wiki/Vocabulary/Glossary

And this describes the "m : n" syntax for defining functions.

  https://code.jsoftware.com/wiki/Vocabulary/com
  
Other:
* https://www.jsoftware.com/help/dictionary/dict2.htm
* https://code.jsoftware.com/wiki/Vocabulary/PartsOfSpeech


## Misc scratchpad
Inline noun:
   0 : 'hi'

Multiline noun
   0 : 0
      ...
   )

Also:

    noun define
        ...
    )

## Bookmarks
https://www.reddit.com/r/ProgrammingLanguages/comments/1gplj9l/comment/lws51ph/
https://code.jsoftware.com/wiki/User:Ric_Sherlock/XHTML/Script
https://code.jsoftware.com/wiki/Help/JforC/38_Parsing_and_Execution_I
https://code.jsoftware.com/wiki/Guides/Lexical_Closure
https://wiki.jsoftware.com/wiki/Guides/Language_FAQ/J_BNF
https://wiki.jsoftware.com/wiki/User:Raul_Miller/ParsingJ
