% Crashes through the year
% Chris Knox

This markdown file is passed through a modified markdown parser.
The modified parse will split the markdown into list of labels
and blocks of markdown. The separator between blocks is a horizontal line
and then a level 1 header. 

```
---
# section name
```

Anything (such as this) before the first of these is discarded. 

The text in the section name is slugified and used to indentify
the blocks of markdown so that they can be rendered as used to
trigger a scrolly article.

---

# intro

After falling for several years New Zealand's road toll has started to rise again.

---

# par two

So what is going on?

Who knows?

