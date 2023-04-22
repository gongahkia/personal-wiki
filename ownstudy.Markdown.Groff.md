# The Groff markup language

## What is Groff?

Groff belongs to a generation of ***document preparation*** *(and typesetting systems)* that LaTex was a part of. 

Compared to LaTex however, Groff is <u>much more lightweight</u>, allowing for **speedier compilation times**, alongside **less overall bloat** on your system.

## Usage

For the specific workflow I'm using right now, I rely on Pandoc to convert `markdown` files to `pdf`, and use Groff as the converter. This is achieved by running the following command.

```console
$ pandoc {filename.md} -t ms -o {filename.pdf}
```

> For a brief explanation of the above...  
> * `pandoc`: calls the universal document converter [Pandoc](https://pandoc.org/).
> * `-t ms`: indicates to Pandoc to target the Groff `ms` macro package, a flavour of Groff with its [own syntax](https://www.gnu.org/software/groff/manual/groff.html#ms).
> * `-o`: indicates to Pandoc to name the newly created PDF with the specified name.

Notably, I have used some basic shell scripting to simplify the process first. As such, as of current *(22 April 2023)*, I simply run the following command to compile a markdown file to a PDF.

```console
$ grf {filename.md} {filename.PDF}
```

Additionally, it is **important to note** that since I am merely using Groff to compile my markdown files to a PDF, any Groff *(in this case `ms` specific)* syntax is to be enclosed in a **code block**, as follows.

````md
```{=ms}
.bp
```
````

> For a brief explanation of the above...
> * `{=ms}` indicates to Groff that this code should be rendered with the Groff `ms` macro package, instead of as regular markdown text.

Moreover, this use case allows for *LaTex-flavoured* math blocks to be added to our markdown document, and rendered as mathematical equations.

## [`ms` macro package syntax](https://manpages.ubuntu.com/manpages/bionic/en/man7/groff_ms.7.html)

Note that Groff's `ms` macro package stipulates that every command *(referred to as a Macro)* is written on a ***new line***.

| Category | Groff `ms` macro | Details | 
| :---: | :---: | :--- |
| Cover page | `.RP` | Creates a **seperate cover page** with relevant information. By default, cover page information is rendered on first page alongside document text. |
| - | `.P1` | Prints the header on page 1. By default, header is suppressed. |
| - | `.ND` | Prints the current date by default in **title page** *(if it exists)*. Arguments can be specified to change the date displayed. |
| - | `.DA` | Prints the current date by default in **title page** *(if it exists)* and **footers**. Arguments can be specified to change the date displayed. |
| - | `.TL` | Document title | 
| - | `.AU` | Author name | 
| - | `.AI` | Author's institution |
| Abstract | `.AB` | Begins the **abstract**. An optional argument `[no]` can be supplied to *suppress* the **ABSTRACT** heading. |
| - | `.AE` | Ends the **abstract**. |
| [Table of contents](https://youtu.be/sWeOEMHwmrE) | `.TC` | Creates a **table of contents**, with titles and page numbers specified in the `.XS`, `.XA` and `.XE` macros. |
| - | `.XS`, `.XA`, `.XE` | Wrap text to appear in **table of contents** in the `.XS` and `.XE` macros. `.XS` is the *first entry*, `.XA` is for *subsequent entries*. `.XS` and `.XA` take a **numeric argument**, which is the page number for the specified entry. |
| - | `.PX` | Creates a **table of contents** without resetting the page number. |
| Paragraphs | `.PP` | Creates an **indented** paragraph. | 
| - | `.LP` | Creates an **unindented** paragraph. |
| - | `.QP` | Indents all text at both **left** and **right** margins. | 
| - | `.XP` | Creates an **exdented** paragraph. |
| Headings | `.NH` | Creates a **numbered** heading. Optional numeric argument `xx` indicates the level of the heading. `S xx xx "..."` specifies the section number explicitely. Heading levels must be in order. | 
| - | `.SH` | Creates an **unnumbered** sub-heading. | 
| Highlighting | `.B` | **Bolds** the provided argument as text. The second argument is printed *after* the bolded text, and the third argument is printed *before* the bolded text. |
| - | `.R` | Sets the provided argument in roman / regular type. Other arguments operate similarly to the `.B` macro. |
| - | `.I` | Sets the provided argument in *italics*. Other arguments operate similarly to the `.B` macro. |
| - | `.CW` | Sets the provided argument in *constant width* font face. Other arguments operate similarly to the `.B` macro. | 
| - | `.BI` | Sets the provided argument in ***bolded italics***. Other arguments operate similarly to the `.B` macro. |
| - | `.BX` | Prints the provided argument in a box. For strings with spaces, use `\0` to indicate a space. |
| - | `.UL` | Sets the provided argument <u>underlined</u>. Other arguments operate similarly to the `.B` macro. | 
| - | `.LG` | Prints the provided arguments in **larger font**. This macro can be specified multiple times to further enlarge the argument displayed. |
| - | `.SM` | Prints the provided arguments in **smaller font**. This macro can be specified multiple times to further shrink the argument displayed. |
| - | `.NL` | Prints the provided arguments in **normal font**. |
| - | `\*{` `\*}` | Prints the enclosed text as *superscript*. |
| Indentation | `.RS` | Starts a section of **indented text**. | 
| - | `.RE` | Ends a section of **indented text**. |
| Lists | `.IP` | Creates a list *(ordered and unordered)*. Optional arguments `[marker [width]]`, where `marker` specified the marker character (`\(bu` bullet character for **unordered lists**, numeric value for **numbered lists**, word or phrase for glossary-style lists and `width` specifies the fixed *indent* for all list items. |
| - | `.TA` | Sets a tab stop. |
| [Tables](https://youtu.be/bTE-l4NhW1I) | `.TS` | Starts a table, rendered using the Groff `tbl` preprocessor. |
| - | `.TE` | Ends a table, rendered using the Groff `tbl` preprocessor. |
| Graphics | `.PS` | Starts a graphic section, rendered using the Groff `pic` preprocessor. | 
| - | `.PE` | Ends a graphic section, rendered using the Groff `pic` preprocessor. |
| Equations | `.EQ` | Starts an equation section, rendered using the Groff `eqn` preprocessor. |
| - | `.EN` | Ends an equation section, rendered using the Groff `eqn` preprocessor. |
| References | `.[` | Starts a reference, rendered using the Groff `refer` preprocessor. |
| - | `.]` | Ends a reference, rendered using the Groff `refer` preprocessor. |
  
There are also a bunch of [other useful `ms` macros](https://youtu.be/bvkmnK6-qao) that I haven't covered above, such as... 

* [**pdf links, references**](https://youtu.be/HQQw3gTWfRo)
* **footnotes**
* **headers and footers** 
* **displays and keeps** 
* **margin options** 
* **multiple columns** 
* **point size specification**
* [**creating macros, strings, registers**](https://youtu.be/si2-ZLQCv4g)
* [**importing fonts**](https://youtu.be/CzUHjtGBdZs)

> All these can be found easily in the Groff `ms` macro package manual.

## [Diagrams in Groff](https://youtu.be/oG2A_1vC6aM)

## [Graphs in Groff](https://youtu.be/tg56sJUJq4w)

## [Images in Groff](https://youtu.be/m1WC0Ww_S2E)

## References

* [From Markdown to Groff playlist](https://youtube.com/playlist?list=PLknodeJt-I5FgZ5VwT-BHda_lu3dYrMeJ)
* [Convert Markdown to PDF (Pandoc and Groff)](https://youtu.be/RW69tq7taXs)
* [Introduction to Groff](https://douglasrumbaugh.com/post/groff-introduction/)
* [Groff manual](https://www.gnu.org/software/groff/manual/groff.html)
* [`-me` syntax for Academic papers](https://opensource.com/article/18/2/writing-academic-papers-groff-me)
* [Most sane Ycombinator conversation about using Groff to write PHD](https://news.ycombinator.com/item?id=32202209)
