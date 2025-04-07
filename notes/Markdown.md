# `Markdown`

Markdown is an easy-to-read markup format with various downstream targets available for transpilation.

## Quickstart

Different markdown flavours will have their own unique syntax quirks. 

The below is the **shared syntax** that most flavours support.

````md
----- HEADER -----

# this is a header equivalent to <h1>
## this is a header equivalent to <h2>
### this is a header equivalent to <h3>
#### this is a header equivalent to <h4>
##### this is a header equivalent to <h5>
###### this is a header equivalent to <h6>

----- ITALIC -----

*this is italicised text*
_this is also italicised text_

----- BOLD -----

**this is bolded text**
__this is also bolded text__

----- ITALIC AND BOLD -----

***this is italicised and bolded text***
___this is also italicised and bolded text___

----- BLOCK QUOTE -----

> This is a block quote.
>> This is an indented block quote.

----- PARAGRAPH -----

Markdown paragraphs are just adjacent lines of text seperated by one or more blank lines.

----- LIST -----

--- UNORDERED LIST ---

* this is an unordered list item
* this is also an unordered list item
* this is a final unordered list item

+ this synatx works too
+ look at me!
+ wow!

- as does this syntax
- with these dashes
- to create an unordered list

--- ORDERED LIST ---

1. there is only one way to 
2. create an
3. ordered list
4. however
5. and its with numbers like this
6. note that 
    * sublists like this are 
    * also valid

--- TASK LIST ---

- [ ] task lists are also
- [ ] created as shown here
- [x] this checkbox is ticked
- [ ] and all checkboxes will render as HTML checkboxes
- [ ] in your markdown document

----- CODE BLOCK ----- 

`Smaller code blocks` can be integrated in plain text with a single backtick ` `.

Larger code blocks can also be defined as shown with triple backticks ``` ``` to provide syntax-specific highlighting for the code block.

```ruby
def foobar
  puts "as below"
end
```

----- HORIZONTAL RULE ----- 

Create a <hr/> element with ---.

----- LINK -----

[This is the text associated with the link.](http://heresTheLink.com)
[As well as supporting relative paths.](README.md)
[Alongside supporting reference style links.](link1)
[And finally header links within the same file.](#quickstart)

[link1]: http://heresAReferenceStyleLink.com

----- IMAGE -----

![This is an image](http://heresTheImageUrl.com)
![Images can also be relative paths](asset/hypotheticalImage.jpg)
````

## Github flavoured markdown

The below two features have been implemented in [GFM](https://github.github.com/gfm/).

### Mathematical expressions

Github flavoured markdown supports [LaTeX formatted mathematical expressions](https://en.wikibooks.org/wiki/LaTeX/Mathematics).

````md
--- MATHEMATICAL EXPRESSION ---

Mathematical expressions are rendered inline with `$` characters.

$\sqrt{3x-1}+(1+x)^2$

Block mathematical expressions (automatically centralised) are rendered with `$$` characters.

$$\left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right) \left( \sum_{k=1}^n b_k^2 \right)$$

Block mathematical expressions can also be rendered with `math` as a code block (the below will render the same as the above).

```math
\left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right) \left( \sum_{k=1}^n b_k^2 \right)
```
````

### Alerts

Github flavoured markdown also supports alerts to signpost significant content.

```md
----- ALERT -----

> [!NOTE]
> blue circle containing exclamation mark

> [!TIP]
> green lightbulb

> [!IMPORTANT]
> purple speech bubble containing exclamation mark

> [!WARNING]
> yellow triangle containing exclamation mark

> [!CAUTION]
> red octagon containing exclamation mark
```

Rendered, they look like this.

>[!NOTE]
> blue circle containing exclamation mark

>[!TIP]
> green lightbulb

>[!IMPORTANT]
> purple speech bubble containing exclamation mark

>[!WARNING]
> yellow triangle containing exclamation mark

>[!CAUTION]
> red octagon containing exclamation mark

## More on

* [learn markdown in y minutes](https://learnxinyminutes.com/docs/markdown/)
* [markdown guide](https://www.markdownguide.org/)
* [GFM tables](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/organizing-information-with-tables)
* [markdown mathematical expressions](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/writing-mathematical-expressions)
* [alerts](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax#alerts)
* [further markdown syntax](https://www.markdownguide.org/hacks/)
* [handling markdown anchor links with same name](https://stackoverflow.com/questions/57935181/markdown-anchor-link-with-same-name-but-different-sections)
* [mdBook Documentation](https://rust-lang.github.io/mdBook/index.html)
