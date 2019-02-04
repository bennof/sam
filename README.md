# SAM - Simplified Augmented Markdown 

## Why?
This is the most important question to answer. Why do we need another another simplified markup language. The reason was due to interoperability of different systems. I started working with HTML and TeX but never found a way to convert text from one format to the other. 

About 10 years ago I started writing a JavaScript implementation of TeX. After implementing a fixed subset for typesetting, it got complicated. In the same time I started to write a TeX system for education (based on plain TeX) because I was not satisfied with the output of Word, etc.. This made development of the JavaScript implementation more complicated and more and more I had to handle style output to CSS. The way TeX and CSS handle style are different, especially due to the fact TeX is a complete programming language.

Next Idea was to write a output driver for pdfTeX/TeX which is like shooting with cannons on pigeons (German saying). The most converters like tex2html use a direct translation system. This will cause problems, if you use non default (what ever this means) commands in TeX. 

Troff is another story ...

So what I will try to do is to build a simple but powerful and dynamic markdown language, using traditional techniques. 
* BNF Grammar (for Yacc-like) for easy implementation in other languages
* clear syntax for plain text to read and to parse
* UTF-8 and URIs
* flow or stream based behavior on data includes 
* support for TeX math
* JSON data support
* output to HTML and TeX (PS, PDF, ...)
* should produce acceptable Markdown output (like this file)
* output presentations

## Elements
The plain text and readability will be improved by using mark down syntax:
*Examples are taken from [GitHub: Mastering Markdown](https://guides.github.com/features/mastering-markdown/)*

### Header 
```
# This is an h1 tag
## This is an h2 tag
###### This is an h6 tag

```

### Emphasis
```
*This text will be italic*

**This text will be bold**
```

### Lists

#### Unordered
```
* Item 1
* Item 2
  * Item 2a
  * Item 2b
```

#### Ordered
```
1. Item 1
1. Item 2
1. Item 3
   1. Item 3a
   1. Item 3b
```

### Block quotes
```
> We're living the future so
> the present is our past.

```

### Code
```
I think you should use an
`<addr>` element here instead.
```

or blocks embedded in ` ``` ` elements.

### Syntax highlighting

> Q: Is there syntax highlighting?  
> A: No. The creator (and) main users of Acme find syntax highlighting unhelpful and distracting.  
> [Rob Pike](http://acme.cat-v.org/faq)

### Tables (not working)
|First Header | Second Header
|------------ | -------------
|Content from cell 1 | Content from cell 2
|Content in the first column | Content in the second column

```GFM
First Header | Second Header
------------ | -------------
Content from cell 1 | Content from cell 2
Content in the first column | Content in the second column
```

### Comments
Comments will be enabled by `%` like TeX or Erlang.

### Escape Char
A escape char is needed to express special symbols/characters: `\c`.


### Math
I think a TeX/Latex like syntax works best, so Mathjs will be used with HTML output:
```
The Gamma function is defined as ${\displaystyle \Gamma (z)=\int _{0}^{\infty }x^{z-1}e^{-x}\,dx}$.
```
The Gamma function is defined as ${\displaystyle \Gamma (z)=\int _{0}^{\infty }x^{z-1}e^{-x}\,dx}$.
or block syntax: 
```
Schrödinger's equation:
$$\psi ({\mathbf  {r}},t)=A\;\exp \left(-{\frac  {{\mathrm  {i}}}{\hbar }}\;(Et-{\mathbf  {p}}\cdot {\mathbf  {r}})\right)$$
```

### Dynamics
missing


## BNF

The language is right now implemented in Erlang with leex (flex inspired) lexer and a top-down parser with a look-ahead of 1. To introduce dynamics intermediate tapes are introduced. 
