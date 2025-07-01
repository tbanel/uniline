---
title: Unicode Sketches Demo
---

<link rel="stylesheet" href="/assets/css/style.css">

# Unicode Sketches

This page demonstrates custom line-height for code/pre sections.

## Example

```text
  ╭───╮
  │███│
  ╰───╯
```

> The Unicode box drawing above should appear more compact if the stylesheet is active.

---

**How to use this page:**
- This page uses a custom CSS file at `/assets/css/style.css` to set `line-height: 1.0` for `pre` sections.
- You can edit this file to adjust styling as desired.
- To view this page styled, visit your GitHub Pages site (see instructions below).

---

## How to view this page

1. **Enable GitHub Pages** in your repository:
    - Go to your repository on GitHub.
    - Click on `Settings` → `Pages`.
    - As the source, select the branch you want (usually `main`) and the folder (`/root` or `/docs`) where `index.md` is located.
    - Save.
2. **Visit your site:**  
   Your Page will be at:  
   `https://tbanel.github.io/uniline/`





# Table of Contents

1.  [Getting started in 10 seconds](#getting-started-in-10-seconds)
2.  [New](#orgd832c58)
3.  [Table of Contents](#table-of-contents)
4.  [Pure UNICODE text diagrams in Emacs](#pure-unicode-text-diagrams-in-emacs)
5.  [Minor mode](#minor-mode)
6.  [Drawing lines](#drawing-lines)
7.  [Brush style](#brush-style)
8.  [The `<insert>` key](#the-insert-key)
9.  [Arrows glyphs `▷ ▶ → ▹ ▸ ↔`](#arrows-glyphs------)
10. [Intersection glyphs `■ ◆ ●`](#intersection-glyphs---)
11. [Drawing rectangles](#drawing-rectangles)
12. [Moving rectangles](#moving-rectangles)
13. [Copying, killing, yanking rectangles](#copying-killing-yanking-rectangles)
14. [Tracing a contour](#tracing-a-contour)
15. [Flood-fill](#flood-fill)
16. [Text direction](#text-direction)
17. [Macros](#macros)
18. [Fine tweaking](#fine-tweaking)
19. [Dashed lines and other styles](#dashed-lines-and-other-styles)
20. [ASCII to UNICODE](#ascii-to-unicode)
21. [Which fonts?](#which-fonts)
22. [Hydra or Transient?](#hydra-or-transient)
    1.  [The Hydra interface](#the-hydra-interface)
    2.  [The Transient interface](#the-transient-interface)
23. [Line spacing](#line-spacing)
24. [Customization](#customization)
25. [How `Uniline` behaves with its environment?](#how-uniline-behaves-with-its-environment)
    1.  [Compatibility with Picture-mode](#compatibility-with-picture-mode)
    2.  [Compatibility with Artist-mode](#compatibility-with-artist-mode)
    3.  [Compatibility with Whitespace-mode](#compatibility-with-whitespace-mode)
    4.  [Compatibility with Org Mode](#compatibility-with-org-mode)
    5.  [Org Mode and LaTex](#org-mode-and-latex)
    6.  [What about `\t` tabs?](#what-about-t-tabs)
    7.  [What about `^L` page separation?](#what-about-l-page-separation)
    8.  [Emacs on the Linux console](#emacs-on-the-linux-console)
    9.  [Emacs on a graphical terminal emulator](#emacs-on-a-graphical-terminal-emulator)
    10. [Emacs on Windows](#emacs-on-windows)
26. [Lisp API](#lisp-api)
27. [Installation](#installation)
28. [Related packages](#related-packages)
29. [Author, contributors](#author-contributors)
30. [License](#license)



<a id="getting-started-in-10-seconds"></a>

# Getting started in 10 seconds

-   Type `M-x uniline-mode`
-   Move cursor with the arrow-keys on the keyboard `→ ← ↑ ↓`
-   Quit `C-c C-c`


<a id="orgd832c58"></a>

# New

The `uniline-hint-style` customizable setting now works on Transient
menus (it used to be Hydra-only). To achieve that, it modifies
`transient-show-popup`. It does so only in the Uniline buffers.

Old news:

Compatibility break.

To choose between `Hydra` or `Transient` user interface, there was a
pre-installation setting: `uniline-interface-type`.

Now `Uniline` has 3 sub packages: `hydra`, `transient`, `core`.  Choosing the
interface is a matter of loading one or the other sub-package.

Example of a possible configuration in your `./emacs`:

    (use-package uniline-hydra
      :bind ("C-<insert>" . uniline-mode))

or:

    (use-package uniline-transient
      :bind ("C-<insert>" . uniline-mode))

See the "Installation" chapter for details.


<a id="table-of-contents"></a>

# Table of Contents

-   [Getting started in 10 seconds](#getting-started-in-10-seconds)
-   [Pure UNICODE text diagrams in Emacs](#pure-unicode-text-diagrams-in-emacs)
-   [Minor mode](#minor-mode)
-   [Drawing lines](#drawing-lines)
-   [Brush style](#brush-style)
-   [The <insert> key](#the-insert-key)
-   [Arrows glyphs ▷ ▶ → ▹ ▸ ↔](#arrows-glyphs------)
-   [Intersection glyphs ■ ◆ ●](#intersection-glyphs---)
-   [Drawing rectangles](#drawing-rectangles)
-   [Moving rectangles](#moving-rectangles)
-   [Copying, killing, yanking rectangles](#copying-killing-yanking-rectangles)
-   [Tracing a contour](#tracing-a-contour)
-   [Flood-fill](#flood-fill)
-   [Text direction](#text-direction)
-   [Macros](#macros)
-   [Fine tweaking](#fine-tweaking)
-   [Dashed lines and other styles](#dashed-lines-and-other-styles)
-   [ASCII to UNICODE](#ascii-to-unicode)
-   [Which fonts?](#which-fonts)
-   [Hydra or Transient?](#hydra-or-transient)
    -   [The Hydra interface](#the-hydra-interface)
    -   [The Transient interface](#the-transient-interface)
-   [Line spacing](#line-spacing)
-   [Customization](#customization)
-   [How Uniline behaves with its environment?](#how-uniline-behaves-with-its-environment)
    -   [Compatibility with Picture-mode](#compatibility-with-picture-mode)
    -   [Compatibility with Artist-mode](#compatibility-with-artist-mode)
    -   [Compatibility with Whitespace-mode](#compatibility-with-whitespace-mode)
    -   [Compatibility with Org Mode](#compatibility-with-org-mode)
    -   [Org Mode and LaTex](#org-mode-and-latex)
    -   [What about \t tabs?](#what-about-t-tabs)
    -   [What about ^L page separation?](#what-about-l-page-separation)
    -   [Emacs on the Linux console](#emacs-on-the-linux-console)
    -   [Emacs on a graphical terminal emulator](#emacs-on-a-graphical-terminal-emulator)
    -   [Emacs on Windows](#emacs-on-windows)
-   [Lisp API](#lisp-api)
-   [Installation](#installation)
-   [Related packages](#related-packages)
-   [Author, contributors](#author-contributors)
-   [License](#license)


<a id="pure-unicode-text-diagrams-in-emacs"></a>

# Pure UNICODE text diagrams in Emacs

Draw diagrams like those:

Document a command:

![img](images/document-command.png)

       pdfjam source.pdf 3-5,9
            ▲    ▲        ▲  ▲
    command╶╯    │        │  │
    input file╶──╯        │  │
    select pages 3,4,5╶───╯  │
    and page 9╶──────────────╯

Connect boxes with arrows:

![img](images/boxes-arrows.png)

                ╭───────────────────────╮
      ╷123╭────▶┤ hundred and something │
      ╰───╯     ╰───────────────────────╯
                                 ╭────▶──╮A╷
        ╭───╮    ┏━━━┓    ╔═══╗  │       ╰─╯
    0╶─→┤ 1 ┝━━━▶┫ 2 ┣═══▷╣ 3 ╟──●────▶──╮B╷
        ╰───╯    ┗━┯━┛    ╚═╤═╝  │       ╰─╯
                   ╰────←───╯    ╰────▶──╮C╷
                                         ╰─╯
       ╔══════════╗
       ║ 1        ║          ▐▀▀▀▀▀▀▀▀▜
       ║    ╭─────╫───╮ ◁──▷ ▐ 3      ▐
       ╚════╪═════╝ 2 │      ▐▄▄▄▄▄▄▄▄▟
            ╰─────────╯

Explain decisions trees:

![img](images/decision-tree.png)

    ┏━━━━━━━━━━━━┓
    ┃which color?┃
    ┗━┯━━━━━━━━━━┛
      │     ╭──────╮
      │  ╭──┤yellow├─▷╮good─choice╭□
      ▽  │  ╰──────╯  ╰═══════════╯
      ╰──●  ╭───╮    ┏━━━━━┓
         ├──┤red├───▷┨dark?┠──╮
         │  ╰───╯    ┗━━━━━┛  │
         │ ╭───◁──────────────╯
         │ │   ╭───╮
         │ ╰─●─┤yes├▷╮regular─red╭─□
         │   │ ╰───╯ ╰═══════════╯
         │   │ ╭──╮
         │   ╰─┤no├─▷╮pink╭────────□
         │     ╰──╯  ╰════╯
         │  ╭────╮
         ├──┤blue├───▷╮next week╭──□
         │  ╰────╯    ╰═════════╯
         │  ╭─────╮
         ╰──┤white├──▷╮available╭──□
            ╰─────╯   ╰═════════╯

Draw lines or blocks:

![img](images/lines-blocks.png)

                                  ╭─╮←─╮
                             ╭╮   │ │  ╰──╴max 235
                           ╭╮││  ╭╯ │
                           │╰╯│╭─╯  │
          ╭╮               │  ││    │
       ╭─╮││╭╮   ╭──╮╭╮    │  ╰╯    ╰╮
      ╭╯ ╰╯╰╯│  ╭╯  ╰╯╰─╮  │         │ ╭╮
    ◁─╯      ╰──╯       ╰──╯         ╰─╯╰────▷
    ◀════════════════════════════════════════▶
                           ╭────────╮
       ▲                   │all time│
       ┃       ▄     ▗▟█ ←─┤highest │
      Qdx      █▌   ████   ╰────────╯
       ┃     ▗▄█▌   █████▙
       ┃   ▟███████▄█████████▄▄▄     ▗▄
       ┃▐▄▄████████████████████████████▄▄▖
        ╺━━━━━━━━━━╸time╺━━━━━━━━━━━━━━━━▶

Outline the General Relativity equation:

![img](images/general-relativity-equation.png)

    
         ╭─────────────────────╴G: Einstein tensor
         │                ╭────╴κ: Gravitational coupling constant
      ╭──▽───╮        ╭───▽──╮
    ┏━┷━━━━━━┷━━━━━━━━┷━━━━━━┷━━━┓
    ┃ R - gR/2 + Λg = (8πG/c⁴)×T ┃◁╴General Relativity equation
    ┗━△━━━△△━━━━━△△━━━━━━△━△━━━△━┛
      │   ││     ││      │ │  ╭╯
      │   ││     ││      │ │  ╰╴Energy-impulsion tensor
      │   ││     ││      │ ╰───╴Speed of light
      │   ││     ││      ╰─────╴Gravitational constant
      │   ││     ╰┴────────────╴Cosmological constant
      │   │╰──────┴────────────╴Scalar curvature
      │   ╰───────╰────────────╴Metric tensor
      ╰────────────────────────╴Ricci tensor

Outline the Schrödinger equation:

![img](images/schrodinger-equation.png)

    
           ╭─────────────────────╴Derivative over time
           │     ╭──────────╭────╴State of quantum system at time t
           │     │          │     (the square of its absolute value
          ╭▽─╮ ╭─▽──╮     ╭─▽──╮   is the probability density)
    ┏━━━━━┷━━┷━┷━━━━┷━━━━━┷━━━━┷━┓
    ┃ i ħ d/dt |Ψ(t)> = Ĥ |Ψ(t)> ┃◁─╴Schrödinger equation
    ┗━△━△━━━━△━━━━△━━━━━△━━━━△━━━┛
      │ │    ╰────╰─────┤────╰───╴Time
      │ │               ╰────────╴Hamiltonian
      │ ╰────────────────────────╴Reduced Plank constant
      ╰──────────────────────────╴Imaginary number i²=-1

Explain the structure of a sentence in a foreign language (which one?):

![img](images/foreign-language-sentence.png)

    
    ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    ┃ the pretty table is standing ┃
    ┗┯━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
     │    ╭────┬─────┬─────╴radicals
     ↕   ╭┴╮  ╭┴─╮  ╭┴─╮
    ┏┷━━━┿━┿━━┿━━┿━━┿━━┿━━━┓
    ┃ la bela tablo staras ┃
    ┗━━━━┿━┿△━┿━━┿△━┿━━┿△━━┛
         ╰─╯│ ╰──╯│ ╰──╯│  ┏━━━━━suffixes━━━━━┓
            │     │     ╰──╂╴as: present tense┃
            │     │        ┃ os: future tense ┃
            │     │        ┃ is: past tense   ┃
            │     ╰────────╂╴ o: noun         ┃
            ╰──────────────╂╴ a: adjective    ┃
                           ┃  e: adverb       ┃
                           ┗━━━━━━━━━━━━━━━━━━┛

Explain Lisp lists:

![img](images/lisp-lists.png)

      '(a b c)
         ┏━━━┳━━━┓   ┏━━━┳━━━┓   ┏━━━┳━━━┓
    ●━━━▶┫ ● ┃ ●─╂──▷┨ ● ┃ ●─╂──▷┨ ● ┃nil┃
         ┗━┿━┻━━━┛   ┗━┿━┻━━━┛   ┗━┿━┻━━━┛
           │           ╰──────────╮╰╮
           │  ╭─────┬───────────╮ │ │
           ╰─▷┤"a\0"│properties │ │ │
              ├─────┼───────────┤ │ │
              │"b\0"│properties ├◁╯ │
              ├─────┼───────────┤   │
              │"c\0"│properties ├◁──╯
              ├─────┼───────────┤
              │...  │...        │
              ╵     ╵           ╵

Draw sketched objects:

![img](images/sketched-objects.png)

    
     ◀─(-)────────(+)──▶    ~╭──────╮~
      ▗──────────────╮     ~~│ ╭~~╮ │~~
      ▐              ╰╮     ~│ ╵  ╵ │~
    ╭□▐   1.5 volts  ╭╯□╮    ╰─╖  ╓─╯
    │ ▝▀▀▀▀▀▀▀▀▀▀▀▀▀▀▘  │      ╠━━╣
    │                   ╰──────╯  │
    ╰─────────────────────────────╯

![img](images/water-sketch.png)

     ╶╮       ╭╴
    ┏┳┥▒▒▒▒▒▒▒┝╸
    ┃┃│▒▒eau▒▒│
    ┃┃│▒▒▒▒▒▒▒│ ╔═════╗
    ┃┃╰──╮▒╭──╯ ║ ╶╮  ▽           ╭╴
    ┃┃    ▒     ║  │  ░           │
    ┃┃    ▒     ║  │░░░░░░░░░░░░░░│
    ┃┃    ╚═════╝  │░░░░░░░░░░░░░░╞════▷▒▒
    ┃┃             │░░░░░akvo░░░░░│    ╶╮ ▒         ╭╴
    ┃┃             │░░░░░░░░░░░░░░│     │  ▒        │
    ┃┃             ╰─┲┳━━━━━━━━┳┱─╯     │▒▒▒▒▒▒▒▒▒▒▒│
    ┃┃               ┃┃        ┃┃       │▒▒▒water▒▒▒│
    ┃┃               ┃┃        ┃┃       │▒▒▒▒▒▒▒▒▒▒▒│
    ┃┃               ┃┃        ┃┃       ╰───────────╯
    ▝▀▀▀▀▀▀▘        ▝▀▘        ▝▀▘      ▀▀▀▀▀▀▀▀▀▀▀▀▀

Those diagrams are pure text. There is nothing graphic. They are
achieved using UNICODE characters. Most often, the text file will be
encoded as UTF-8.

Creating such diagrams by hand is painfully slow. Use `Uniline` to
draw lines while you move the cursor with keyboard arrows.

**Beware!**

If you see those diagrams miss-aligned, most likely the font used to
display them does not support UNICODE block characters. See bellow the
paragraph "Which fonts?".


<a id="minor-mode"></a>

# Minor mode

`Uniline` is a minor mode. Activate it temporarily:

`M-x uniline-mode`

Exit it with:

`C-c C-c`

The current major mode is still active underneath `uniline-mode`.

While in `uniline-mode`, overwriting is active, as well as long lines
truncation. Also, a hollow cursor is provided. Those settings are
reset to their previous state when exiting `uniline-mode`.


<a id="drawing-lines"></a>

# Drawing lines

Use keyboard arrows to draw lines.

By default, drawing lines only happens over empty space or over other
lines. If there is already text, it will not be erased. However, by
hitting the control-key while moving, lines overwrite whatever there
is.

The buffer is "infinite" in bottom and right directions. Which means
that when the cursor ends up outside the buffer, white space
characters are automatically added.

The usual numeric prefix is available. For instance, to draw a line 12
characters wide downward, type: `M-12 <down>`


<a id="brush-style"></a>

# Brush style

Set the current brush with:

-   `-` single thin line
    `╭─┬─╮`

-   `+` single thick line
    `┏━┳━┓`

-   `=` double line
    `╔═╦═╗`

-   `#` quarter block
    `▙▄▟▀`

-   `<delete>` eraser

-   `<return>` move without drawing anything

The current brush and the current text direction (see below) are
reflected in the mode-line (at the bottom of the Emacs screen). It
looks like this:

![img](images/mode-line.png)

    
     current text                  current
        direction╶────╮       ╭───╴brush
                      ▼       ▼
    ══════════════════╧═══════╧══════════════
    U:** buff    (... →Uniline┼ ...)
    ═════════════════════════════════════════


<a id="the-insert-key"></a>

# The `<insert>` key

The `<insert>` key is a prefix for other keys:

-   for drawing arrows, squares, crosses, o-shapes glyphs,
-   for handling rectangles,
-   for inserting `# = - +` which otherwise change the brush style,
-   for trying a choice of mono-spaced fonts.

Why `<insert>`? Because:

-   `Uniline` tries to leave their original meaning to as many keys as
    possible,
-   the standard meaning of `<insert>` is to toggle the `overwrite-mode`;
    but `Uniline` is already in `overwrite-mode`, and de-activating
    overwrite would break `Uniline`.

So preempting `<insert>` does not sacrifices anything.


<a id="arrows-glyphs------"></a>

# Arrows glyphs `▷ ▶ → ▹ ▸ ↔`

At any time, an arrow may be drawn. The arrow points in the direction
that the line drawing follows.

`Uniline` supports 6 arrows types: `▷ ▶ → ▹ ▸ ↔`

![img](images/arrow-styles.png)

    
    □
    ╰─◁──▷─╮       □─╮ ╭─╮ ╭─╮ ╭─□
    ╭─◀──▶─╯         △ ▲ ↑ ▵ ▴ ↕
    ╰─←──→─╮         │ │ │ │ │ │
    ╭─◃──▹─╯         ▽ ▼ ↓ ▿ ▾ ↕
    ╰─◂──▸─╮         ╰─╯ ╰─╯ ╰─╯
    ╭─↔──↔─╯
    □

Actually, there are tons of arrows of all styles in the UNICODE
standard. Unfortunately, support by fonts is weak. So `Uniline`
restrains itself to those six safe arrows.

To insert an arrow, type: `<insert> a` or `<insert> a a` or `<insert> a a a`. (`a`
cycles through the 6 styles, `A` cycles backward).

`<insert> 4 a` is equivalent to `<insert> a a a a`, which is also equivalent to
`<insert> A A A`. Those 3 shortcuts insert an arrow of this style: `▵▹▿◃`. The
actual direction where the arrow points follows the last movement of
the cursor.

To change the direction of the arrow, use shift-arrow, for example:
`S-<up>` will change from `→` to `↑`.


<a id="intersection-glyphs---"></a>

# Intersection glyphs `■ ◆ ●`

There are a few other UNICODE characters which are mono-space and
symmetric in the 4 directions. They are great at line intersections:

To insert a square `□ ■ ▫ ▪ ◆ ◊` type:
`<insert> s s s...` (`s` cycles, `S` cycles backward).

To insert a circular shape `· ∙ • ● ◦ Ø ø` type:
`<insert> o o o...` (`o` cycles, `O` cycles backward).

To insert a cross shape `╳ ÷ × ± ¤` type:
`<insert> x x x...` (`x` cycles, `X` cycles backward).

To insert a usual ASCII letter or symbol, just type it.

As the keys `- + = #` are preempted by `Uniline` mode, to type them,
prefix them with `<insert>`. Example: `<insert> -` inserts a `-` and
`<insert> +` inserts a `+`.

![img](images/insert-glyphs.png)

    
    <insert>
        │
        ▼
       ╭┴╮   ╭───────╮  ╭──────────────────╮
       │s├─▶─┤squares├──┤ □  ■  ▫  ▪  ◆  ◊ │
       ╰┬╯   ╰───────╯  ╰──────────────────╯
       ╭┴╮   ╭───────╮  ╭─────────────────────╮
       │o├─▶─┼circles┼──┤ ·  ∙  •  ●  ◦  Ø  ø │
       ╰┬╯   ╰───────╯  ╰─────────────────────╯
       ╭┴╮   ╭───────╮  ╭───────────────╮
       │x├─▶─┼crosses┼──┤ ╳  ÷  ×  ±  ¤ │
       ╰┬╯   ╰───────╯  ╰───────────────╯
       ╭┴╮              ╭───╮
       │+├─▶────────────┤ + │
       ╰┬╯              ╰───╯
       ╭┴╮              ╭───╮
       │-├─▶────────────┤ - │
       ╰┬╯              ╰───╯
       ╭┴╮              ╭───╮
       │=├─▶────────────┤ = │
       ╰┬╯              ╰───╯
       ╭┴╮              ╭───╮
       │#├─▶────────────┤ # │
       ╰─╯              ╰───╯


<a id="drawing-rectangles"></a>

# Drawing rectangles

To draw a rectangle in one shot, select a rectangular region with
`C-SPC` or `C-x SPC` and move the cursor.

You may also use `S-<arrow>` (`<arrow>` being any of the 4
directions) to extend the selection. The buffer grows as needed with
white spaces to accommodate the selection. Selection extension mode is
active when `shift-select-mode` is non-nil.

If needed, change the brush with any of
 `- + = # <delete>`

then hit

-   `r` to draw a rectangle inside the selection
-   `S-R` to draw a rectangle outside the selection
-   `C-r` to overwrite a rectangle inside the selection
-   `C-S-R` to overwrite a rectangle outside the selection

![img](images/draw-rectangle.png)

    ╭───────╮          r: inside╮╭───────╮
    │ one   │          ▗▄▄▄▄▄▄▖╭┤│▛▀▀▀▀▀▜│
    │  ┏━━━━┿━━━━━━┓   ▐╭────╮▌│╰┼▌     ▐│
    ╰──╂────╯ two  ┃   ▐│    │▌│ │▙▄▄▄▄▄▟│
       ┃   ╔═══════╋═╗ ▐│    ├▌╯ ╰─────┬─╯
       ┗━━━╋━━━━━━━┛ ║ ▐╰────╯▌────────┴───╮
           ║  three  ║ ▝▀▀▀▀▀▀▘  R: outside╯
           ╚═════════╝
    
                           ╭─────────╮
    my text I              │my text I│
    want to  ╶─<insert>R─▷ │want to  │
    box                    │box      │
                           ╰─────────╯

The usual `C-_` or `C-/` keys may be hit to undo, even with the region still
active visually.


<a id="moving-rectangles"></a>

# Moving rectangles

Select a region, then press `<insert>`. The selection becomes rectangular if it
was not.

Use arrow keys to move the rectangle around. A numeric prefix may be
used to move the rectangle that many characters. Be sure to specify
the numeric prefix with just digits, without the `Alt` key. Typing
`15 <left>` moves the rectangle 15 characters to the left. `M-15 <left>`
does not work.

Press `q`, `<return>`, or `C-g` to stop moving the rectangle.

The `C-_` key may also be used to undo the previous movements, even
though the selection is still active.

![img](images/move-rectangle.png)

                    ▲
                    │
                   <up>
              ╭─────┴──────╮
              │this is     │
              │my rectangle│
    ◀─<left>──┤I want to   ├─<right>─▶
              │move        │
              ╰─────┬──────╯
                  <down>
                    │
                    ▼


<a id="copying-killing-yanking-rectangles"></a>

# Copying, killing, yanking rectangles

A rectangle can be copied or killed, then yanked somewhere else. Press:

-   `c` to copy
-   `k` to kill
-   `y` to yank (aka paste)

This is similar to the Emacs standard rectangle handling:

-   `C-x r r` copy rectangle to register
-   `C-x r k` kill rectangle
-   `C-x r y` yank killed rectangle

The first difference is that `Uniline` rectangles when killed and
yanked, do not move surrounding characters.

The second difference is that the white characters of the yanked
rectangle are considered transparent. The result is that only
non-blank parts of the yanked rectangle are over-printed.

`Uniline` and Emacs standard rectangle share the same storage for copied
and killed rectangles, `killed-rectangle`. So, a rectangle can be killed
one way, and yanked another way.


<a id="tracing-a-contour"></a>

# Tracing a contour

![img](images/contour-tracing.png)

      ╭──────────────╮
    ╭─╯A.written.text╰────────╮
    │outlined by the.`contour'│
    ╰─╮function.gets╶┬────────╯
      ╰╮a.surrounding╰───────╮
       ╰─╮line.in.the.current│
         ╰─╮brush.style╭─────╯
           ╰───────────╯

Choose or change the brush style with any of `-,+,=_,#,<delete>`. Put
the cursor anywhere on the shape or outside but touching it. Then
type:

`<insert> c`

A contour line is traced (or erased if brush style is `<delete>`)
around the contiguous shape close to the cursor.

When hitting capital letter: `<insert> C` the contour is
overwritten. This means that if there was already a different style of
line on the contour path, it is overwritten.

The shape is distinguished because it floats in a blank characters
ocean. For the shake of the contour function, blank characters are
those containing lines as drawn by `Uniline` (including true blank
characters). Locations outside the buffer are also considered blank.

The algorithm has an upper limit of 10000 steps. This avoids an
infinite loop in which the algorithm may end up in some rare
cases. One of those cases is when the contour crosses a new-page
character, displayed by Emacs as `^L`. 10000 steps require a fraction of
a second to run. For shapes really huge, you may launch the contour
command once again, at the point where the previous run ended.


<a id="flood-fill"></a>

# Flood-fill

![img](images/flood-fill.png)

    
    this.text.surrounds      this.text.surrounds
    .                 /      .▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒/
    .                //╶───▷╴.▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒//
    ...            ////      ...▒▒▒▒▒▒▒▒▒▒▒▒////
      ...a.hole/////           ...a.hole/////

A hollow shape is a contiguous region of identical characters (not
necessarily blank), surrounded by a boundary of different
characters. The end of the buffer in any direction is also considered
a boundary.

Put the cursor anywhere in the hole. Then type:

`<insert> i`

Answer by giving a character to fill the hole.

If instead of a character, `SPC` or `DEL` is typed, then a shade of grey
character is picked. `SPC` selects a darker grey than the one the point
is on, while `DEL` selects a lighter. There are 5 shades of grey in the
UNICODE standard: `" ░▒▓█"`.  Those grey characters are well supported
by the suggested fonts.

`C-y` is also an option. The first character in the top of the kill
ring will be chosen as the filling character. The kill ring is filled
by functions like `C-k` or `M-w`.

Typing `<return>` or `C-g` aborts the filling operation.

A rectangular shape may also be filled.

-   Mark a region
-   `<insert> i`
-   answer which character should be used to fill.

There is no limit on the area to fill. Therefore, the filling
operation may flood the entire buffer (but no more).


<a id="text-direction"></a>

# Text direction

Usually, inserting text in a buffer moves the cursor to the right. (And
sometimes to the left for some locales). Any of the 4 directions can be
selected under `Uniline`. Just type any of:

-   `<insert> C-<up>`
-   `<insert> C-<right>`
-   `<insert> C-<down>`
-   `<insert> C-<left>`

The current direction is reflected in the mode-line, just before the
word `"uniline"`.


<a id="macros"></a>

# Macros

`Uniline` adds directional macros to the Emacs standard macros.

Record a macro as usual with `C-x (` … `C-x )`.

Then call it with the usual `C-x e`. But then, instead of executing
the macro, a menu is offered to execute it in any of the 4 directions.

When a macro is executed in a direction other than the one it was
recorded, it is twisted in that direction. This means that recorded
hits on the 4 keyboard arrows are rotated. It happens also for shift
and control variations of those keys. Direction of text insertion is
also rotated.

There is still the classical `e` option to call the last recorded
macro. So instead of the usual `C-x e`, type `C-x e e`. And of course,
the usual repetition typing repeatedly `e` is available.

Why are directional macros useful? To create fancy lines. For
instance, if we want a doted line instead of the continuous one, we
record a macro for one step:

    C-x (             ;; begin recording
    INS o             ;; insert a small dot
    <right> <right>   ;; draw a line over 2 characters
    C-x )             ;; stop recording

Then we call this macro repeatedly in any of the 4 directions:

![img](images/macro-doted-line.png)

    
    ·─·─·─·─·  ╷     ·──·
            │  │     │  │
            ·  ·     ·  ·
            │  │     │  │
            ·  ·─·─·─·  ·
            │           │
            ·─·─·─·─·─·─·

We can draw complex shapes by just drawing one step. Hereafter, we
call a macro in 4 directions, closing a square:

![img](images/macro-fancy-squares.png)

    
      ╭╮╭╮╭╮╭╮╭╮╭╮     △ △ △ △ △ △       ╭─╮ ╭─╮ ╭─╮ ╭─╮     ╭─╮ ╭─╮ ╭─╮ ╭─╮
    ╭─╯╰╯╰╯╰╯╰╯╰╯│    ╶╯╶╯╶╯╶╯╶╯╶╯╷   ╭──╯∙╰─╯∙╰─╯∙╰─╯∙│    ▷┤□├▷┤□├▷┤□├▷┤□├▽
    ╰╮           ╰╮  ◁╮           ╰▷  │∙               │   ╭┴┼─╯ ╰─╯ ╰─╯ ╰─┼┴╮
    ╭╯           ╭╯   ╵           ╷   ╰╮               ╰╮  │□│             │□│
    ╰╮           ╰╮  ◁╮           ╰▷   │               ∙│  ╰┬╯             ╰┬╯
    ╭╯           ╭╯   ╵           ╷   ╭╯               ╭╯   △               ▽
    ╰╮           ╰╮  ◁╮           ╰▷  │∙               │   ╭┴╮             ╭┴╮
    ╭╯           ╭╯   ╵           ╷   ╰╮               ╰╮  │□│             │□│
    ╰╮           ╰╮  ◁╮           ╰▷   │               ∙│  ╰┬┼─╮ ╭─╮ ╭─╮ ╭─┼┬╯
     │╭╮╭╮╭╮╭╮╭╮╭─╯   ╵╭╴╭╴╭╴╭╴╭╴╭╴    │∙╭─╮∙╭─╮∙╭─╮∙╭──╯   △┤□├◁┤□├◁┤□├◁┤□├◁
     ╰╯╰╯╰╯╰╯╰╯╰╯      ▽ ▽ ▽ ▽ ▽ ▽     ╰─╯ ╰─╯ ╰─╯ ╰─╯       ╰─╯ ╰─╯ ╰─╯ ╰─╯


<a id="fine-tweaking"></a>

# Fine tweaking

![img](images/fine-tweaking.png)

    
     convert this  ═══▶   into that
    ╭───────────╮        ╭───────────╮
    │╶───┬────▷ │        │╶───╮────▷ │
    │    │      │        │    │      │
    │           │        │           │
    │    ▀▀▀    │        │    ▀▟▀    │
    ╰───────────╯        ╰───────────╯

At the crossing of lines, it may be appealing to do small
adjustments. In the above example, we removed a segment of line which
occupies 1/4 of a character. This cannot be achieve with line tracing
alone. We also modified a quarter-block line in a non-obvious way.

-   Put the point (the cursor) on the character where lines cross each other.
-   type `INS S-<right> S-<right>`

`<right>` here refers to the right part of the character under the
point. The 1/4 line segment will cycle through all displayable
forms. On the second stroke, no segment will be displayed, which is
what we want.

Caveat! The UNICODE standard does not define all possible combinations
including double line segments. (It does for all combinations of thin
and tick lines). So sometimes, when working with double lines, the
process may be frustrating.

This works also for lines made of quarter-blocks. There are 4
quarter-blocks in a character, either on or off. Each of the 4 shifted
keyboard arrows flips a quarter-block on-and-off.

In the above example, the effect was achieved with:
`INS S-<up> S-<down> S-<left>`


<a id="dashed-lines-and-other-styles"></a>

# Dashed lines and other styles

![img](images/four-styles.png)

    
    ╭────▷───╮   ┏━━━━▶━━━┓   ╔════▶═══╗
    │ ╭─□──╮ │   ┃ ┏━■━━┓ ┃   ║ ╔═■══╗ ║
    △ │    │ ▽   ▲ ┃    ┃ ▼   ▲ ║    ║ ▼
    │ ╰───◦╯ │   ┃ ┗━━━•┛ ┃   ║ ╚═══•╝ ║
    ╰───◁────╯   ┗━━━◀━━━━┛   ╚═══◀════╝
    
    ╭╌╌╌╌▷╌╌╌╮   ┏╍╍╍╍▶╍╍╍┓
    ┆ ╭╌□╌╌╮ ┆   ┇ ┏╍■╍╍┓ ┇
    △ ┆    ┆ ▽   ▲ ┇    ┇ ▼
    ┆ ╰╌╌╌◦╯ ┆   ┇ ┗╍╍╍•┛ ┇
    ╰╌╌╌◁╌╌╌╌╯   ┗╍╍╍◀╍╍╍╍┛
    
    ╭┈┈┈┈▷┈┈┈╮   ┏┉┉┉┉▶┉┉┉┓
    ┊ ╭┈□┈┈╮ ┊   ┋ ┏┉■┉┉┓ ┋
    △ ┊    ┊ ▽   ▲ ┋    ┋ ▼
    ┊ ╰┈┈┈◦╯ ┊   ┋ ┗┉┉┉•┛ ┋
    ╰┈┈┈◁┈┈┈┈╯   ┗┉┉┉◀┉┉┉┉┛

A base drawing can be converted to dashed lines. Moreover, lines can
be made either thin or thick.

-   Select the rectangular area you want to operate on (with mouse drag
    or `S-<left>`, `S-<down>` and so on).
-   Type `INS`, then `s` (as "style").

You will be offered a choice of styles:

-   `3`: vertical lines will become 3 dashes per character, while
    horizontal ones will get 2 dashes per character.
-   `4`: vertical and horizontal lines will get 4 dashes per character.
-   `h`: thin lines corners, which are usually rounded, become hard angles.
-   `+`: thin lines corners and intersections become thick, empty glyphs
    get filled.
-   `-`: thick lines corners and intersections become thin, filled glyphs
    are emptied.
-   `=`: thick and thin lines become double lines.
-   `0`: come back to standard base-line `Uniline` style: plain not-dashed
    lines, thin corner rounded, ASCII art is converted to UNICODE.
-   `a`: apply the `aa2u-rectangle` function from the unrelated
    `ascii-art-to-unicode` package, to convert ASCII art to UNICODE (this
    only works if `ascii-art-to-unicode` is already installed)

Converting parts of a drawing from one style to another can produce
nice looking sketches.

![img](images/same-sketch-several-styles.png)

    
    ╭───╮   ╭───╮   ╭───╮
    │░░░│   │░░░│   │░░░┝━▶┓ ╭╌╌╌╌╌╮
    │░░░╰───╯░░░╰───╯░░░│  ┃ ┆░░░░░╰╌╌╌╌╌╮
    □░░░░░░░░░░░░░░░░░░░│  ┗━┥░░░░░░░░░░░┆
    │░░░╭───╮░░░╭───╮░░░│    ┆░░░░░╭╌╌╌╌╌╯
    ╰───╯   ╰─┰─╯   ╰─┰─╯    ╰╌╌┰╌╌╯
              ▲       ┃         ▼
              ┗━━━━━━━┻━━━━━━━━━┛
    
    ┏━━━┓   ┏━━━┓   ┏━━━┓
    ┃░░░┃   ┃░░░┃   ┃░░░┠─▷╮ ┏╍╍╍╍╍┓
    ┃░░░┗━━━┛░░░┗━━━┛░░░┃  │ ┇░░░░░┗╍╍╍╍╍┓
    ■░░░░░░░░░░░░░░░░░░░┃  ╰─┨░░░░░░░░░░░┇
    ┃░░░┏━━━┓░░░┏━━━┓░░░┃    ┇░░░░░┏╍╍╍╍╍┛
    ┗━━━┛   ┗━┯━┛   ┗━┯━┛    ┗╍╍┯╍╍┛
              △       │         ▽
              ╰───────┴─────────╯


<a id="ascii-to-unicode"></a>

# ASCII to UNICODE

The standard base-line `Uniline` (`INS s 0`) or `aa2u-rectangle` (`INS s a`)
conversions may be used to convert ASCII art to UNICODE. The original
ASCII art may be drawn for instance by the `artist-mode` or the
`picture-mode` packages.

To use `aa2u-rectangle`, install the `ascii-art-to-unicode` package by
Thien-Thi Nguyen (RIP), available on ELPA. `Uniline` does not requires a
dependency on this package, by lazy evaluating any call to
`aa2u-rectangle`.
See <https://elpa.gnu.org/packages/ascii-art-to-unicode.html>

![img](images/ascii-2-unicode.png)

    
    +-------------+    +--+
    |             +-->-|  +-----+   ASCII art
    | 1  +--------+--+ | 3      |   made by
    +----+--------+  | +----+---+   Artist-mode
         | 2         +-<----+
         +-----------+
    
    ╭─────────────╮    ╭──╮
    │             ├──▷─│  ╰─────╮   Converted to
    │ 1  ╭────────┼──╮ │ 3      │   Uniline base style
    ╰────┼────────╯  │ ╰────┬───╯   INS s 0
         │ 2         ├─◁────╯
         ╰───────────╯
    
    ┌─────────────┐    ┌──┐
    │             ├──>─│  └─────┐   Converted by
    │ 1  ┌────────┼──┐ │ 3      │   aa2u-rectangle
    └────┼────────┘  │ └────┬───┘   INS s a
         │ 2         ├─<────┘
         └───────────┘

`INS s 0` with selection active calls the `uniline-change-style-standard`
function. It converts what looks ASCII-art to UNICODE-art. Of course,
there are ambiguities regarding whether a character is part of a
sketch or not.

The heuristic is to consider that a character is part of a sketch if
it is surrounded by at least one other character which is part of a
sketch. So, an isolated `-` minus character will be left alone, while
two such characters `--` will be converted to UNICODE. Conversion will
happens also for `<-` for instance.

Here is a fairly convoluted ASCII-art example, along with its
conversion by `INS s 0`:

![img](images/ascii-2-unicode-b.png)

    
         ╭─↔--<-◁-◀--━+           +--->------==+
    /----/ Rectangle1 |-----+-----+ Rectangle2 v    v
    |    | <uni^code> ^     "     | "quote"    +-\  ▼
    ^^   \------------/   /-+-\   +------------+ "  v
    |    \--+------+--/   |   |   +----\----/--+ "  >▷▶>
    \>--\   |      |      \---/        |    |    "
        v   \==<===/   a=b 1=2 a-to-b  +----+ ◁==/  >->
    
         ╭─↔──◁─◁─◀──━┑           ╭───▷──────══╕
    ╭────┤ Rectangle1 │─────╥─────┤ Rectangle2 ▽    ▽
    │    │ <uni^code> △     ║     │ "quote"    ├─╖  ▼
    △^   ├────────────┤   ╭─╨─╮   ├────────────┤ ║  ▽
    │    ╰──┬──────┬──╯   │   │   ╰────┬────┬──╯ ║  ▷▷▶▷
    ╰▷──╮   │      │      ╰───╯        │    │    ║
        ▽   ╘══◁═══╛   a=b 1=2 a-to-b  ╰────╯ ◁══╝  ▷─▷


<a id="which-fonts"></a>

# Which fonts?

A mono-space character font must be used. It must also support UNICODE.

Not all fonts are born equal.

-   `(set-frame-font "DejaVu Sans Mono"        )`
-   `(set-frame-font "Unifont"                 )`
-   `(set-frame-font "Hack"                    )`
-   `(set-frame-font "JetBrains Mono"          )`
-   `(set-frame-font "Cascadia Mono"           )`
-   `(set-frame-font "Agave"                   )`
-   `(set-frame-font "JuliaMono"               )`
-   `(set-frame-font "FreeMono"                )`
-   `(set-frame-font "Iosevka Comfy Fixed"     )`
-   `(set-frame-font "Iosevka Comfy Wide Fixed")`
-   `(set-frame-font "Aporetic Sans Mono"      )`
-   `(set-frame-font "Aporetic Serif Mono"     )`
-   `(set-frame-font "Source Code Pro"         )`

Those fonts are known to support the required UNICODE characters, AND
display them as mono-space. There are fonts advertised as mono-space
which give arbitrary widths to non-ASCII characters. That is bad for
the kind of drawings done by `Uniline`.

You may want to try any of the suggested fonts. Just hit the
corresponding entry in the `Uniline` menu, or type `<insert> f`. You may
also execute the above Lisp commands like that:

`M-: (set-frame-font "DejaVu Sans Mono")`

This setting is for the current session only. If you want to make it
permanent, you may use the Emacs customization:

`<insert> f *`

or

`M-x customize-face default`

Beware that Emacs tries to compensate for missing UNICODE support by
the current font. Emacs substitutes one font for another, character
per character. The user may not notice until the drawings done under
Emacs are displayed on another text editor or on the Web.

To know which font Emacs has chosen for a given character, type:

`C-u C-x =`

Note that none of those commands downloads a font from the Web.
The font should already be available.


<a id="hydra-or-transient"></a>

# Hydra or Transient?

Casual usage of `Uniline` should be easy: just move the point, and lines
are traced.

More complex actions are summoned by the `<insert>` key, with or without
selection. This is a single key to remember. Then a textual menu is
displayed, giving the possible keys continuations and their
meaning. All that is achieved by the `Hydra` or `Transient` libraries,
which are now part of Emacs (thanks!).

The `Hydra` and `Transient` libraries offer similar features. Some users
may prefer one or the other.

`Uniline` was developed from day one with `Hydra`. `Transient` is a late
addition.

Two files are compiled when installing `Uniline`

-   `uniline-hydra.el`
-   `uniline-transient.el`

One of them should be loaded (but not both). There are several
ways. The cleanest is `use-package`. Add those lines to your `~/.emacs`
file:

    (use-package uniline-hydra
      :bind ("C-<insert>" . uniline-mode))

or:

    (use-package uniline-transient
      :bind ("C-<insert>" . uniline-mode))

Note: there used to be a customizable setting to switch between the
two interfaces. This had many issues. One of them is that the
native-compiler is blind to all user-customized settings.

Therefore, now `Uniline` ships with 3 Lisp source files (hydra,
transient, core). Loading `uniline-hydra.el` or `uniline-transient.el`
automatically loads `uniline-core.el`.


<a id="the-hydra-interface"></a>

## The Hydra interface

    (use-package uniline-hydra
      :bind ("C-<insert>" . uniline-mode))

Beware that the `Melpa` package no longer declares `Hydra` as a dependency
as it used to. Therefore, the `Hydra` package must be installed
separately prior to installing `Uniline`. This is for avoiding the
automatic and useless installation of `Hydra` when `Transient` is
chosen. (There is no way to make the dependencies conditional).

The multi-lines Hydra's menus are quite useful for casual users. For
seasoned users, those huge textual menus may distract them from
their workflow.

It is now possible to switch to less distracting textual menus. They
are displayed in the echo-area on a single line.

To do so, type:

-   `TAB` within a sub-mode (glyph insertion mode, rectangle handling,
    etc.)
-   `C-h TAB` at the top-level.

This will flip between the two sizes of textual menus. It also affects
the welcome message, the one displayed when entering the `Uniline` minor
mode.

The current size is controlled by the `uniline-hint-style` variable:

-   `t` for full fledged messages over several lines
-   `1` for one-liner messages
-   `0` for no message at all

The variable is "buffer-local", which means that it can take distinct
values on distinct buffers.

Its default value can be customized and save for future sessions:

`M-x customize-variable uniline-hint-style`

It can be changed later, on a buffer per buffer basis, with the `TAB`
key.


<a id="the-transient-interface"></a>

## The Transient interface

    (use-package uniline-transient
      :bind ("C-<insert>" . uniline-mode))

`Transient` interface was added recently to `Uniline`. This leaded to the
splitting of the single `uniline.el` file into 4 source
files. Hopefully, the added complexity remains hidden by the `Elpa`-`Melpa`
packaging system.


<a id="line-spacing"></a>

# Line spacing

The `line-spacing` setting in Emacs can change the display of a sketch.

The best looking effect is given by:

    (setq line-spacing nil)

You may want to change your current setting. `Uniline` may handle this
variable some day. Right now, `line-spacing` is left as a matter of
choice for everyone.

![img](images/line-spacing.png)

    
    ╭────┬────────┬────╮   ╺┯━━━━┯┯━━┯┯━┯┯━━━━━━━━┯┯━━━━━━━┯┯━━━━━━┯╸
    │▒▒▒▒╰────────╯▒▒▒▒│    │    │╰is╯╰a╯│        ││       │╰around╯
    │▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒│    ╰this╯       ╰sentence╯╰hanging╯
    │▒▒▒╭─╮▒▒▒▒▒▒╭─╮▒▒▒│            △
    │▒▒▒╰─╯▒▒▒▒▒▒╰─╯▒▒▒│            │                  △
    │▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒│            ╰─────────┬────────╯
    ╰──────────────────╯                    verbs
                 (setq line-spacing nil)


<a id="customization"></a>

# Customization

Type: `M-x customize-group uniline`.

Or `Menu bar ⟶ Options ⟶ Customize Emacs ⟶ Specific Group… ⟶ "uniline"`.

This invokes the standard Emacs customization system. Your settings
will be saved in the file pointed to by the `custom-file` variable if
set, or your `~/.emacs` file. (Along with all your other settings
unrelated to `Uniline`).

Two settings are special.

**Interface type.**

This switch is obsolete. Choosing between `Hydra` or `Transient` interface
is done by loading one or the other sub-package. See "Installation"
for details.

**Insert key.**

By default, the `<insert>` or `INS` key is the prefix for most
of the `Uniline` actions. Some computers do not have an `INS` key
(Apple?), or it is bound to some other command.

This can be changed temporarily or permanently. The customization
allows to set several keys.

Depending on whether Emacs is run in a graphical environment or a
text-only terminal, either the `<insert>` or the `<insertchar>` events are
generated by the `INS` key. Therefore, by default `Uniline` defines both
events as the `INS` key.

The other settings are self-explanatory.


<a id="how-uniline-behaves-with-its-environment"></a>

# How `Uniline` behaves with its environment?


<a id="compatibility-with-picture-mode"></a>

## Compatibility with Picture-mode

`Picture-mode` and `uniline-mode` are compatible. Their features overlap
somehow:

-   Both implement an unlimited buffer in east and south directions.
-   Both visually truncate long lines (actual text is not truncated).
-   Both set the overwrite mode (`uniline-mode` activates
    `overwrite-mode`, while `picture-mode` re-implements it)
-   Both are able to draw rectangles (`uniline-mode` in UNICODE,
    `picture-mode` in ASCII), copy and yank them.

They also have features unique to each:

-   `Picture-mode` writes in 8 possible directions
-   `Picture-mode` handles TAB stops
-   `Uniline-mode` draws lines and arrows


<a id="compatibility-with-artist-mode"></a>

## Compatibility with Artist-mode

`Artist-mode` and `uniline-mode` are mostly incompatible. This is because
`artist-mode` preempts the arrow keys, which give access to a large part
of `uniline-mode` features.

However, it is possible to use both one after the other.


<a id="compatibility-with-whitespace-mode"></a>

## Compatibility with Whitespace-mode

`Whitespace-mode` and `uniline-mode` are mostly compatible.

Why activate `whitespace-mode` while in `uniline-mode`? Because
`Uniline` creates a lot of white-spaces to implement an infinite
buffer. And it is funny to look at this activity.

To make `uniline-mode` and `whitespace-mode` fully compatible, disable
the newline visualization:

-   `M-x customize-variable whitespace-style`
-   uncheck `(Mark) NEWLINEs`

This is due to a glitch in `move-to-column` when a visual property is
attached to newlines. And `uniline-mode` makes heavy use of `move-to-column`.


<a id="compatibility-with-org-mode"></a>

## Compatibility with Org Mode

You may want to customize the shift extension mode in `Org Mode`. This
is because `Org Mode` preempts `shift-select-mode` for other useful
purposes. Just type:

    M-x customize-variable org-support-shift-select

and choose "when outside special context", which sets it to `t`.

You then get the shift-selection from `Org Mode`, not from `Uniline`. The
difference is that the `Uniline`'s one handles the infinite-ness of the
buffer.

Other than that, `Uniline` is compatible with `Org Mode`


<a id="org-mode-and-latex"></a>

## Org Mode and LaTex

Use the `pmboxdraw` LaTex module. This gives limited support for "box
drawing" characters in LaTex documents.

Example:

    
    #+LATEX_HEADER: \usepackage{pmboxdraw}
    
    #+begin_src text
    
    this works:
    ┌─────┐       ┌────────────┐
    │     ├───────┤            │
    └─────┘       │            │
    ┌─────┐  ┌────┤            │
    │     ├──┘    │            │
    └─────┘  ┌────┤            │
    ┌─────┐  │    │            │
    │     ├──┘    └────────────┘
    └─────┘
    
    this does not quite work:
       ┏━━━┓  ┏━━┓     ┏━━━━━┓
       ┃   ┃  ┃  ┣━━━━━┫     ┃
       ┃   ┗━━┛  ┃    ┏┛     ┃
       ┗━━━━━━━━━┛    ┗━━━━━━┛
    
    but that is OK:
         ┏━━━┓
         ┃   ┃
         ┗━━━┛
    
    that is OK too:
    ╺════╦══╗  ╔════╗
         ║ A║  ║ B  ╚══╗
         ╚══╝  ╚═══════╝
    
    this works:
    
    ├── dev
    └┬┬ release
     │├── new
     │└── old
     ├── graph
     └── non-graph
    
    #+end_src

Note that corners of thin lines should be sharp. There is no support
for rounded corners.

To export this Org Mode example to PDF through LaTex, type:

`C-c C-E l o`


<a id="what-about-t-tabs"></a>

## What about `\t` tabs?

Some files may contain tabs (the character `\t`). Those include
programming code (Python, Perl, C++, D, Rust, JavaScript and so on).

When `Uniline` draws something in the middle of a tab, it first
converts it to spaces, then proceeds as usual. This process is
invisible. So be cautious if tabs have a special meaning in the file.

One way to see what is going on, is to activate the `whitespace-mode`.


<a id="what-about-l-page-separation"></a>

## What about `^L` page separation?

`Uniline` does not work well with `^L` (page separation)
character. Nore with similar characters, like `^T`. When trying to
draw a line over such a character, the cursor may get stuck. This is
because those characters occupy twice the width of a normal character.

Just try to get away from `^L`, `^T` and such when drawing with
`Uniline`.


<a id="emacs-on-the-linux-console"></a>

## Emacs on the Linux console

Linux consoles are the 7 non-graphic screens which can be accessed
usually typing `C-M-F1`, `C-M-F2`, and so on. Such a screen is also
presented when connecting through `ssh` or `tls` into a non-graphical server.

By default they use a font
named "Fixed" with poor support for Unicode. However, it supports
lines of the 3 types, mixing all of them in thin lines though.

Another problem is that by default `S-<left>` and `C-<left>` are
indistinguishable from `<left>`. Same problem with `<right>`, `<up>`, `<down>`
and `<insert>`. This has nothing to do with Emacs. A solution can be
found here: <https://www.emacswiki.org/emacs/MissingKeys>


<a id="emacs-on-a-graphical-terminal-emulator"></a>

## Emacs on a graphical terminal emulator

This is the Emacs launched from a terminal typing `emacs -nw`. In this
environment, `<insert>` does not exists. It is replaced by
`<insertchar>`. This has already been taken into account by `Uniline`
by duplicating the key-bindings for the two flavors of this key.

If you decide to bind globally `C-<insert>` to the toggling of
`Uniline` minor mode as suggested, then you will have to do the same
for `C-<insertchar>`, for example with `use-package` in your
`~/.emacs` file:

    (use-package uniline
      :defer t
      :bind ("C-<insert>"     . uniline-mode)
      :bind ("C-<insertchar>" . uniline-mode))


<a id="emacs-on-windows"></a>

## Emacs on Windows

On Windows the only native mono-spaced fonts are `Lucida Console` and
`Courier New`. They are not mono-spaced for the Unicodes used by
`Uniline`.

Often, the `Consolas` font is present on Windows. It supports quite well
the required Unicodes to draw lines. A few glyphs produce unaligned
result though. They should be avoided under `Consolas`: `△▶▹◆`

Of course, other fonts may be installed. It is quite easy.


<a id="lisp-api"></a>

# Lisp API

Could `Uniline` be programmed (versus used interactively)?
Yes!

The API is usable programatically:

Move cursor while drawing lines by calling any of the 4 directions
functions:

-   `uniline-write-up↑`
-   `uniline-write-ri→`
-   `uniline-write-dw↓`
-   `uniline-write-lf←`

They expect a repeat `count` (usually 1) and optionally `force=t` to
overwrite the buffer

Set the current brush by calling any of the following:

-   `uniline--set-brush-nil   ;; write nothing`
-   `uniline--set-brush-0     ;; eraser`
-   `uniline--set-brush-1     ;; single thin line╶─╴`
-   `uniline--set-brush-2     ;; single thick line╺━╸`
-   `uniline--set-brush-3     ;; double line╺═╸`
-   `uniline--set-brush-block ;; blocks ▙▄▟▀`

Those functions are equivalent to:

-   `(setq uniline--brush nil)`
-   `(setq uniline--brush 0)`
-   `(setq uniline--brush 1)`
-   `(setq uniline--brush 2)`
-   `(setq uniline--brush 3)`
-   `(setq uniline--brush :block)`

except the functions also update the mode-line.

For instance, if we want to create a function to draw a "plus" sign,
we can code it as follows:

    (defun uniline-draw-plus ()
      (interactive)
      (uniline-write-ri→ 1)
      (uniline-write-dw↓ 1)
      (uniline-write-ri→ 1)
      (uniline-write-dw↓ 1)
      (uniline-write-lf← 1)
      (uniline-write-dw↓ 1)
      (uniline-write-lf← 1)
      (uniline-write-up↑ 1)
      (uniline-write-lf← 1)
      (uniline-write-up↑ 1)
      (uniline-write-ri→ 1)
      (uniline-write-up↑ 1))

Calling `M-x uniline-draw-plus` will result in this nice little
plus-shape:

![img](images/plus-shape.png)

     ╭╮
    ╭╯╰╮
    ╰╮╭╯
     ╰╯
    generated by
    M-x uniline-draw-plus

We may modify the function to accept the size of the shape as a
parameter:

    (defun uniline-draw-plus (size)
      (interactive "Nsize? ")
      (uniline-write-ri→ size)
      (uniline-write-dw↓ size)
      (uniline-write-ri→ size)
      (uniline-write-dw↓ size)
      (uniline-write-lf← size)
      (uniline-write-dw↓ size)
      (uniline-write-lf← size)
      (uniline-write-up↑ size)
      (uniline-write-lf← size)
      (uniline-write-up↑ size)
      (uniline-write-ri→ size)
      (uniline-write-up↑ size))

The `(interactive "Nsize? ")` form prompt user for the size of the shape
if not given as a parameter.

This API works in any mode, not only in `Uniline` minor mode. They take
care of the infiniteness of the buffer in the right and down
directions.

Other useful functions are:

Drawing and moving many characters at once:

-   `uniline-contour`
-   `uniline-fill`
-   `uniline-draw-inner-rectangle`
-   `uniline-draw-outer-rectangle`
-   `uniline-copy-rectangle`
-   `uniline-kill-rectangle`
-   `uniline-yank-rectangle`
-   `uniline-fill-rectangle`
-   `uniline-move-rect-up↑`
-   `uniline-move-rect-ri→`
-   `uniline-move-rect-dw↓`
-   `uniline-move-rect-lf←`

Constants for the 4 directions:

-   `uniline-direction-up↑ ;; constant 0`
-   `uniline-direction-ri→ ;; constant 1`
-   `uniline-direction-dw↓ ;; constant 2`
-   `uniline-direction-lf← ;; constant 3`

Changing text direction:

-   `uniline-text-direction-up↑`
-   `uniline-text-direction-ri→`
-   `uniline-text-direction-dw↓`
-   `uniline-text-direction-lf←`

or (in this case the mode-line is not updated):

-   `(setq uniline-text-direction uniline-direction-up↑)`
-   `(setq uniline-text-direction uniline-direction-ri→)`
-   `(setq uniline-text-direction uniline-direction-dw↓)`
-   `(setq uniline-text-direction uniline-direction-lf←)`

Call macro in any direction:

-   `uniline-call-macro-in-direction-up↑`
-   `uniline-call-macro-in-direction-ri→`
-   `uniline-call-macro-in-direction-dw↓`
-   `uniline-call-macro-in-direction-lf←`

Insert glyphs:

-   `uniline-insert-fw-arrow`
-   `uniline-insert-fw-square`
-   `uniline-insert-fw-oshape`
-   `uniline-insert-fw-cross`
-   `uniline-insert-bw-arrow`
-   `uniline-insert-bw-square`
-   `uniline-insert-bw-oshape`
-   `uniline-insert-bw-cross`

Rotate arrow or tweak 4-half-lines or 4-block characters:

-   `uniline-rotate-up↑`
-   `uniline-rotate-ri→`
-   `uniline-rotate-dw↓`
-   `uniline-rotate-lf←`

Move point, possibly extending the buffer in right and bottom
directions:

-   `uniline-move-to-column`
-   `uniline-move-to-line`
-   `uniline-move-to-lin-col`
-   `uniline-move-to-delta-column`
-   `uniline-move-to-delta-line`


<a id="installation"></a>

# Installation

Add the following lines to your `.emacs` file,
and reload it, if not already done:

    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.org/packages/")
                 t)
    (package-initialize)

Alternately you may customize this variable:

    M-x customize-variable package-archives

Then download the package:

    (package-install "uniline")

Alternately, you can download the Lisp files, and load them.

    (load-file "uniline-hydra.el")   ;; interpreted form
    (load-file "uniline-hydra.elc")  ;; byte-compiled form
    (load-file "uniline-hydra.eln")  ;; native-compiled form
    ;; this automatically
    ;; loads "uniline-core.el"
    ;; or    "uniline-core.elc"
    ;; or    "uniline-core.eln"

or if you prefer the Transient interface over the Hydra one:

    (load-file "uniline-transient.el")   ;; interpreted form
    (load-file "uniline-transient.elc")  ;; byte-compiled form
    (load-file "uniline-transient.eln")  ;; native-compiled form
    ;; this automatically
    ;; loads "uniline-core.el"
    ;; or    "uniline-core.elc"
    ;; or    "uniline-core.eln"

You should prefer the byte-compiled or native-compiles forms over the
interpreted form, because there are a lot of optimizations performed
at compile time.

You may want to give `uniline-mode` a key-binding. `use-package`
in your `$HOME/.emacs` file is great for that:

    (use-package uniline-hydra
      :bind ("C-<insert>" . uniline-mode))

or:

    (use-package uniline-transient
      :bind ("C-<insert>" . uniline-mode))

or as an alias to `uniline-hydra`:

    (use-package uniline
      :bind ("C-<insert>" . uniline-mode))

In this example, `C-<insert>` was chosen. You can use whatever keys combination you want.
`<insert>` happens to also be the key used inside `Uniline`.

If you do not have `use-package`, you can add those lines in your `~/.emacs` file:

    (require 'uniline-hydra)
    (bind-keys :package uniline-hydra ("C-<insert>" . uniline-mode))

The downside is that `Uniline` will be loaded as soon as `Emacs` is
launched, rather than deferred until invoked.

If the `Hydra` package is not installed, `Uniline` installation completes
nonetheless. Then `uniline-hydra` will lack most of its features.


<a id="related-packages"></a>

# Related packages

-   `artist-mode`: the ASCII art mode built into Emacs.

-   `ascii-art-to-unicode`: as the name suggest, converts ASCII drawings
    to UNICODE, giving results similar to those of `Uniline`.

-   `picture-mode`: as in `Uniline`, the buffer is infinite in east & south
    directions.

-   `ascii-art-to-unicode` ASCII art to UNICODE in Emacs. This is a
    standard ELPA package by Thien-Thi Nguyen (rest in peace). `Uniline`
    may call it to convert ASCII art drawings to equivalent
    UNICODE. `Uniline` arranges to not require a dependency on
    `ascii-art-to-unicode` by lazy evaluating a call to `aa2u`.

-   `org-pretty-table`: Org Mode tables *appear* to be drawn in UNICODE
    characters (actually they are still in ASCII).

-   `boxes`: draws artistic boxes around text, with nice looking unicorns,
    flowers, parchments, all in ASCII art.

-   `org-drawio`: a bridge between the Draw.Io editor and Emacs, producing
    drawing similar to those of `Uniline`, but in `.svg`.

-   `syntree`: draws ASCII trees on-the-fly from description.

-   `unicode-enbox`: create a UNICODE box around a text; input and output
    are strings.

-   `unicode-fonts`: in Emacs, helps alleviate the lack of full UNICODE
    coverage of most fonts.

-   `org-superstar`: prettify headings and plain lists in Org Mode, using
    UNICODE glyphs.

-   `charmap`: UNICODE table viewer for Emacs.

-   `insert-char-preview`: insert UNICODEs with character preview in
    completion prompt.

-   `list-unicode-display`: list all UNICODE characters, or a selection of
    them.

-   `show-font`: show font features in a buffer.

-   `ob-svgbob`: convert your ascii diagram scribbles into happy little
    SVG

-   `el-easydraw`: a full featured SVG editor right inside your Emacs

-   `asciiflow`: (not Emacs) draw on the web, then copy-paste your UNICODE text

-   `dot-to-ascii.ggerganov.com:` (not Emacs) describe your schema in the
    Graphviz language, and copy-past your UNICODE text.

-   `monosketch`: (not Emacs) draw on the web, then copy-paste your UNICODE text

-   `ibm-box-drawing-hydra.el`: keyboard interface to insert UNICODE
    box-drawing characters one at a time

-   `org-excalidraw`: integrate SVG images generated by excalidraw into
    Org Mode

-   `rcd-box`: create tables surrounded by box-drawing characters from
    Lisp descriptions

-   `ob-diagram`: generate various diagrams using diagrams backend

-   `ob-mermaid`: generate Mermaid diagrams within org-mode babel

-   `quail-boxdrawing.el`: input method for box drawing characters

-   `make-box.el`: box around part of a buffer

-   `vim drawit ascii diagrams`: in Vin, in ASCII


<a id="author-contributors"></a>

# Author, contributors

-   Thierry Banel, author

Feedback:

-   Chris Rayner (@riscy), gave recommendations prior to insertion in
    MELPA

-   Adam Porter (@alphapapa), suggested submitting `Uniline` to `ELPA`;
    should I?

-   Joost Kremers <https://github.com/joostkremers> found a bug in the
    minor-mode key-binding definitions, and incompatibility with

-   DogLooksGood <https://github.com/DogLooksGood> gave feedback on
    inserting usual characters not moving the cursor

-   LuciusChen & lhindir on GitHub, arthurno1 & karthink on Reddit,
    pushed toward `Transient` as the default interface instead of `Hydra`

-   karthink noted that `Transient` was now built into Emacs, loosening
    the dependencies conundrum, arthurno1 participated in the `Hydra` -
    `Transient` discussion

-   karthink pointed to the new `Aporetic` font family, which was added
    to the `Uniline` supported fonts

Contributors:

-   JD Smith (jdtsmith on GitHub) rewrote the `:lighter` for added
    flexibility (the information in the mode-line about the state of
    Uniline)

Utilities:

-   Oleh Krehel alias abo-abo for his package `Hydra`

-   The `Magit` team for the `Transient` library

-   Thien-Thi Nguyen (RIP) for his package `ascii-art-to-unicode`


<a id="license"></a>

# License

Copyright (C) 2024-2025  Thierry Banel

Uniline is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

Uniline is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

