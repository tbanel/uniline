

<a id="getting-started-in-10-seconds"></a>

# Getting started in 10 seconds

-   Type `M-x uniline-mode`
-   Move cursor with the arrow-keys on the keyboard `→ ← ↑ ↓`
-   Quit `C-c C-c`


<a id="new"></a>

# New

Use `C-t` to toggle one-liner vs. full fledged menus, both in Hydra &
Transient. In Hydra, it used to be `TAB`. Transient natively offers `C-t`,
but just in one direction; Uniline extends it to both directions.


<a id="table-of-contents"></a>

# Table of Contents

-   [Getting started in 10 seconds](#getting-started-in-10-seconds)
-   [New](#new)
-   [Pure UNICODE text diagrams in Emacs](#pure-unicode-text-diagrams-in-emacs)
-   [Minor mode](#minor-mode)
-   [Drawing lines](#drawing-lines)
    -   [Draw lines by moving the cursor](#draw-lines-by-moving-the-cursor)
    -   [Brush style](#brush-style)
    -   [Text direction](#text-direction)
-   [The <insert> key](#the-insert-key)
-   [Glyphs ▷ ▶ → □ ◆ ╮─ insertion & modification](#glyphs-------insertion--modification)
    -   [Arrows glyphs ▷ ▶ → ▹ ▸ ↔](#arrows-glyphs------)
    -   [Intersection glyphs ■ ◆ ●](#intersection-glyphs---)
    -   [Fine tweaking of lines](#fine-tweaking-of-lines)
-   [Rectangular actions](#rectangular-actions)
    -   [Drawing a rectangle](#drawing-a-rectangle)
    -   [Filling a rectangle](#filling-a-rectangle)
    -   [Moving a rectangle](#moving-a-rectangle)
    -   [Copying, killing, yanking a rectangle](#copying-killing-yanking-a-rectangle)
    -   [Dashed lines and other styles](#dashed-lines-and-other-styles)
    -   [ASCII to UNICODE](#ascii-to-unicode)
-   [Tracing a contour](#tracing-a-contour)
-   [Flood-fill](#flood-fill)
-   [Macros](#macros)
-   [Which fonts?](#which-fonts)
-   [Hydra or Transient?](#hydra-or-transient)
    -   [Selecting Hydra or Transient](#selecting-hydra-or-transient)
    -   [One-liner menus](#one-liner-menus)
    -   [The Hydra interface](#the-hydra-interface)
    -   [The Transient interface](#the-transient-interface)
-   [Customization](#customization)
    -   [Interface type (obsolete)](#interface-type-obsolete)
    -   [Insert key](#insert-key)
    -   [Maximum steps when drawing a contour](#maximum-steps-when-drawing-a-contour)
    -   [Cursor type](#cursor-type)
    -   [Hint style](#hint-style)
    -   [Welcome message visibility](#welcome-message-visibility)
    -   [Line spacing](#line-spacing)
    -   [Font](#font)
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
-   [Mouse support](#mouse-support)
-   [Installation](#installation)
-   [Related packages](#related-packages)
-   [Author, contributors](#author-contributors)
-   [License](#license)


<a id="pure-unicode-text-diagrams-in-emacs"></a>

# Pure UNICODE text diagrams in Emacs

Draw diagrams like those:

Document a command:


       pdfjam source.pdf 3-5,9
            ▲    ▲        ▲  ▲
    command╶╯    │        │  │
    input file╶──╯        │  │
    select pages 3,4,5╶───╯  │
    and page 9╶──────────────╯

Connect boxes with arrows:


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

Outline the Schrodinger's equation:


    
           ╭─────────────────────╴Derivative over time
           │     ╭──────────╭────╴State of quantum system at time t
           │     │          │     (the square of its absolute value
          ╭▽─╮ ╭─▽──╮     ╭─▽──╮   is the probability density)
    ┏━━━━━┷━━┷━┷━━━━┷━━━━━┷━━━━┷━┓
    ┃ i ħ d/dt |Ψ(t)> = Ĥ |Ψ(t)> ┃◁─╴Schrodinger's equation
    ┗━△━△━━━━△━━━━△━━━━━△━━━━△━━━┛
      │ │    ╰────╰─────┤────╰───╴Time
      │ │               ╰────────╴Hamiltonian
      │ ╰────────────────────────╴Reduced Plank constant
      ╰──────────────────────────╴Imaginary number i²=-1

Explain the structure of a sentence in a foreign language (which one?):


    
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


    
     ◀─(-)────────(+)──▶    ~╭──────╮~
      ▗──────────────╮     ~~│ ╭~~╮ │~~
      ▐              ╰╮     ~│ ╵  ╵ │~
    ╭□▐   1.5 volts  ╭╯□╮    ╰─╖  ╓─╯
    │ ▝▀▀▀▀▀▀▀▀▀▀▀▀▀▀▘  │      ╠━━╣
    │                   ╰──────╯  │
    ╰─────────────────────────────╯


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
achieved using UNICODE characters.

Most often, the text file will be encoded as UTF-8. This is becoming
the de-facto standard for text files, including source code files.

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
truncation. Also, a hollow cursor is provided (customizable). Those
settings are reset to their previous state when exiting `uniline-mode`.


<a id="drawing-lines"></a>

# Drawing lines

Use keyboard arrows to draw lines. Change the brush to any of the 6
styles.


<a id="draw-lines-by-moving-the-cursor"></a>

## Draw lines by moving the cursor

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

## Brush style

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


    
     current text                  current
        direction╶────╮       ╭───╴brush
                      ▼       ▼
    ══════════════════╧═══════╧══════════════
    U:** buff    (... →Uniline┼ ...)
    ═════════════════════════════════════════


<a id="text-direction"></a>

## Text direction

Usually, inserting text in a buffer moves the cursor to the right. (And
sometimes to the left for some locales). Any of the 4 directions can be
selected under `Uniline`. Just type any of:

-   `<insert> C-<up>`
-   `<insert> C-<right>`
-   `<insert> C-<down>`
-   `<insert> C-<left>`

The current direction is reflected in the mode-line, just before the
word `"uniline"`.


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

**Customization**

Another key may be defined instead of `<insert>`. Type:

    M-x customize-variable uniline-key-insert


<a id="glyphs-------insertion--modification"></a>

# Glyphs `▷ ▶ → □ ◆ ╮─` insertion & modification

Individual character glyphs may be inserted and changed.

-   Put the cursor where a glyphs should be edited or inserted.
-   Then press `<insert>` (this key may be customized, see the
    "Customization" chapter).

Arrows, squares, circles, crosses may be handled. Also lines may be
fine tweaked a single character at a time.


<a id="arrows-glyphs------"></a>

## Arrows glyphs `▷ ▶ → ▹ ▸ ↔`

When inserting an arrow, it points in the direction that the line
drawing follows.

`Uniline` supports 6 arrows types: `▷ ▶ → ▹ ▸ ↔`


    
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

## Intersection glyphs `■ ◆ ●`

There are a few UNICODE characters which are mono-space and symmetric
in the 4 directions. They are great at line intersections:

To insert a square `□ ■ ▫ ▪ ◆ ◊` type:
`<insert> s s s...` (`s` cycles, `S` cycles backward).

To insert a circular shape `· ∙ • ● ◦ Ø ø` type:
`<insert> o o o...` (`o` cycles, `O` cycles backward).

To insert a cross shape `╳ ╱ ╲ ÷ × ± ¤` type:
`<insert> x x x...` (`x` cycles, `X` cycles backward).

To insert a usual ASCII letter or symbol, just type it.

As the keys `- + = #` are preempted by `uniline-mode`, to type them,
prefix them with `<insert>`. Example: `<insert> -` inserts a `-` and
`<insert> +` inserts a `+`.


    
    <insert>
        │
        ▼
       ╭┴╮   ╭───────╮  ╭──────────────────╮
       │s├─▶─┤squares├──┤ □  ■  ▫  ▪  ◆  ◊ │
       ╰┬╯   ╰───────╯  ╰──────────────────╯
       ╭┴╮   ╭───────╮  ╭─────────────────────╮
       │o├─▶─┼circles┼──┤ ·  ∙  •  ●  ◦  Ø  ø │
       ╰┬╯   ╰───────╯  ╰─────────────────────╯
       ╭┴╮   ╭───────╮  ╭───────────────────╮
       │x├─▶─┼crosses┼──┤ ╳  ╱ ╲ ÷  ×  ±  ¤ │
       ╰┬╯   ╰───────╯  ╰───────────────────╯
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


<a id="fine-tweaking-of-lines"></a>

## Fine tweaking of lines


    
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


<a id="rectangular-actions"></a>

# Rectangular actions

-   Drawing,
-   filling,
-   moving,
-   copying & yanking,
-   change line & glyph styles,

those actions may be performed on a rectangular selection.

Select a rectangular region with `C-SPC` or `C-x SPC` and move the cursor.

You may also use `S-<arrow>` (`<arrow>` being any of the 4
directions) to extend the selection. The buffer grows as needed with
white spaces to accommodate the selection. Selection extension mode is
active when `shift-select-mode` is non-nil.

Or you may use the mouse to highlight the desired region.

All those region-highlighting are standard in Emacs, and unrelated to
Uniline.

Once you have a region highlighted, press `<insert>` (this key can be
customized, see the "Customization" chapter). The selection becomes
rectangular if it was not. You are offered a menu of possible actions.


<a id="drawing-a-rectangle"></a>

## Drawing a rectangle

To draw a rectangle in one shot, select a region, press `<insert>`, then
hit:

-   `r` to draw a rectangle inside the selection
-   `S-R` to draw a rectangle outside the selection
-   `C-r` to overwrite a rectangle inside the selection
-   `C-S-R` to overwrite a rectangle outside the selection

If needed, change the brush with any of `- + = # <delete>`


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

The usual `C-_` or `C-/` keys may be hit to undo, even with the region
still active visually.


<a id="filling-a-rectangle"></a>

## Filling a rectangle

While the rectangular mode is active, press `i` to fill the
rectangle. You will be asked to choose a character. You have those
options:

-   for a regular character like `t`, just type it.
-   `SPC` or `DEL` for a shade of grey `" ░▒▓█"` among the 5 available in
    UNICODE. `SPC` to make it darker and darker. `DEL` to make the rectangle
    lighter and lighter.
-   `C-y` to chose the first character in the top of the kill ring.

The above selection is the same as for the flood-fill action (see the
"Flood-fill" chapter).


<a id="moving-a-rectangle"></a>

## Moving a rectangle

Select a region, then press `<insert>`.

Use arrow keys to move the rectangle around. A numeric prefix may be
used to move the rectangle that many characters.

-   Under `Hydra`, be sure to specify the numeric prefix with just digits,
    without the `Alt` key. Typing `15 <left>` moves the rectangle 15
    characters to the left. `M-15 <left>` does not work.
-   Under `Transient`, use the `Alt` key, like anywhere else in Emacs. Type
    `M-15 <left>` to move the selected rectangle 15 characters to the left.

Press `q`, `<return>`, or `C-g` to stop moving the rectangle.

The `C-_` key may also be used to undo the previous movements, even
though the selection is still active.


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


<a id="copying-killing-yanking-a-rectangle"></a>

## Copying, killing, yanking a rectangle

A rectangle can be copied or killed, then yanked somewhere else.

Select a region, press `<insert>`, then:

-   `c` to copy
-   `k` to kill
-   `y` to yank (aka paste)

This is similar to the Emacs standard rectangle handling:

-   `C-x r r` copy rectangle to register
-   `C-x r k` kill rectangle
-   `C-x r y` yank killed rectangle

The first difference is that `Uniline` rectangles, when killed and
yanked, do not move surrounding characters.

The second difference is that the white characters of the yanked
rectangle are considered transparent. As a result, only non-blank
parts of the yanked rectangle are over-printed.

`Uniline` and Emacs standard rectangle share the same storage for copied
and killed rectangles, namely the `killed-rectangle` Lisp variable. So,
a rectangle can be killed one way, and yanked another way.


<a id="dashed-lines-and-other-styles"></a>

## Dashed lines and other styles


    
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
    or `S-<left>`, `S-<down>` and so on as described earlier).
-   Type `INS`, then `s` (as "style").

You will be offered a choice of styles:

-   `3`: vertical lines will become 3 dashes per character, while
    horizontal ones will get 2 dashes per character.
-   `4`: vertical and horizontal lines will get 4 dashes per character.
-   `h`: thin lines corners, which are usually rounded, become hard angles.
-   `+`: thin lines and intersections become thick, empty glyphs get
    filled.
-   `-`: thick lines and intersections become thin, filled glyphs are
    emptied.
-   `=`: thick and thin lines become double lines.
-   `0`: come back to standard base-line `Uniline` style: plain, not-dashed
    lines, thin corner rounded, ASCII art is converted to UNICODE.
-   `a`: apply the `aa2u-rectangle` function from the unrelated
    `ascii-art-to-unicode` package, to convert ASCII art to UNICODE (this
    only works if `ascii-art-to-unicode` is already installed).

Converting parts of a drawing from one style to another can produce
nice looking sketches.


    
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

## ASCII to UNICODE

The standard base-line `Uniline` (`INS s 0`) or `aa2u-rectangle` (`INS s a`)
conversions may be used to convert ASCII art to UNICODE. The original
ASCII art may be drawn for instance by the `artist-mode` or the
`picture-mode` packages.

To use `aa2u-rectangle`, install the `ascii-art-to-unicode` package by
Thien-Thi Nguyen (RIP), available on ELPA. `Uniline` does not requires a
dependency on this package, by lazy evaluating any call to
`aa2u-rectangle`.
See <https://elpa.gnu.org/packages/ascii-art-to-unicode.html>


    
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


<a id="tracing-a-contour"></a>

# Tracing a contour


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

When hitting capital letter: `<insert> S-C` the contour is
overwritten. This means that if there was already a different style of
line on the contour path, it is overwritten.

The shape is distinguished because it floats in a blank characters
ocean. For the shake of the contour function, blank characters are
those containing lines as drawn by `Uniline` (including true blank
characters). Locations outside the buffer are also considered blank.

The algorithm has an upper limit of `10000` steps. This avoids an
infinite loop in which the algorithm may end up in some rare
cases. One of those cases is when the contour crosses a new-page
character, displayed by Emacs as `^L`. `10000` steps require a fraction of
a second to run. For shapes really huge, you may launch the contour
command once again, at the point where the previous run ended.

This `10000` steps limit is customizable. Type:

    M-x customize-variable uniline-contour-max-steps


<a id="flood-fill"></a>

# Flood-fill


    
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
ring will be chosen as the filling character. (The kill ring is filled
by functions like `C-k` or `M-w`, unrelated to `Uniline`).

Typing `<return>` or `C-g` aborts the filling operation.

A rectangular shape may also be filled.

-   Mark a region
-   `<insert> i`
-   answer which character should be used to fill.

There is no limit on the area to fill. Therefore, the filling
operation may flood the entire buffer (but no more).


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
instance, if we want a doted-line instead of the continuous one, we
record a macro for one step:

    C-x (             ;; begin recording
    INS o             ;; insert a small dot
    <right> <right>   ;; draw a line over 2 characters
    C-x )             ;; stop recording

Then we call this macro repeatedly in any of the 4 directions:


    
    ·─·─·─·─·  ╷     ·──·
            │  │     │  │
            ·  ·     ·  ·
            │  │     │  │
            ·  ·─·─·─·  ·
            │           │
            ·─·─·─·─·─·─·

We can draw complex shapes by just drawing one step. Hereafter, we
call a macro in 4 directions, closing a square:


    
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
Emacs are displayed on another text editor or on the Web. Of course,
using the suggested fonts and the UNICODEs drawn by `Uniline` keeps you
away from those glitches.

To know which font Emacs has chosen for a given character, type:

`C-u C-x =`

Note that none of those commands downloads a font from the Web.
The font should already be available.


<a id="hydra-or-transient"></a>

# Hydra or Transient?

The basic usage of `Uniline` should be easy: just move the point, and lines
are traced. Change brush to draw thicker lines.

More complex actions are summoned by the `<insert>` key, with or without
selection. This is a single key to remember. Then a textual menu is
displayed, giving the possible keys continuations and their
meaning. All that is achieved by the `Hydra` or `Transient` libraries,
which are now part of Emacs (thanks!).

The `Hydra` and `Transient` libraries offer similar features. Some users
may prefer one or the other.

`Uniline` was developed from day one with `Hydra`. `Transient` is a late
addition.


<a id="selecting-hydra-or-transient"></a>

## Selecting Hydra or Transient

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

There is a third file, `uniline-code.elc`. Loading `uniline-hydra.elc` or
`uniline-transient.elc` automatically loads `uniline-core.elc`.


<a id="one-liner-menus"></a>

## One-liner menus

The multi-lines menus in Hydra and Transient are quite useful for
casual users. For seasoned users, those huge textual menus may
distract them from their workflow.

It is now possible to switch to less distracting textual menus. They
are displayed in the echo-area on a single line.

To do so, type:

-   `C-t` within a sub-mode (glyph insertion mode, rectangle handling,
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

Its default value can be customized and saved for future sessions:

`M-x customize-variable uniline-hint-style`

After customization it can be changed later, on a buffer per buffer
basis, with the `C-t` or `C-h TAB` keys.

Transient natively offers a similar setting:
`transient-show-popup`. (There is no such variable in Hydra). It can be
customized with `t`, `nil`, `0` (zero), or a number. This is similar but not
exactly the same as the Hydra behavior and the `uniline-hint-style`.
the Transient setting stays in effect until the `C-t` or `C-h TAB` keys
are not used, . As soon as one of those keys is invoked,
`transient-show-popup` is toggled (which does not happens in Transient
alone). The change is kept in effect throughout the Uniline session,
but no longer.


<a id="the-hydra-interface"></a>

## The Hydra interface

Put that in your `~/.emacs` file:

    (use-package uniline-hydra
      :bind ("C-<insert>" . uniline-mode))

It has been asked by `Transient`-only users to avoid installing the
`Hydra` package. Currently, it is not possible to make dependencies
conditional in `Melpa`. And removing the `Hydra` dependency would hurt
`Hydra` users. Therefore, for the time being, the `Hydra` package is still
installed when installing `Uniline` through `Melpa`.


<a id="the-transient-interface"></a>

## The Transient interface

Put that in your `~/.emacs` file:

    (use-package uniline-transient
      :bind ("C-<insert>" . uniline-mode))

`Transient` interface was added recently to `Uniline`. This leaded to the
splitting of the single `uniline.el` file into 4 source
files. Hopefully, the added complexity remains hidden by the `Elpa` -
`Melpa` packaging system.


<a id="customization"></a>

# Customization

Type: `M-x customize-group uniline`.

Or `Menu bar ⟶ Options ⟶ Customize Emacs ⟶ Specific Group… ⟶ "uniline"`.

This invokes the standard Emacs customization system. Your settings
will be saved in the file pointed to by the `custom-file` variable if
set, or your `~/.emacs` file. (Along with all your other settings
unrelated to `Uniline`).

Two settings are special: interface type (obsolete) & the insert
key. The other settings are self-explanatory


<a id="interface-type-obsolete"></a>

## Interface type (obsolete)

This switch is **obsolete**. Choosing between `Hydra` or `Transient` interface
is done by loading one or the other sub-package. See "Installation"
for details.


<a id="insert-key"></a>

## Insert key

By default, the `<insert>` or `INS` key is the prefix for most of the
`Uniline` actions. Some computers do not have an `INS` key, or it is bound
to some other command (Apple?).

This can be changed temporarily or permanently. The customization
allows to set several keys at the same time.

Depending on whether Emacs is run in a graphical environment or a
text-only terminal, either the `<insert>` or the `<insertchar>` events are
generated by the `INS` key. Therefore, by default `Uniline` defines both
events as the `INS` key.

Variable `uniline-key-insert`.


<a id="maximum-steps-when-drawing-a-contour"></a>

## Maximum steps when drawing a contour

Defaults to `10000`.
To avoid an infinite loop in some rare cases.

Variable `uniline-contour-max-steps`.


<a id="cursor-type"></a>

## Cursor type

Hollow by default, so that what is under the cursor remains visible.

There is the option to leave the cursor as it is.

Variable `uniline-cursor-type.`


<a id="hint-style"></a>

## Hint style

Currently only applicable to the `Hydra`.
It defaults to "full fledged menus".

Variable `uniline-hint-style`.

`Transient` offers a similar setting: `transient-show-popup`.


<a id="welcome-message-visibility"></a>

## Welcome message visibility

Default is "on". Turn it "off" for less distraction.

Even when turned of, the welcome message can still be displayed by
pressing `C-h TAB`.

Variable `uniline-show-welcome-message`.


<a id="line-spacing"></a>

## Line spacing

The `line-spacing` setting in Emacs can change the display of a
sketch. (This setting is unrelated to `Uniline`).

The best looking effect is given by:

    (setq line-spacing nil)

You may want to change your current setting. `Uniline` may handle this
variable some day. Right now, `line-spacing` is left as a matter of
choice for everyone.


    
    ╭────┬────────┬────╮   ╺┯━━━━┯┯━━┯┯━┯┯━━━━━━━━┯┯━━━━━━━┯┯━━━━━━┯╸
    │▒▒▒▒╰────────╯▒▒▒▒│    │    │╰is╯╰a╯│        ││       │╰around╯
    │▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒│    ╰this╯       ╰sentence╯╰hanging╯
    │▒▒▒╭─╮▒▒▒▒▒▒╭─╮▒▒▒│            △
    │▒▒▒╰─╯▒▒▒▒▒▒╰─╯▒▒▒│            │                  △
    │▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒│            ╰─────────┬────────╯
    ╰──────────────────╯                    verbs
                 (setq line-spacing nil)


<a id="font"></a>

## Font

Face customization is unrelated to `Uniline`. However, `Uniline` can
assist in choosing a good font and customizing the `default` face. See
the "Which fonts?" chapter.

Type `<insert> f` to select a font just for the current `Uniline`
session. Type `*` to enter the Emacs customization of the `default` face
and retain your choice for future sessions.


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

Thanks to jdtsmith (GitHub) for sharing a funny fact he discovered. If
a source block is created with the `Uniline` language (`Uniline` is
**not** a language like `C++,` `Python`, or `Bash`), then it can be
edited (`M-x org-edit-special`) with `uniline-mode` automatically
activated.

    #+begin_src uniline
    ╭───╮   ╭───╮
    │ ╷ ╰───╯ ╷ │
    │ ╰─    ╶─╯ │
    ╰╮ ●     ● ╭╯
     │      ╷  │
     ╰╮ ────╯ ╭╯
      ╰───────╯
    #+end_src


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
character. Nor with similar characters, like `^T`. When trying to
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
environment, `<insert>` does not exist. It is replaced by
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

The API is usable programmatically:

Move cursor while drawing lines by calling any of the 4 directions
functions:

-   `uniline-write-up↑`
-   `uniline-write-ri→`
-   `uniline-write-dw↓`
-   `uniline-write-lf←`

They expect a repeat `count` (usually 1) and optionally `force=t` to
overwrite the buffer

Set the current brush by calling any of the following:

-   `uniline--set-brush-nil`   ;; write nothing
-   `uniline--set-brush-0`     ;; eraser
-   `uniline--set-brush-1`     ;; single thin line╶─╴
-   `uniline--set-brush-2`     ;; single thick line╺━╸
-   `uniline--set-brush-3`     ;; double line╺═╸
-   `uniline--set-brush-block` ;; blocks ▙▄▟▀

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

The `(interactive "Nsize? ")` form prompts user for the size of the
shape if not given as a parameter.

This API works in any mode, not only in `Uniline` minor mode. It takes
care of the infiniteness of the buffer in the right and down
directions.

There are other useful functions operating on many characters at
once. Contour tracing and flood-filling are among them:

-   `uniline-contour`
-   `uniline-fill`

The following functions operate on a rectangular region, which must be
active prior to calling them:

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

-   `uniline-direction-up↑` ;; constant 0
-   `uniline-direction-ri→` ;; constant 1
-   `uniline-direction-dw↓` ;; constant 2
-   `uniline-direction-lf←` ;; constant 3

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

Here are the lowest level functions. Move point, possibly extending
the buffer in right and bottom directions:

-   `uniline-move-to-column`
-   `uniline-move-to-line`
-   `uniline-move-to-lin-col`
-   `uniline-move-to-delta-column`
-   `uniline-move-to-delta-line`

A drawing in a rectangular selection may have its style changed:

-   `uniline-change-style-dot-3-2`      ;; 3 dashes vert. ┆, 2 horiz. ╌
-   `uniline-change-style-dot-4-4`      ;; 4 dashes vert. ┊ & horiz. ┈
-   `uniline-change-style-standard`     ;; back to Uniline base style
-   `uniline-change-style-hard-corners` ;; rounded corners╭╴become hard┌
-   `uniline-change-style-thin`         ;; convert to ╭╴ thin lines
-   `uniline-change-style-thick`        ;; convert to ┏╸ thick lines
-   `uniline-change-style-double`       ;; convert to ╔═ thick lines
-   `uniline-aa2u-rectangle`            ;; call aa2u to convert ASCII to Unicode

The above functions require a region to be marked.


<a id="mouse-support"></a>

# Mouse support

The out-of-the-box mouse support of Emacs works perfectly. Except when
the mouse clicks on a position outside the buffer. This happens when
clicking past the end of a too short line, or past the end of the buffer.

To handle those cases, a few standard Emacs functions have been
extended to add blank characters or blank lines. Doing so, the
mouse-click now falls on a valid part of the buffer. Of course, those
extensions are only active on `uniline-mode` activated buffers.

Beware that when the window is at the same time zoomed with `C-x C-+
C--` AND horizontally scrolled with `C-x <`, the cursor positioning is
not accurate. This is due to Emacs limitations and bugs. Just click
twice to fix the inaccuracy.


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

You should prefer the byte-compiled or native-compiled forms over the
interpreted forms, because there are a lot of optimizations performed
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

In this example, `C-<insert>` was chosen. You can use whatever key combinations you want.
`<insert>` happens to also be the key used inside `Uniline`.

If you do not have `use-package`, you can add those lines in your `~/.emacs` file:

    (require 'uniline-hydra)
    (bind-keys :package uniline-hydra ("C-<insert>" . uniline-mode))

The downside is that `Uniline` will be loaded as soon as `Emacs` is
launched, rather than deferred until invoked.


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

-   `vim drawit ascii diagrams`: in Vim, in ASCII


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

-   karthink pointed to the new `Aporetic` font family, which was then
    added to the `Uniline` supported fonts

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

