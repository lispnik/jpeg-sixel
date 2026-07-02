# jpeg-sixel

Convert JPEG images to [sixel](https://en.wikipedia.org/wiki/Sixel) escape
sequences for display in sixel-capable terminals (Kitty, iTerm2, xterm, foot,
recent VTE terminals). Built on [cl-jpeg](https://github.com/sharplispers/cl-jpeg).

## Features

- Median-cut color quantization to <=256 colors
- Floyd-Steinberg dithering (serpentine scan) with a cached nearest-color lookup
- Box-average downscale-to-fit (`:max-width` / `:max-height` / `:cols`)
- Best-effort terminal cell-size probe (XTerm `14t`/`18t`) so `:cols` sizes to
  the real terminal geometry, with a safe fallback when there is no tty or the
  terminal does not answer

## Dependencies

- `cl-jpeg` **with progressive-JPEG support** (merged upstream Feb 2026, PR #43).
  Older pinned versions will signal an error on progressive JPEGs before the
  sixel code runs. If your dist lags, point it at the cl-jpeg git head.

## Usage

```lisp
(asdf:load-system :jpeg-sixel)

;; write a sixel image straight to the terminal, fit to 80 columns, dithered
(jpeg-sixel:write-jpeg-sixel "photo.jpg" *standard-output* :cols 80 :dither t)

;; or get the string
(jpeg-sixel:jpeg->sixel "photo.jpg" :max-width 400 :max-colors 128 :dither t)
```

### Detecting sixel support

Before emitting, you can ask the terminal whether it supports sixel via its
Primary Device Attributes:

```lisp
(jpeg-sixel:sixel-supported-p)
;; => T         terminal reports sixel (DA feature code 4)
;; => NIL       terminal answered, but without sixel
;; => :UNKNOWN  no tty, no reply, or a malformed response
```

Treat `:UNKNOWN` as "proceed if you like, but I couldn't confirm" rather than a
hard no — some terminals support sixel without advertising it in DA.

Note: the escape sequence must reach the actual terminal stream. From a SLIME
REPL the bytes will not render; direct it at the tty.

## Tests

```lisp
(asdf:test-system :jpeg-sixel)
```

The test suite encodes its own fixture via cl-jpeg, so no image files are needed.

## License

MIT
