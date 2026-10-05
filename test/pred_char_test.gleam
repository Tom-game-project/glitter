import gleam/io
import gleam/string
import glitter/glitter.{
  type Span, Span, choice_p, fixed_point_combinator, ignorethen_p, many1_p,
  many_p, map_p, pred_char_p, pred_char_with_span_p, span_gather, then_p,
  thenignore_p, utf_end_p, utf_end_with_span_p, word_with_span_p,
}
import list1/list1

pub type Paren {
  Paren(List(Paren))
  Num(String)
  Word(String)
  Other
}

pub type ParseErr {
  EndErr
  ParenErr
  CharNotFound
  WordErr

  OtherwiseErr
}

pub fn pred_char_test() -> Nil {
  let str = "{{ABC}{CCC{123}ABC}}"

  let open_c = pred_char_p(string.to_utf_codepoints("{"), CharNotFound)
  let close_c = pred_char_p(string.to_utf_codepoints("}"), CharNotFound)

  let word_parser =
    pred_char_p(string.to_utf_codepoints("ABC"), CharNotFound)
    |> many1_p
    |> map_p(fn(inner) {
      inner |> list1.to_list |> string.from_utf_codepoints |> Word
    })

  let number_parser =
    pred_char_p(string.to_utf_codepoints("1234567890"), CharNotFound)
    |> many1_p
    |> map_p(fn(inner) {
      inner |> list1.to_list |> string.from_utf_codepoints |> Num
    })

  let entry_parser =
    {
      use dispatch <- fixed_point_combinator
      [
        word_parser,
        number_parser,
        open_c
          |> ignorethen_p({
            use inner <- map_p(dispatch)
            Paren(inner)
          })
          |> thenignore_p(close_c),
      ]
      |> choice_p(OtherwiseErr)
      |> many_p
    }
    |> thenignore_p(utf_end_p(EndErr))

  case
    str
    |> string.to_utf_codepoints
    |> entry_parser
  {
    Ok(#(v, remain)) -> {
      echo v
      io.println("remain :\"" <> remain |> string.from_utf_codepoints <> "\"")
    }
    Error(_err) -> {
      io.println("failed to parser")
    }
  }
}

pub type SpanParen {
  SpanParen(Span, List(SpanParen))
  SpanNum(Span, String)
  SpanWord(Span, String)
  Let
}

pub fn pred_char_with_span_test() -> Nil {
  let str = "{{ABC}{CCC{123}ABC}}"

  let open_c =
    pred_char_with_span_p(string.to_utf_codepoints("{"), fn(_s) { CharNotFound })
  let close_c =
    pred_char_with_span_p(string.to_utf_codepoints("}"), fn(_s) { CharNotFound })
  let new_line_c =
    pred_char_with_span_p(string.to_utf_codepoints("\n"), fn(_s) {
      CharNotFound
    })

  let many1_pred_char =
    pred_char_with_span_p(string.to_utf_codepoints("ABC"), fn(_s) {
      CharNotFound
    })
    |> many1_p
    |> map_p(fn(inner) {
      let #(span, utf_codepoints) = span_gather(inner)
      SpanWord(
        span,
        utf_codepoints |> list1.to_list |> string.from_utf_codepoints,
      )
    })

  let word_let_p =
    word_with_span_p("let" |> string.to_utf_codepoints, Let, fn(_s) { WordErr })
  let word_fn_p =
    word_with_span_p("fn" |> string.to_utf_codepoints, Let, fn(_s) { WordErr })
  let word_loop_p =
    word_with_span_p("loop" |> string.to_utf_codepoints, Let, fn(_s) { WordErr })

  let pad_p =
    pred_char_with_span_p(string.to_utf_codepoints(" \n"), fn(_s) {
      CharNotFound
    })

  let number_parser =
    pred_char_with_span_p(string.to_utf_codepoints("1234567890"), fn(_s) {
      CharNotFound
    })
    |> many1_p
    |> map_p(fn(inner) {
      let #(span, utf_codepoints) = span_gather(inner)

      SpanNum(
        span,
        utf_codepoints |> list1.to_list |> string.from_utf_codepoints,
      )
    })

  let entry_parser =
    {
      use dispatch <- fixed_point_combinator
      [
        many1_pred_char,

        number_parser,

        {
          use #(#(#(open_span, _), inner_paren), #(close_span, _)) <- map_p(
            open_c
            |> then_p(dispatch)
            |> then_p(close_c),
          )
          SpanParen(
            Span(start: open_span.start, end: close_span.end),
            inner_paren,
          )
        },
      ]
      |> choice_p(OtherwiseErr)
      |> many_p
    }
    |> thenignore_p(utf_end_with_span_p(EndErr))

  case entry_parser(#(0, str |> string.to_utf_codepoints)) {
    Ok(#(v, remain)) -> {
      echo str
      echo v
      //io.println("remain :\"" <> remain |> string.from_utf_codepoints <> "\"")
      Nil
    }
    Error(_err) -> {
      io.println("failed to parser")
    }
  }
}
