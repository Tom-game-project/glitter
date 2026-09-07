import gleam/io
import gleam/list
import gleam/option
import gleam/string
import glitter/glitter.{
  type Parser, type Span, Span, choice_p, end_p, fixed_point_combinator,
  ignorethen_p, many1_p, many_p, map_p, map_then_p, or_p, pred_char_p,
  pred_char_with_span_p, span_gather, then_p, thenignore_p, utf_end_p,
  utf_end_with_span_p, word_p,
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
}

pub fn pred_char_with_span_test() -> Nil {
  let str = "{{ABC}{CCC{123}ABC}}"

  let open_c =
    pred_char_with_span_p(string.to_utf_codepoints("{"), CharNotFound)
  let close_c =
    pred_char_with_span_p(string.to_utf_codepoints("}"), CharNotFound)

  let word_parser =
    pred_char_with_span_p(string.to_utf_codepoints("ABC"), CharNotFound)
    |> many1_p
    |> map_p(fn(inner) {
      let #(span, utf_codepoints) = span_gather(inner)
      SpanWord(
        span,
        utf_codepoints |> list1.to_list |> string.from_utf_codepoints,
      )
    })

  let number_parser =
    pred_char_with_span_p(string.to_utf_codepoints("1234567890"), CharNotFound)
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
        word_parser,

        number_parser,

        // open_c
        //   |> then_p({
        //     use inner <- map_p(dispatch)
        //     inner
        //   })
        //   |> then_p(close_c)
        //   |> map_p(fn(input) {
        //     let #(#(#(open_span, _), inner), #(close_span, _)) = input
        //     SpanParen(Span(start: open_span.start, end: close_span.end), inner)
        //   }),
        {
          use #(#(#(open_span, _), inner_paren), #(close_span, _)) <- map_p(
            open_c
            |> then_p({
              use inner <- map_p(dispatch)
              inner
            })
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
