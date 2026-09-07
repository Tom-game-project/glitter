import gleam/io
import gleam/string
import glitter/glitter.{
  type Parser, choice_p, end_p, fixed_point_combinator, ignorethen_p, many1_p,
  many_p, map_p, map_then_p, or_p, pred_char_p, thenignore_p, utf_end_p, word_p,
}

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
    |> map_p(fn(inner) { inner |> string.from_utf_codepoints |> Word })

  let number_parser =
    pred_char_p(string.to_utf_codepoints("1234567890"), CharNotFound)
    |> many1_p
    |> map_p(fn(inner) { inner |> string.from_utf_codepoints |> Num })

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
