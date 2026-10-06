import gleam/int
import gleam/option
import gleam/io
import gleam/list
import gleam/string
import glitter/glitter.{
  type Span, Span, choice_p, fixed_point_combinator, foldl, ignorethen_p,
  many1_p, many_p, map_p, or_p, pred_char_p, pred_char_with_span_p, separated_by,
  span_gather, then_p, thenignore_p, trymap_p, list_end_p, list_end_with_span_p,
  word_with_span_p, ignore, reconsider, skip_ignore_many_p, just
}
import list1/list1

pub type ParseErr {
  EndErr
  ParenErr
  CharNotFound
  WordErr

  OtherwiseErr
  InvalidOperator
}

pub type BinOpe {
  Add
  Mul
  Sub
  Div
  Pow
}

pub type UnaryOpe {
  Plus
  // +expr
  Minus
  // -expr
  Point
  // *expr
}

pub type UntypedExpr {
  Paren(Span, UntypedExpr)
  Bin(#(Span, BinOpe), UntypedExpr, UntypedExpr)
  Unary(#(Span, UnaryOpe), UntypedExpr)
  Num(Span, String)
  Word(Span, String)
  Call(Span, UntypedExpr, List(UntypedExpr))
}

fn get_span(a: UntypedExpr) -> Span {
  case a{
    Paren(span, _) -> span
    Bin(#(span, _), _, _) -> span
    Unary(#(span, _), _) -> span
    Num(span, _) -> span
    Word(span, _) -> span
    Call(span, _, _) -> span
  }
}

pub type Token {
  RParen
  LParen
  Comma
  BinOpe(BinOpe)
  NumT(String)
  WordT(String)
}

const indent_space = "  "

fn span_string(span: Span) -> String {
  " @ Span(" <> int.to_string(span.start) <> ", " <> int.to_string(span.end) <> ")"
}

fn untyped_expr_to_string(depth: Int, ast: UntypedExpr) -> String {
  string.repeat(indent_space, times: depth)
  <> case ast {
    Paren(span, inner_ast) ->
      "Paren" <> span_string(span) <> "\n" 
        <> untyped_expr_to_string(depth + 1, inner_ast)

    Bin(#(span, ope), expr1, expr2) ->
      case ope {
        Add -> "Add"
        Sub -> "Sub"
        Div -> "Div"
        Mul -> "Mul"
        Pow -> "Pow"
      }
      <> span_string(span) <> "\n" 
      <> untyped_expr_to_string(depth + 1, expr1)
      <> untyped_expr_to_string(depth + 1, expr2)

    Unary(#(span, ope), expr) ->
      case ope {
        Minus -> "Minus"
        Plus -> "Plus"
        Point -> "Point"
      }
      <> span_string(span) <> "\n" 
      <> untyped_expr_to_string(depth + 1, expr)

    Num(span, num_string) -> "Num:" <> num_string <> span_string(span) <> "\n" 

    Word(span, word) -> "Word:" <> word <> span_string(span) <> "\n" 

    Call(span, func_name, args) ->
      "Func:" <> span_string(span) <> "\n" 
      <> untyped_expr_to_string(depth + 1, func_name)
      <> "\n"
      <> string.join(
        list.map(args, fn(in) { untyped_expr_to_string(depth + 1, in) }),
        "",
      )
  }
}

fn lexer(
  input: #(Int, List(UtfCodepoint)),
) -> Result(#(List(#(Span, Token)), #(Int, List(UtfCodepoint))), ParseErr) {
  let open_paren_c =
    pred_char_with_span_p(string.to_utf_codepoints("("), fn(_s) { CharNotFound })
    |> map_p(fn (in) {#(in.0, LParen)})
  let close_paren_c =
    pred_char_with_span_p(string.to_utf_codepoints(")"), fn(_s) { CharNotFound })
    |> map_p(fn (in) {#(in.0, RParen)})
  let comma_c =
    pred_char_with_span_p(string.to_utf_codepoints(","), fn(_s) { CharNotFound })
    |> map_p(fn (in) {#(in.0, Comma)})

  let number_parser =
    pred_char_with_span_p(string.to_utf_codepoints("1234567890"), fn(_s) {
      CharNotFound
    })
    |> many1_p
    |> map_p(fn(inner) {
      let #(span, utf_codepoints) = span_gather(inner)

      #(span, NumT(utf_codepoints |> list1.to_list |> string.from_utf_codepoints))
    })

  let binop_p =
    pred_char_with_span_p(string.to_utf_codepoints("+-*/"), fn(_s) {
      CharNotFound
    })
    |> trymap_p(fn(inner) {
      let #(span, c) = inner
      case string.utf_codepoint_to_int(c)
      {
        0x2b -> Ok(#(span, BinOpe(Add)))
        0x2d -> Ok(#(span, BinOpe(Sub)))
        0x2a -> Ok(#(span, BinOpe(Mul)))
        0x2f -> Ok(#(span, BinOpe(Div)))
        _ -> Error(InvalidOperator)
      }
    })

  let word_parser_orig =
    pred_char_with_span_p(
      string.to_utf_codepoints("abcdefghijklmnopqrstuvwxy"),
      fn(_s) { CharNotFound },
    )
    |> many1_p
    |> map_p(fn(inner) {
      let #(span, utf_codepoints) = span_gather(inner)

      #(span, WordT(utf_codepoints |> list1.to_list |> string.from_utf_codepoints))
    })

  let pad_p = 
    pred_char_with_span_p(
      string.to_utf_codepoints(" \n\t"),
      fn(_s) { CharNotFound },
    )
    |> many1_p

  let token_list = choice_p([
    open_paren_c,
    close_paren_c,
    comma_c,
    number_parser,
    word_parser_orig,
    binop_p,
  ], OtherwiseErr)
  |> reconsider()
  |> or_p(
    pad_p |> ignore()
  )
  |> skip_ignore_many_p 

  token_list(input)
}

fn expr_parser(
  input: List(#(Span, Token)),
) -> Result(#(UntypedExpr, List(#(Span, Token))), ParseErr) {
  let open_paren_c = just(
    fn (input: #(Span, Token)) {
      case input {
        #(span, LParen) -> option.Some(#(span, LParen)) 
        _ -> option.None 
      }
    }, CharNotFound)
  let close_paren_c = just(fn (input) {case input { #(span, RParen) -> option.Some(#(span, RParen)) _ -> option.None }}, CharNotFound)
  let comma_c = just(fn (input) {case input { #(span, Comma) -> option.Some(#(span, Comma)) _ -> option.None }}, CharNotFound)
  let number_parser = just(fn (input) {case input { #(span, NumT(str)) -> option.Some(Num(span, str)) _ -> option.None }}, CharNotFound)
  let word_parser_orig = just(fn (input) {case input { #(span, WordT(str)) -> option.Some(Word(span, str)) _ -> option.None }}, CharNotFound)
  let binop_p = just(
    fn (input) {
      case input { 
        #(span, BinOpe(binope)) -> {
          option.Some(#(span, binope))
        }
        _ -> option.None}}, CharNotFound)

  let expr_p =
    {
      use expr <- fixed_point_combinator

      let paren_p = {
        use #(#(#(open_span, _), inner_paren), #(close_span, _)) <- map_p(
          open_paren_c
          |> then_p(expr)
          |> then_p(close_paren_c),
        )
        Paren(Span(start: open_span.start, end: close_span.end), inner_paren)
      }

      let call = {
        use #(#(#(word, _open_c), arg_list), close_c) <- map_p(
          word_parser_orig
          |> then_p(open_paren_c)
          |> then_p(separated_by(expr, comma_c))
          |> then_p(close_paren_c),
        )

        Call(Span(start: get_span(word).start, end: close_c.0.end), word, arg_list)
      }

      let ident =
        [number_parser, call, word_parser_orig, paren_p]
        |> choice_p(OtherwiseErr)

      let unary =
        {
          use #(span, binope) <- trymap_p(binop_p)
          case binope {
            Add -> Ok(#(span, Plus))
            Sub -> Ok(#(span, Minus))
            _ -> Error(InvalidOperator)
          }
        }
        |> then_p(ident)
        |> map_p(fn(i) {
          let #(ope, a) = i
          Unary(ope, a)
        })
        |> or_p(ident)

      let product =
        unary
        |> foldl(
          {
            use #(span, binope) <- trymap_p(binop_p)
            case binope {
              Mul -> Ok(#(span, Mul))
              Div -> Ok(#(span, Div))
              _ -> Error(InvalidOperator)
            }
          }
            |> then_p(unary)
            |> many_p,
          fn(acc, o) { Bin(o.0, acc, o.1) },
        )

      let sum =
        product
        |> foldl(
          {
            use #(span, binope) <- trymap_p(binop_p)
            case binope {
              Add -> Ok(#(span, Add))
              Sub -> Ok(#(span, Sub))
              _ -> Error(InvalidOperator)
            }
          }
            |> then_p(product)
            |> many_p,
          fn(acc, o) { Bin(o.0, acc, o.1) },
        )

      [
        sum,
        number_parser,
        word_parser_orig,
        paren_p
      ]
      |> choice_p(OtherwiseErr)
    }
    |> thenignore_p(list_end_p(EndErr))

  expr_p(input)
}

pub fn normal_expr_test() -> Nil {
  let str = "123 + 42 * 333  + (x +1)+ f(x ,y+ 1)"
  // let str = "-1+-1"

  case lexer(#(0, str |> string.to_utf_codepoints)) {
    Ok(#(lexed, remain)) -> {
      echo lexed
      case expr_parser(lexed) {
        Ok(#(v, remain)) -> {
          // echo str
          // echo v
          io.println(str)
          io.println(untyped_expr_to_string(0, v))
          Nil
        }
        Error(err) -> {
          echo err
          echo "failed to parser"
          Nil
        }
      }
    }
    Error(err) -> {
      echo err
      echo "lexer failed"
      Nil
    }
  }
  Nil
}
