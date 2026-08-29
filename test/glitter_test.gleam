import gleam/io
import gleam/list
import gleam/string
import gleeunit
import glitter/glitter.{
  type Parser, choice_p, end_p, ignorethen_p, many_p, map_p, map_then_p, or_p,
  rec_p, thenignore_p, word_p,
}

pub fn main() -> Nil {
  gleeunit.main()
}

pub type Node {
  Pre(String)
  A
  B
}

pub type ParseError {
  ExpectedA
  ExpectedB
  Fail
}

pub fn or_p_test() {
  io.println("or_p_test")
  let str = "Ahello world"

  // let #(c, remain) = #(string.first(str), string.drop_start(str, 1))
  // io.println("char: " <> c)
  let p1: Parser(String, Node, ParseError) = word_p("A", A, ExpectedA)
  let p2: Parser(String, Node, ParseError) = word_p("B", B, ExpectedB)

  let n_p = or_p(p1, p2)

  case n_p(str) {
    Ok(#(v, remain)) -> {
      io.println("Success to parse")
      case v {
        Pre(pre) -> {
          io.println("pre: " <> pre)
        }
        A -> {
          io.println("pre: A")
        }
        B -> {
          io.println("pre: B")
        }
      }
      io.println("remain: " <> remain)
    }
    Error(_e) -> {
      io.println("Failed to Parse")
    }
  }
}

pub fn then_p_test() {
  io.println("then_p_test")
  //let str = "letmuthello world"
  let str = "xxxyyyletmuthello world"

  // let #(c, remain) = #(string.first(str), string.drop_start(str, 1))
  // io.println("char: " <> c)
  let p1: Parser(String, String, ParseError) = word_p("let", "let", ExpectedA)
  let p2: Parser(String, String, ParseError) = word_p("mut", "mut", ExpectedB)

  let p3: Parser(String, String, ParseError) = word_p("xxx", "xxx", ExpectedA)
  let p4: Parser(String, String, ParseError) = word_p("yyy", "yyy", ExpectedB)

  let n_p1 = map_then_p(p1, p2, fn(a, b) { Pre(a <> b) })
  let n_p2 = map_then_p(p3, p4, fn(a, b) { Pre(a <> b) })

  let n_p = or_p(n_p1, n_p2)
  let m_p = many_p(n_p)

  case m_p(str) {
    Ok(#(v, remain)) -> {
      io.println("Success to parse")
      print_list(v, fn(a) {
        case a {
          Pre(pre) -> {
            "pre: " <> pre
          }
          A -> {
            "pre: A"
          }
          B -> {
            "pre: B"
          }
        }
      })
      io.println("remain: " <> remain)
    }
    Error(_e) -> {
      io.println("Failed to Parse")
    }
  }
}

fn print_list(lst: List(a), stringify: fn(a) -> String) -> Nil {
  lst
  |> list.map(stringify)
  |> string.join(", ")
  |> io.println
}

pub type Paren {
  Paren(List(Paren))
  AtomA
  AtomB
  Other
}

pub fn rec_test() {
  io.println("rec_test")
  let str = "{{A}{ABA}}"

  let a_p = word_p("A", AtomA, ExpectedA)
  let b_p = word_p("B", AtomB, ExpectedB)

  let open_c = word_p("{", Other, ExpectedA)
  let close_c = word_p("}", Other, ExpectedA)

  let p =
    {
      use dispatch <- rec_p
      [
        a_p,
        b_p,
        open_c
          |> ignorethen_p({
            use inner <- map_p(dispatch)
            Paren(inner)
          })
          |> thenignore_p(close_c),
      ]
      |> choice_p(ExpectedA)
      |> many_p
    }
    |> thenignore_p(end_p(ExpectedA))

  case p(str) {
    Ok(#(v, remain)) -> {
      io.println("")
      echo v
      // print_tree_list(v)
      io.println("remain \"" <> remain <> "\"")
      io.println("success to parse")
    }
    Error(_e) -> {
      io.println("failed to parse")
    }
  }
}
