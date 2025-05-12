zig-mode

(a "const " (p "NAME") " = [_]" (p "TY") "{ " q " };")

(v "var " p " = " q ";")
(c "const " p " = " q ";")
(im "const " p " = @import(\"" q "\");")

(f "fn " (p "NAME") "(" p ") " (p "R") " {" n>
   q n
   "}" >)

(pf "pub fn " (p "NAME") "(" p ") " (p "R") " {" n>
   q n
   "}" >)
(r "return " q ";")

(i "if (" p ") {" n> q n "}" >)
(ei "else if (" p ") {" n q "}" >)
(e "else {" n> q n "}" >)
(ie "if (" p ") {" n> p n "} else {" > n> q n "}" >)

(wh "while (" p ") {" n> q n "}" >)

(p "std.debug.print(\"" q "\", .{});")

(for "for (" (p "SEQ") ") |" (p "VAR") "| {" n> q n "}" > )

(err "const " (p "NAME") " = error{" n>
     q n
     "}" >)

(sw "switch (" p ") {" n>
    (p "CASE") " => " q "," n
    "};" > )
