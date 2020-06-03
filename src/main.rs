use rust_lua::{interpreter, lexer, parser};

fn main() {
    /*let src = r#"local x = {}
local y = "hello world \nbye"
local z = -1.2e-10
a = {b=1, ['c']="hahaha", .3}
b = 'a' .. "b"
x.p = 1 + 2 - 3 * 7 / 2 ^ 10 % 2
function x:q(a, ...) return 10 end
while true do break end
repeat print(y); break until false
for k,v in pairs(x) do print(k) end
if z ~= nil then
    print(b)
elseif z >= 1 and z <= 2 then
    print("inbetween")
elseif z == 0 or z > 9.1e10 or z < -9.1e10 then
    print ("huh")
else
    print("well")
end
-- comment
print(#x)"#*/
    let src = r#"a = 1/3
local b = '0.1'
return a - b, c"#
        .chars()
        .collect::<Vec<_>>();

    let tokens = lexer::tokenize(&src);
    let ast = parser::parse(tokens);
    print!("{}", ast.iter().map(|stmt| stmt.format()).collect::<Vec<_>>().join("\n"));
    let value = interpreter::interpret(ast, vec![]).unwrap();
    println!("==> {}", value);
}
