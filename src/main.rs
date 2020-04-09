use rust_lua::lexer;

fn main() {
    let src = r#"
local x = {}
local y = "hello world \nbye"
local z = -1.2e-10
a = [1, "hahaha", .3]
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
print(#x)
    "#
    .chars()
    .collect::<Vec<_>>();

    for t in lexer::tokenize(&src) {
        println!("{:?}", t.ty);
    }
}
