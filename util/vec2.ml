
module Vec2 = struct
  type t = { x: int; y: int }
  let add (a: t) (b: t): t = { x = a.x + b.x; y = a.y + b.y }
  let sub (a: t) (b: t): t = { x = a.x - b.x; y = a.y - b.y }
  let mul (a: t) (k: int): t = { x = a.x * k; y = a.y * k }
end

let get (grid: 'a array array) (pos: Vec2.t): 'a =
  grid.(pos.x).(pos.y)
