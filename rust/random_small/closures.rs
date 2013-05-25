fn multiplier(mult: int) -> ~fn(int) -> int {
  return |i| i * mult;
}

fn main() {
  let times_two = multiplier(2);
  io::println(times_two(3).to_str());
  let num = multiplier(4);
  io::println(fmt!("is this twenty? %d", num(5)));
}
