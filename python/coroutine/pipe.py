def coroutine(func):
  def start(*args, **kwargs):
    cr = func(*args, **kwargs)
    cr.next()
    return cr
  return start

def source_user(target):
  while True:
    line = raw_input(": ")
    if not line:
      break
    target.send(line)

@coroutine
def sink_print():
  while True:
    print(yield)

def make_grep(pattern):
  @coroutine
  def grep(target):
    while True:
      line = yield
      if pattern in line:
        target.send(line)
  return grep

def pipe(*args):
  built = None
  for f in reversed(args):
    if built is None:
      built = f()
    else:
      built = f(built)
  return built
