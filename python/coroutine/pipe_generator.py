def source_user():
  while True:
    line = raw_input(": ")
    if not line:
      break
    yield line

def sink_print(link):
  for line in link:
    print line

def make_grep(pattern):
  def grep(link):
    for line in link:
      if pattern in line:
        yield line
  return grep

def pipe(*args):
  built = None
  for f in args:
    if built is None:
      built = f()
    else:
      built = f(built)
  return built
