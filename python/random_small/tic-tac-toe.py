# simple unoptimized tic-tac-toe game
# computer plays with minimax
# written to illustrate minimax to my brother

from copy import deepcopy
PIECE = ['X', 'O']
EMPTY_PIECE = ' '

class Board:
  def __init__(self, size):
    self.size = size
    self.board = [[EMPTY_PIECE for _ in range(size)] for _ in range(size)]

  def draw(self):
    for row in self.board:
      if any(x == EMPTY_PIECE for x in row):
        return False
    return True

  def game_over(self):
    return self.draw() or self.victory(PIECE[0]) or self.victory(PIECE[1])

  def victory(self, piece):
    for row in range(self.size):
      if all(self.board[row][i] == piece for i in range(self.size)):
        return True

    for col in range(self.size):
      if all(self.board[i][col] == piece for i in range(self.size)):
        return True

    if all(self.board[i][i] == piece for i in range(self.size)):
      return True

    if all(self.board[i][self.size-i-1] == piece for i in range(self.size)):
      return True

  def __str__(self):
    return '\n'.join(''.join(row) for row in self.board)

  def make_move(self, (row,col), piece):
    if self.can_make_move((row, col)):
      self.board[row][col] = piece
    else:
      raise RuntimeError("Invalid move!")

  def undo_move(self, (row,col)):
    self.board[row][col] = EMPTY_PIECE

  def can_make_move(self, (row,col)):
    return 0 <= row < self.size and 0 <= col < self.size and self.board[row][col] == EMPTY_PIECE

def speculatively_move(move, piece, board):
  new_board = deepcopy(board)
  new_board.make_move(move, piece)
  return new_board

def play_game(get_move, size):
  num_players = len(get_move)
  board = Board(size)
  player = 0
  while not board.game_over():
    piece = PIECE[player]
    print("It's '" + piece + "'s move")
    print(board)
    move = get_move[player](piece, board)
    board.make_move(move, piece)
    player = (player + 1) % num_players
    print("----------------")

  print(board)
  if board.draw():
    print("Draw!")
  else:
    for piece in PIECE:
      if board.victory(piece):
        print(piece + " wins!")


def prompt_move(piece, board):
  while True:
    try:
      row = int(raw_input("row: "))
      col = int(raw_input("col: "))
      if board.can_make_move((row, col)):
        return (row, col)
      print("Invalid move")
    except ValueError:
      print("Please enter an integer.")


def play_human_game():
  play_game([prompt_move] * 2, 3)

def play_computer_game():
  play_game([prompt_move, computer_move], 3)


def score_game(piece, board):
  assert(board.game_over())
  if board.draw():
    return 0
  elif board.victory(piece):
    return 1
  else:
    return -1

def all_possible_moves(board):
  all_moves = [(r,c) for r in range(board.size) for c in range(board.size)]
  return filter(lambda m: board.can_make_move(m), all_moves)


# returns a 2-tuple (best score, best move)
def evaluate_board(piece, board):
  if board.game_over():
    return (score_game(piece, board), None)

  moves = all_possible_moves(board)
  next_piece = PIECE[(PIECE.index(piece) + 1) % len(PIECE)]

  best_score = float("-inf")
  best_move = None
  for move in moves:
    new_board = speculatively_move(move, piece, board)
    score, _ = evaluate_board(next_piece, new_board)
    score *= -1 # The value to us is the inverse of the value to the other piece
    if score > best_score:
      best_score = score
      best_move = move
  return (best_score, best_move)

def computer_move(piece, board):
  _, move = evaluate_board(piece, board)
  return move

if __name__ == '__main__':
  play_computer_game()
