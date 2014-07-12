DECLARE SUB MazeInit (p!(), q!(), xv!(), yv!(), xd!, yd!, xn!, yn!)
DECLARE SUB Maze1 (p!(), q!(), xv!(), yv!(), xd!, yd!, xn!, yn!, zzc!)
DECLARE SUB Maze2 (p!(), xv!(), yv!(), xd!, yd!, x!, y!, zzc!)
DECLARE SUB MazeSolve (q!(), xv!(), yv!(), xd!, yd!, xn!, yn!)
'Maze

SCREEN 1

yd = 14
xd = yd
yn = INT(199 \ yd)
xn = INT(319 \ xd)

DIM p(xn + 2, yn + 2), q(xn, yn)

xv(0) = 1
xv(1) = 0
xv(2) = -1
xv(3) = 0

yv(0) = 0
yv(1) = 1
yv(2) = 0
yv(3) = -1

WHILE INKEY$ = ""
  MazeInit p(), q(), xv(), yv(), xd, yd, xn, yn
WEND

END

SUB Maze1 (p(), q(), xv(), yv(), xd, yd, xn, yn, zzc)
  XMIN = 0
  XMAX = xn
  YMIN = 0
  YMAX = yn
  WHILE (XMIN < XMAX AND YMIN < YMAX)
    FOR xo = XMIN TO XMAX
      x = xo
      y = YMIN
      Maze2 p(), xv(), yv(), xd, yd, x, y, zzc
    NEXT xo
    YMIN = YMIN + 1
    FOR yo = YMIN TO YMAX
      y = yo
      x = XMAX
      Maze2 p(), xv(), yv(), xd, yd, x, y, zzc
    NEXT yo
    XMAX = XMAX - 1
    FOR xo = XMIN TO XMAX
      x = xo
      y = YMAX
      Maze2 p(), xv(), yv(), xd, yd, x, y, zzc
    NEXT xo
    YMAX = YMAX - 1
    FOR yo = YMIN TO YMAX
      y = yo
      x = XMIN
      Maze2 p(), xv(), yv(), xd, yd, x, y, zzc
    NEXT yo
    XMIN = XMIN + 1
    FOR ii = 1 TO 20
    NEXT ii
  WEND
END SUB

SUB Maze2 (p(), xv(), yv(), xd, yd, x, y, zzc)
  C = zzc

start:

  S = INT(RND(1) * 4)
  FOR n = S TO 3 + S: V = n MOD 4
    IF p(x + xv(V) + 1, y + yv(V) + 1) <> 1 THEN
      p(x + xv(V) + 1, y + yv(V) + 1) = 1
      LINE (x * xd, y * yd)-((x + xv(V)) * xd, (y + yv(V)) * yd), C, B
      x = x + xv(V): y = y + yv(V)
      GOTO start
    END IF
 NEXT
END SUB

SUB MazeInit (p(), q(), xv(), yv(), xd, yd, xn, yn)
  zzc = 1
  DEF SEG = &HF000
  a = PEEK(&HC05D)
  DEF SEG
  IF a = &H4C THEN zzc = 3

  RANDOMIZE TIMER

  FOR x = 0 TO xn + 2
    p(x, 0) = 1
    p(x, 1) = 1
    p(x, yn + 1) = 1
    p(x, yn + 2) = 1
  NEXT x
  FOR y = y TO yn + 2
    p(0, y) = 1
    p(1, y) = 1
    p(xn + 1, y) = 1
    p(xn + 2, y) = 1
  NEXT y
  FOR x = 2 TO xn
    FOR y = 2 TO yn
      p(x, y) = 0
    NEXT y
  NEXT x
  FOR x = 0 TO xn
    FOR y = 0 TO yn
      q(x, y) = 0
    NEXT y
  NEXT x
  CLS
  LINE (xd * 0, yd * 0)-(xd * xn, yd * 0), zzc, B
  LINE -(xd * xn, yd * (yn - 1)), zzc, B
  LINE (xd * 0, yd * 1)-(xd * 0, yd * yn), zzc, B
  LINE -(xd * xn, yd * yn), zzc, B
  Maze1 p(), q(), xv(), yv(), xd, yd, xn, yn, zzc
  MazeSolve q(), xv(), yv(), xd, yd, xn, yn
END SUB

SUB MazeSolve (q(), xv(), yv(), xd, yd, xn, yn)
  DIM xw(4)
  DIM yw(4)

  DIM a%(9)
  a%(0) = &H10
  a%(1) = &H8
  a%(2) = &HF003
  a%(3) = &HF003
  a%(4) = &HC000
  a%(5) = &HFF3F
  a%(6) = &HC000
  a%(7) = &H3003
  a%(8) = &HC0C
  a%(9) = &H330

  C = 2
  IF zzc = 3 THEN C = 3

  XB = xd / 2
  YB = yd / 2

  xw(0) = xd
  xw(1) = XB - 1
  xw(2) = 0
  xw(3) = XB - 1

  yw(0) = YB - 1
  yw(1) = yd
  yw(2) = YB - 1
  yw(3) = 0

  LINE (0, YB)-(XB, YB), C, , &H5555: PUT (4, 4), a%
  x = 0
  y = 0
  S = 0
  WHILE (x <> xn - 1 OR y <> yn - 1)
    q(x, y) = 1
    PUT (x * xd + 4, y * yd + 4), a%
    PRESET (x * xd + XB, y * yd + YB)
    FOR n = S + 3 TO S + 6
    V = n MOD 4
      IF POINT((x * xd + xw(V)), (y * yd + yw(V))) <> 1 THEN
        x = x + xv(V): y = y + yv(V)
'        PRINT x, y
        IF q(x, y) THEN CL = 5 ELSE CL = C
        LINE -(x * xd + XB, y * yd + YB), CL, , &H5555
        PUT (x * xd + 4, y * yd + 4), a%
        S = V
        IF q(x, y) = 0 THEN FOR ZZ = 1 TO 1000: NEXT
        EXIT FOR
      END IF
    NEXT n
    FOR OKR = 1 TO 10
      '
    NEXT OKR
  WEND
  FOR J = 1 TO 4
    PUT (x * xd + 4, y * yd + 4), a%
    'PLAY "T120O3L64ABCDFGGABO4C"
    FOR OKR = 1 TO 200
      '
    NEXT OKR
  NEXT J
END SUB

