Dette program benytter chess.fs, pieces.fs og chessApp.fsx.
Bibliotekerne chess.fs og pieces.fs oversættes således:
$ fsharpc -a chess.fs
$ fsharpc -a chess.fs pieces.fs
Testprogrammet oversættes og køres således:
$ fsharpc -r chess.fs -r pieces.fs chessApp.fsx
$ mono chessApp.exe