import enum


class TokenType(enum.Enum):
    Number = 0,
    Operator = 1,
    Id = 2,
    LParen = 3,
    RParen = 4,
    Comma = 5,
    Invalid = 6


def transTable(c, st):
    if st == 0:
        if '0' <= c <= '9':
            return 1
        if c == '+' or c == '*' or c == '/' or c == '-' or c == '^':
            return 6
        if ('a' <= c <= 'z') or c == '_':
            return 7
        if c == '(':
            return 8
        if c == ')':
            return 9
        if c == ',':
            return 10
        if c == ' ' or c == '\t' or c == '\n' or c == '\r':
            return 0
    if st == 1:
        if '0' <= c <= '9':
            return 1
        if c == '.':
            return 2
        if c == 'e' or c == 'E':
            return 3
    if st == 2:
        if '0' <= c <= '9':
            return 2
        if c == 'e' or c == 'E':
            return 3
    if st == 3:
        if '0' <= c <= '9':
            return 5
        if c == '+' and c == '-':
            return 4
    if st == 4:
        if '0' <= c <= '9':
            return 5
    if st == 5:
        if '0' <= c <= '9':
            return 5
    if st == 7:
        if ('a' <= c <= 'z') or ('0' <= c <= '9') or c == '_':
            return 7
    return -1


class Lexer:
    def __init__(self, my_input):
        self.input = my_input
        self.curChIx = 0
        self.curSt = 0
    def getNextToken(self):
        lastAccChIx = self.curChIx
        self.curSt = 0
        resultToken = TokenType.Invalid
        res = list()
        buf =""
        curCh = self.input[self.curChIx]
        self.curChIx += 1
        self.curSt = transTable(curCh, self.curSt)
        if self.curSt > 0:
            buf += curCh
        while self.curSt != -1:
            if self.curSt == 1 or self.curSt == 2 or self.curSt == 5:
                resultToken.type = TokenType.Number
                resultToken.attribute = buf
                lastAccChIx = self.curChIx
            if self.curSt == 6:
                resultToken.type = TokenType.Operator
                resultToken.attribute = buf
                lastAccChIx = self.curChIx
            if self.curSt == 7:
                resultToken.type = TokenType.Id
                resultToken.attribute = buf
                lastAccChIx = self.curChIx
            if self.curSt == 8:
                resultToken.type = TokenType.LParen
                resultToken.attribute = buf
                lastAccChIx = self.curChIx
            if self.curSt == 9:
                resultToken.type = TokenType.RParen
                resultToken.attribute = buf
                lastAccChIx = self.curChIx
            if self.curSt == 10:
                resultToken.type = TokenType.Comma
                resultToken.attribute = buf
                lastAccChIx = self.curChIx
            if self.curChIx >= len(self.input):
                break
            curCh = self.input[self.curChIx]
            self.curChIx += 1
            self.curSt = transTable(curCh, self.curSt)
            if self.curSt > 0:
                buf += curCh
        self.curChIx = lastAccChIx
        return resultToken


if __name__ == '__main__':

    lx = Lexer('6.6722e2')
    print(lx.getNextToken().type)