# coding=utf-8
import re
import sys

class Token(object):
    def __init__(self, type, value, position):
        self.type = type
        self.value = value
        self.position = position

    def __str__(self):
        return '%s(%s) позиция %s' % (self.type, self.value, self.position)

class Mistakes(Exception):
    def __init__(self, position):
        self.position = position

class Lexer(object):
    def __init__(self, rules, skip_whitespace=True):
        idx = 1
        regex_parts = []
        self.group_type = {}

        for regex, type in rules:
            groupname = 'GROUP%s' % idx
            regex_parts.append('(?P<%s>%s)' % (groupname, regex))
            self.group_type[groupname] = type
            idx += 1

        self.regex = re.compile('|'.join(regex_parts))
        self.skip_whitespace = skip_whitespace
        self.re_ws_skip = re.compile('\S')

    def input(self, buffer):
        self.buffer = buffer
        self.position = 0

    def token(self):
        if self.position >= len(self.buffer):
            return None
        else:
            if self.skip_whitespace:
                m = self.re_ws_skip.search(self.buffer[self.position:])

                if m:
                    self.position += m.start()
                else:
                    return None

            m = self.regex.match(self.buffer[self.position:])
            if m:
                groupname = m.lastgroup
                tok_type = self.group_type[groupname]
                tok = Token(tok_type, m.group(groupname), self.position)
                self.position += m.end()
                return tok

            raise Mistakes(self.position)

    def tokens(self):
        while 1:
            tok = self.token()
            if tok is None: break
            yield tok

if __name__ == '__main__':
    rules = [
        ('(\d+)+(\.[0-9]*)?([eE][+-]?[0-9]+)?', 'number'),
        ('[a-z_][a-z_0-9]*', 'identifier'),
        ('[\+\-\*\/\^\=]', 'operator'),
        ('\(', 'lparen'),
        ('\)', 'rparen'),
        ('[\,\.]', 'comma or dot'),
    ]

    lx = Lexer(rules, skip_whitespace = True)
    lx.input('6.6722e2')

    try:
        for tok in lx.tokens():
            print (tok)
    except Mistakes as mistake:
        print ('Ошибка в ', mistake.position)