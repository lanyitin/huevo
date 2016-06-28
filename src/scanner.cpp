#include"scanner.hpp"
#include"cstring"
#include <iostream>
Scanner::Scanner(Reader& r): reader(r), state(StartState), buf(""), currentLine(0), col(0) {
    this->Forward();
}

void Scanner::Forward() {
    this->buf[0] = '\0';

    {
        char peek = this->reader.Peek();
        while (this->isNewLine(peek)) {
            // @Todo: able to handle different newline symbol for windows, linux, and macos
            this->currentLine++;
            this->col = 0;

            this->reader.Forward();
            peek = this->reader.Peek();
        }
        if (peek == EOF) {
            this->currentTokenType = EOF;
            return;
        }
        switch (this->state) {
            case StartState:
                char c = this->reader.Peek();
                if (this->isAlphabet(c)) {
                    this->scanWord();
                } else if (this->isSymbol(c)) {
                    this->scanSymbol();
                }
                break;
        }
    }
}

void Scanner::scanWord() {
    char c[2] = {'\0'};
    c[0] = this->reader.Peek();
    strcat(this->buf, c);
    while(true) {
        this->reader.Forward();
        this->col++;
        c[0] = this->reader.Peek();
        if (this->isAlphabet(c[0])) {
            strcat(this->buf, c);
        } else {
            if (strcmp(this->buf, "while") == 0) {
                this->currentTokenType = KEYWORD_WHILE;
            } else if (strcmp(this->buf, "IF") == 0) {
                this->currentTokenType = KEYWORD_IF;
            } else if (strcmp(this->buf, "ELSE") == 0) {
                this->currentTokenType = KEYWORD_ELSE;
            }
            this->currentTokenType = IDENTIFIER;
            return;
        }
    }
}

void Scanner::scanSymbol() {
    char c[2] = {'\0'};
    c[0] = this->reader.Peek();
    strcat(this->buf, c);
    this->reader.Forward();
    switch(c[0]) {
        case '{':
            this->currentTokenType = OPEN_BRACES;
            break;
        case '}':
            this->currentTokenType = CLOSE_BRACES;
            break;
        case '(':
            this->currentTokenType = OPEN_PARENTHESES;
            break;
        case ')':
            this->currentTokenType = CLOSE_PARENTHESES;
            break;
        case '<':
            this->currentTokenType = OPEN_CHEVRONS;
            break;
        case '>':
            this->currentTokenType = CLOSE_CHEVRONS;
            break;
        case '\'':
            this->currentTokenType = SINGLE_QOUTE;
            break;
        case '"':
            this->currentTokenType = DOUBLE_QOUTE;
            break;
        case ':':
            this->currentTokenType = COLUMN;
            break;
        case ';':
            this->currentTokenType = SEMI_COLUMN;
            break;
        case '.':
            this->currentTokenType = PERIOD;
            break;
        case ',':
            this->currentTokenType = COMMA;
            break;
    }
}

std::string Scanner::GetText() {
    return std::string(this->buf);
}

TokenType Scanner::GetTokenType() {
    return this->currentTokenType;
}

bool Scanner::isNewLine(char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ' ';
}

bool Scanner::isAlphabet(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

bool Scanner::isSymbol(char c) {
    return (c >= 33 && c <= 47) || (c >= 58 && c <= 64);
}
