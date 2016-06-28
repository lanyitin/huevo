#ifndef SCANNER_HPP
#define SCANNER_HPP
#include "common.hpp"
#include "reader.hpp"
#include <string>

enum ScannerState {
    StartState
};
class Scanner {
    public:
        Scanner(Reader&);
        void Forward();
        void Backward();
        TokenType GetTokenType();
        std::string GetText();
    private:
        Reader& reader;
        ScannerState state;

        char buf[255];
        TokenType currentTokenType;
        int currentLine;
        int col;

        void scanWord();
        void scanSymbol();


        bool isNewLine(char);
        bool isAlphabet(char);
        bool isSymbol(char);
};

#endif
