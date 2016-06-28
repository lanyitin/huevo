#ifndef COMMON_HPP
#define COMMON_HPP
enum TokenType { 
    EOF = -1,
    OPEN_PARENTHESES,       // (
    CLOSE_PARENTHESES,      // )
    OPEN_BRACES,            // {
    CLOSE_BRACES,           // }
    OPEN_CHEVRONS,          // <
    CLOSE_CHEVRONS,         // >
    PLUS,                   // +
    MINUS,                  // -
    EQUAL,                  // =
    ASTERISk,               // *
    AMPERSAND,              // &
    AT,                     // @
    HASH,                   // #
    PERCENT,                // %
    UNDERSCORE,             // _
    VERTICALBAR,            // |
    SLASH,                  // /
    BACKSLACH,              // 
    DARLAR,                 // $
    EXCLAMATION,            // !
    PERIOD,                 // .
    COMMA,                  // ,
    SEMI_COLUMN,            // ;
    COLUMN,                 // :
    SINGLE_QOUTE,           // '
    DOUBLE_QOUTE,           // "

    BOOLEAN_EQUAL,          // ==
    BOOLEAN_NOT_EQUAL,      // !=
    BOOLEAN_GREATER_EQUAL,  // >=
    BOOLEAN_LESS_EQUAL,     // <=

    KEYWORD_WHILE,          // while
    KEYWORD_IF,             // if
    KEYWORD_ELSE,           // else

    IDENTIFIER,
    SPACE,
    NEW_LINE,
    LINE_COMMENT,
    BLOCK_COMMENT,
    EOL
};
#endif
