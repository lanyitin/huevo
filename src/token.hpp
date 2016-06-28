#ifndef TOKEN_HPP
#define TOKEN_HPP
#include "common.hpp"
struct Token {
    TokenType type;
    char text[];
};
#endif
