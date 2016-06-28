#include "scanner.hpp"
#include "reader.hpp"
#include <string>
#include <iostream>

int main() {
    Reader reader(std::string("I am a Man, and I love vicky."));
    Scanner scanner(reader);
    TokenType token;
    int count = 0;
    do {
        count++;
        token = scanner.GetTokenType();
        std::cout << token << ":" << scanner.GetText() << std::endl;
        scanner.Forward();
    } while (token != EOF && count < 15);
    return 0;
}
