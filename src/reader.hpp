#ifndef READER_HPP
#define READER_HPP
#include <string>
class Reader {
    public:
        Reader(std::string);
        char Peek();
        void Forward(int);
        void Backward(int);
        void Forward();
        void Backward();
    private:
        std::string code;
        int currentPosition;

        int codeLength; // tmp variable

};
#endif
