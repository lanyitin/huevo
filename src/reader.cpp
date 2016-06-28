#include "reader.hpp"
#include <string>
#include <stdexcept>
#include <cstdio>
Reader::Reader(std::string data): code(data), currentPosition(0), codeLength(data.length()){}

void Reader::Forward(int num) {
    if (num <= 0) {
        throw std::invalid_argument("the parameter of Reader::Forward should be greater than 0");
    }
    if (this->currentPosition >= this->codeLength) {
        throw "end of file";
    }
    this->currentPosition += num;
    if (this->currentPosition >= this->codeLength) {
        this->currentPosition = this->codeLength;
    }
}

void Reader::Forward() {
    this->Forward(1);
}

char Reader::Peek() {
    if (this->currentPosition >= this->codeLength) {
        return EOF;
    }
    return this->code.at(this->currentPosition);
}

void Reader::Backward(int num) {
    if (num <= 0) {
        throw std::invalid_argument("the parameter of Reader::Backward should be greater than 0");
    }
    this->currentPosition -= num;
    if (this->currentPosition < 0) {
        this->currentPosition = 0;
    }
}

void Reader::Backward() {
    this->Backward(1);
}
