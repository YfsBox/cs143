#!/bin/bash

./lexer $1 > test/test_result
../../bin/.i686/lexer $1 > test/test_right