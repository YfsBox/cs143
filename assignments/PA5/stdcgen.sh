#! /bin/bash

../../bin/.i686/lexer $1 | ../../bin/.i686/parser | ../../bin/.i686/semant | ../../bin/.i686/cgen