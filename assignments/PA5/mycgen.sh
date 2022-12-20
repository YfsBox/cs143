#!/bin/bash

./lexer $1 | ./parser | ./semant | ./cgen
