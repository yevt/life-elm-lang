#!/bin/bash
elm-make ./main.elm --output build/life.js --debug
cp index.html build/index.html