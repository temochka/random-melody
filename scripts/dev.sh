#!/bin/sh

elm-live "src/Main.elm" --dir="public" --pushstate --hot -o -- --output="public/dist/elm.js"
