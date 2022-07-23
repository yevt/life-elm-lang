[Rules](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#Rules)


Precompiled version included in `./build` folder. To launch open `./build/index.html` in your browser. 


### How to run locally

#### Install elm-0.18
elm >= 0.19 won't compile

```
curl -L -o elm.tar.gz https://github.com/lydell/elm-old-binaries/releases/download/main/0.18.0-linux-x64.tar.gz
tar -xzf elm.tar.gz
chmod +x dist_binaries/*
mv dist_binaries/* /usr/local/bin/
```
Make sure Elm is working 
```
elm --help
```
Build
```
./build.sh
```

