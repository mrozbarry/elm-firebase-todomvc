# TodoMVC in Elm - https://elm-firebase-todomvc.firebaseapp.com/

All of the Elm code lives in `Todo.elm` and relies on the [elm-lang/html][http://package.elm-lang.org/packages/elm-lang/html/latest] and [pairshaped/elm-firebase](https://github.com/pairshaped/elm-firebase) libraries.


## Build Instructions

Run the following command from the root of this project:

```bash
$ npm install -g yarn
$ yarn
$ yarn build
$ yarn start
```

### With your own database

You may have to create a new firebase project in your firebase console rather than with firebase-tools.

```bash
$ node_modules/firebase-tools/bin/firebase login
$ node_modules/firebase-tools/bin/firebase init
$ node_modules/firebase-tools/bin/firebase deploy
```

Then open [localhost:5000](http://localhost:5000) in your browser!
