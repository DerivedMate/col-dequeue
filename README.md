To run this mess, you'll need node with bs-platform (from npm). After that it's as simple as running 
``` 
npm i -D; npm run build; npm run run
``` 
to run the 'main'. Feel free to fiddle around in the last function `let () = ...`; just don't remove "()" from the list line, because it needs to be of type "unit" - `()`