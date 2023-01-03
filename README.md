# EnlightnU Quote

This is the front end site. Written in elm, compiled to javascript. Uses a barebones simple CSS.

Elm works great for handling input logic and such. My formatting of the HTML in Elm works but was far less elegant.

Connect to python REST api hosted at heroku

TODO: need to have NAIC codes for preferred providers in a simple csv or such, then parse that in Elm somehow.

To update Part B:

edit line 215 of src/Main.elm

To add new companies:
edit src/Preset.elm. Need the NAIC code(s), and you have to add to both the displayNames and
naicCategory sections.
https://content.naic.org/cis_consumer_information.htm

After making any changes to the *.elm files, you have to recompile and push to github for it to take effect. The public/index.html file is looking for `elm.min.js` in the same folder. To do generate this, compile then minifiy as described here:
https://guide.elm-lang.org/optimization/asset_size.html

Video walkthrough: https://www.loom.com/share/ce8e081a28d445d2b1e64ef806a16e6b



