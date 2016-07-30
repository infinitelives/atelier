# atelier

Atelier is an asset generation toolbox for the infinitelives clojurescipt game framework.

## Overview

Presently this can not be used to make any assets, but the base application structure is tested and you can see the present state of the app at the following link:

[Launch Atelier!](https://infinitelives.github.io/atelier/)

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL.

## Devcards

To see the devcards run:

	lein figwheel devcards

Then point your browser at [localhost:3449/cards.html](http://localhost:3449/cards.html).

## License

This software is Copyright © 2016 Crispin Wellington

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
