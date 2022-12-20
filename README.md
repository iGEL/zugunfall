## Zugunfall

Dies ist der Mastodon-Bot, der [@zugunfall@botsin.space](https://botsin.space/@zugunfall) betreibt.
Er sucht die Berichte der [Bundesstelle für Eisenbahnunfalluntersuchung](https://www.eisenbahn-unfalluntersuchung.de) zusammen und publiziert sie auf Mastodon.
Er ist in [ClojureScript](https://clojurescript.org/) geschrieben.

**Bitte den Code verwenden, um zu Twitter zu posten!**

This is the Mastodon bot running [@zugunfall@botsin.space](https://botsin.space/@zugunfall).
It is collecting the reports of the German [Federal Authority for Railway Accident Investigation](https://www.eisenbahn-unfalluntersuchung.de) and publishing the results to Mastodon.
It is written in [ClojureScript](https://clojurescript.org/).

**Please do not use this code to post to Twitter!**

## Setup & running it

1. Install [Node.js](https://nodejs.org) & [yarn](https://yarnpkg.com/)
2. Create a mastodon application and obtain the access token following the instructions of
   [this guide](https://docs.joinmastodon.org/client/token/).
   Scopes should be `read write:statuses write:media`
3. Run `yarn build`
4. Run `INSTANCE_BASE_URI=<instance-base-uri> ACCESS_TOKEN=<access-token> node target/app.js`

## Configuration

The bot can be configured by environment variables:

* `INSTANCE_BASE_URI` This is the base URI of the instance including the protocol, eg. `https://botsin.space`
* `ACCESS_TOKEN` The access token for using the Mastodon API
* `VISIBILITY` The visibility of toots to publish, see [Mastodon API docs](https://docs.joinmastodon.org/methods/statuses/#form-data-parameters). Defaults to `unlisted`.
* `ENV` Unless this is set to `prod`, toots will actually not be published. Defaults to `dev`.

## License

Copyright 2022 Johannes Barre

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
