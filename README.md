## Zugunfall

🇩🇪  
Dies ist der Mastodon-Bot, der [@zugunfall@botsin.space](https://botsin.space/@zugunfall) betreibt.
Er sucht die Berichte der [Bundesstelle für Eisenbahnunfalluntersuchung](https://www.eisenbahn-unfalluntersuchung.de) zusammen und publiziert sie auf Mastodon.
Er ist in [ClojureScript](https://clojurescript.org/) geschrieben.

Der Bot verwendet
* [Screen-Scraping](https://de.wikipedia.org/wiki/Screen_Scraping), um die Berichte zu finden
* Die Liste der bereits publizierten Berichte wird schlicht von Mastodon abgerufen
* Basisinformationen, Text-Inhalte und Screenshots der PDFs werden mit [Poppler](https://poppler.freedesktop.org/) ausgelesen/generiert.
* Die Verwendung von [Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) zieht sich durch den Code. Die Funktionen, die ein Promise zurückgeben, sind mit einem `+` am Ende benannt.

**Bitte nicht den Code verwenden, um zu Twitter zu posten!**

🇺🇸  
This is the Mastodon bot running [@zugunfall@botsin.space](https://botsin.space/@zugunfall).
It is collecting the reports of the German [Federal Authority for Railway Accident Investigation](https://www.eisenbahn-unfalluntersuchung.de) and publishing the results to Mastodon.
It is written in [ClojureScript](https://clojurescript.org/).

The bot uses
* [Screen scraping](https://en.wikipedia.org/wiki/Web_scraping) to find the reports
* The list of already published reports is simply fetched from Mastodon
* Basic info, text contents, and screenshots of the PDFs are read/generated by [Poppler](https://poppler.freedesktop.org/).
* [Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) are used all over the place. Functions returning a promise are named ending with a `+`.

**Please do not use this code to post to Twitter!**

## Setup

1. Install [docker](https://docs.docker.com/engine/install/)
2. Create the docker image `docker build -t zugunfall .`
4. Create the docker container
   `docker create -e INSTANCE_BASE_URI=<instance-base-uri> -e ACCESS_TOKEN=<access-token> -e VISIBILITY=<visibility> -e ENV=<env> -v `pwd`/reports:/zugunfall/reports --name zugunfall zugunfall`

## Running it

Run `docker start zugunfall`

## Configuration

The bot can be configured by environment variables:

* `INSTANCE_BASE_URI` This is the base URI of the instance including the protocol, eg. `https://botsin.space`
* `ACCESS_TOKEN` The access token for using the Mastodon API
* `VISIBILITY` The visibility of toots to publish, see [Mastodon API docs](https://docs.joinmastodon.org/methods/statuses/#form-data-parameters). Defaults to `unlisted`.
* `ENV` Unless this is set to `prod`, toots will actually not be published. Defaults to `dev`.

## Creating the API application & retrieving the access token

First, you need to create an API application. It's just a record in the database of the Mastodon instance:

```sh
curl -X POST \
        -F 'client_name=Zugunfall' \
        -F 'redirect_uris=urn:ietf:wg:oauth:2.0:oob' \
        -F 'scopes=read write:statuses write:media' \
        -F 'website=https://github.com/iGEL/zugunfall' \
        https://mastodon.example/api/v1/apps
```

You'll receive a `client_id` and a `client_secret`. Store them in a secure place.

Open `https://mastodon.example/oauth/authorize?client_id=CLIENT_ID&scope=read+write:statuses+write:media&redirect_uri=urn:ietf:wg:oauth:2.0:oob&response_type=code` in your browser.

Finally, you can get an access token with this:
```sh
curl -X POST \
	-F 'client_id=your_client_id_here' \
	-F 'client_secret=your_client_secret_here' \
	-F 'redirect_uri=urn:ietf:wg:oauth:2.0:oob' \
	-F 'grant_type=authorization_code' \
	-F 'code=user_authzcode_here' \
	-F 'scope=read write:statuses write:media' \
	https://mastodon.example/oauth/token
```

Store the `access_token` in the secure place. You also need it to run the bot.

## License

Copyright 2022-24 Johannes Barre

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
