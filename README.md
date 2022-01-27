# ytm

YouTube video manager TUI

## Setup

### Required programs

- Stack
- curl
- [yt-dlp](https://github.com/yt-dlp/yt-dlp)

### Running

1. `git clone https://github.com/ivanjermakov/ytm`
2. Create file `~/.config/ytm/.env`:

    ```
    API_KEY=
    CLIENT_ID=
    CLIENT_SECRET=
    REFRESH_TOKEN=
    ```

4. `stack build`

### Obtaining API credentials

#### Obtaining `API_KEY`, `CLIENT_ID`, `CLIENT_SECRET`

You have to create a project in a [Google Cloud Console](https://console.cloud.google.com/). More
details [here](https://developers.google.com/youtube/registering_an_application).

#### Obtaining `REFRESH_TOKEN`

You need to have `CLIENT_ID` and `CLIENT_SECRET` obtained before.

1. Request `CODE` with `CLIENT_ID`
    ```
    https://accounts.google.com/o/oauth2/v2/auth?client_id=CLIENT_ID&redirect_uri=urn:ietf:wg:oauth:2.0:oob&scope=https://www.googleapis.com/auth/youtube&response_type=code
    ```

2. Request `REFRESH_TOKEN` with `CODE`, `CLIENT_ID` and `CLIENT_SECRET`
    ```
    curl -s \
    --request POST \
    --data "code=CODE&client_id=CLIENT_ID&client_secret=CLIENT_SECRET&redirect_uri=urn:ietf:wg:oauth:2.0:oob&grant_type=authorization_code" \
    https://accounts.google.com/o/oauth2/token
    ```
