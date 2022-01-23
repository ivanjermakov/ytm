# ytm

## Install

### Required programs

- Stack
- [yq](https://github.com/kislyuk/yq) (xml parsing)
- curl

## Setup

### Obtaining `ACCESS_TOKEN`

You need to have `CLIENT_ID` and `CLIENT_SECRET` obtained before.

1. Request `CODE` with `CLIENT_ID`
    ```
    https://accounts.google.com/o/oauth2/v2/auth?client_id=CLIENT_ID&redirect_uri=urn:ietf:wg:oauth:2.0:oob&scope=https://www.googleapis.com/auth/youtube&response_type=code
    ```

2. Request `ACCESS_TOKEN` with `CODE`, `CLIENT_ID` and `CLIENT_SECRET`
    ```
    curl -s \
    --request POST \
    --data "code=CODE&client_id=CLIENT_ID&client_secret=CLIENT_SECRET&redirect_uri=urn:ietf:wg:oauth:2.0:oob&grant_type=authorization_code" \
    https://accounts.google.com/o/oauth2/token
    ```